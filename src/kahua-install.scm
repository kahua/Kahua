;; Utility script to install kahua-related materials
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id$

;; Installs Kahua's application server materials according to
;; the kahua.conf configuration settings.

(use kahua.config)
(use gauche.parseopt)
(use file.util)

(define (usage)
  (display
   "Usage: kahua-install [<options>] file ...
options:
  -c conf-file     specify configuration file.
  -S path-to-site  specify path to a site bundle.
  -U               uninstall files instead of installing.
  -t type          specify install type:
                   script|static|base|plugin|template.  static by default.
  -r rename        rename file to name.  When this option is given,
                   only one file can be specified.
  --no-overwrite   if the target file exists, the file won't be installed.
"
   (current-error-port))
  (exit 1))

(define (main args)
  (let-args (cdr args)
      ((site "S=s")
       (conf-file "c=s")
       (material-type "t=s" "static")
       (rename    "r=s")
       (uninstall? "U")
       (dirs?     "dirs")
       (no-over   "no-overwrite")
       (gosh      "gosh=s")  ;; wrapper script adds this.  ignore.
       . files)
    (unless (member material-type
                    '("script" "static" "base" "plugin" "template")) (usage))
    (when (and rename (not (= (length files) 1))) (usage))
    (kahua-common-init site conf-file)
    (if dirs?
      (install-dirs uninstall?)
      (if uninstall?
        (for-each (cut uninstall-file <> material-type rename) files)
        (for-each (cut install-file <> material-type rename no-over) files))))
  0)

(define (install-file file material-type rename no-over)
  (let* ((target     (target-path material-type (or rename file)))
         (target-dir (sys-dirname target)))
    (cond
     ((not (file-exists? file))
      (warn "file ~a doesn't exist.  skipping installation."  file))
     ((not (file-is-regular? file))
      (warn "file ~a isn't a regular file.  skipping installation."  file))
     ((and no-over (file-exists? target))
      (warn "target file ~a exists, and not to be overwritten." target))
     (else
      (make-directory* target-dir #o755)
      (copy-file file target :if-exists :supersede :safe #t)
      (sys-chmod target #o444)))))

(define (uninstall-file file material-type rename)
  (let* ((target     (target-path material-type (or rename file)))
         (target-dir (sys-dirname target)))
    (when (file-exists? target)
      (guard (e (else (warn "couldn't uninstall ~a: ~a" target (ref e 'message))))
	(sys-unlink target)))
    (when (and (file-is-directory? target-dir)
               (equal? (sys-readdir target-dir) '("." "..")))
      (guard (e (else (warn "couldn't remove the directory ~a: ~a"
			    target-dir (ref e 'message))))
	(sys-rmdir target-dir)))))

;; NB: uninstalling dirs isn't implemented yet, since it can be
;; pretty dangerous operation.
(define (install-dirs uninstall?)
  (let* ((sockaddr (supervisor-sockaddr (kahua-sockbase)))
         (dirs (list* (kahua-working-directory)
                      (kahua-static-document-path "")
                      (if (is-a? sockaddr <sockaddr-un>)
                        (list (sys-dirname (sockaddr-name sockaddr)))
                        '()))))
    (dolist (dir dirs)
      (make-directory* dir))))

(define (target-path material-type file)
  (cond ((equal? material-type "script")
         (build-path (kahua-application-directory) file))
        ((equal? material-type "base")
         (build-path (kahua-working-directory) file))
        ((equal? material-type "plugin")
         (build-path (kahua-plugin-directory) file))
        ((equal? material-type "static")
         (kahua-static-document-path file))
	((equal? material-type "template")
	 (build-path (kahua-template-directory) file))
        (else
         (error "unknown material type:" material-type))))

;; Local variables:
;; mode: scheme
;; end:
