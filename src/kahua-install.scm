;; Utility script to install kahua-related materials
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-install.scm,v 1.4 2006/10/08 06:00:12 bizenn Exp $

;; Installs Kahua's application server materials according to
;; the kahua.conf configuration settings.

(use kahua.config)
(use gauche.parseopt)
(use file.util)

(define (usage)
  (print "kahua-install [-c conf-file][-U][-t type][-r name][--no-overwrite] file ...")
  (print "  Install files under kahua-managed directories.")
  (print "   -U : uninstall files instead of installing.")
  (print "   -t type: script | static | base | plugin.  static by default.")
  (print "   -r (rename): rename file to name.  When this option is given,")
  (print "      only one file can be specified.")
  (print "   --no-overwrite : if the target file exists, the file won't be")
  (print "         installed.")
  (print)
  (print "kahua-install [-c conf-file][-U] --dirs")
  (print "  Creates (or removes) kahua-managed directories.")
  (exit 0))

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
                    '("script" "static" "base" "plugin")) (usage))
    (when (and rename (not (= (length files) 1))) (usage))
    (kahua-common-init site conf-file #f)
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
      (with-error-handler
          (lambda (e)
            (warn "couldn't uninstall ~a: ~a" target (ref e 'message)))
        (lambda ()
          (sys-unlink target))))
    (when (and (file-is-directory? target-dir)
               (equal? (sys-readdir target-dir) '("." "..")))
      (with-error-handler
          (lambda (e)
            (warn "couldn't remove the directory ~a: ~a"
                  target-dir (ref e 'message)))
        (lambda ()
          (sys-rmdir target-dir))))))

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
        (else
         (error "unknown material type:" material-type))))

;; Local variables:
;; mode: scheme
;; end:
