;;;
;;; Generate test environment
;;;
;;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;;  See COPYING for terms and conditions of using this software
;;;
;;;  $Id: make-testconf.scm,v 1.2 2004/10/19 02:37:34 shiro Exp $

;; Usage:
;;
;;    % gosh ./make-testconf.scm $(top_builddir) $(top_srcdir)
;;
;;    - generates $(top_builddir)/src/test.conf
;;    - creates $(top_builddir)/tmp/checkout and symlinks
;;

(use file.util)
(use util.match)

(define (main args)
  (match (cdr args)
    ((top-builddir top-srcdir)
     (let ((builddir (absolutify top-builddir))
           (srcdir   (absolutify top-srcdir)))
       (create-tmphier srcdir builddir)
       (generate builddir)
       ))
    (else
     (error "Usage: gosh ./make-testconf.scm $top_builddir $top_srcdir")))
  0)

(define (generate builddir)
  (with-output-to-file "test.conf"
    (lambda ()
      (let* ((testtemp (build-path builddir "tmp"))
             (sockbase #`"unix:,|testtemp|")
             (docpath  (build-path testtemp "checkout"))
             (userconf (build-path testtemp "user.conf")))
        (print ";; configuration file used for testing")
        (print ";; generated automatically at make time")
        (print "(make <kahua-config>")
        (format #t "  :sockbase ~s\n" sockbase)
        (format #t "  :working-directory ~s\n" testtemp)
        (format #t "  :static-document-path ~s\n" docpath)
        (format #t "  :static-document-url  \"/doc\"\n")
        (format #t "  :userconf-file ~s\n" userconf)
        (print ")")))))

(define (create-tmphier srcdir builddir)
  (let* ((tmpdir (build-path builddir "tmp"))
         (codir  (build-path tmpdir "checkout"))
         (exdir  (build-path srcdir "examples")))
    (when (file-exists? tmpdir)
      (remove-directory* tmpdir))
    (make-directory* tmpdir)
    (sys-symlink exdir codir)
    (sys-symlink (build-path exdir "app-servers.sample")
                 (build-path tmpdir "app-servers"))
    (sys-symlink (build-path srcdir "plugins")
                 (build-path tmpdir "plugins"))
    ))

(define (absolutify path)
  (if (relative-path? path)
    (simplify-path (build-path (current-directory) path))
    path))

