;;;
;;; Generate test.conf file
;;;
;;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;;  See COPYING for terms and conditions of using this software
;;;
;;;  $Id: make-testconf.scm,v 1.1.2.1 2004/10/15 07:22:18 shiro Exp $

;; Usage:
;;
;;    % gosh ./make-testconf.scm $(top_builddir)
;;

(use file.util)
(use util.match)

(define (main args)
  (match (cdr args)
    ((top-builddir)
     (generate (if (relative-path? top-builddir)
                 (simplify-path (build-path (current-directory) top-builddir))
                 top-builddir)))
    (else
     (error "Usage: gosh ./make-testconf.scm $top_builddir")))
  0)

(define (generate builddir)
  (with-output-to-file "test.conf"
    (lambda ()
      (let* ((testtemp (build-path builddir "tmp"))
             (sockbase #`"unix:,|testtemp|")
             (docpath  (build-path testtemp "doc"))
             (userconf (build-path testtemp "user.conf")))
        (print ";; configuration file used for testing")
        (print ";; generated automatically at make time")
        (print "(make <kahua-config>")
        (format #t "  :sockbase ~s\n" sockbase)
        (format #t "  :working-directory ~s\n" testtemp)
        (format #t "  :static-document-path ~s\n" docpath)
        (format #t "  :static-document-url  \"doc\"\n")
        (format #t "  :userconf-file ~s\n" userconf)
        (print ")")))))
      
