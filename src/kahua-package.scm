;; package maintainance shell script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-package.scm,v 1.1.2.7 2006/07/22 15:26:18 cut-sea Exp $
(use srfi-13)

(use file.util)
(use gauche.parseopt)
(use util.match)

(use kahua.config)

;
; generate
;
(define (generate-getter prmt rexp)
  (lambda ()
    (let lp ((dat #f))
      (cond ((and dat (rexp dat)) dat)
	    (else (display prmt)
		  (flush)
		  (lp (read-line)))))))

(define (generate skel proj creator mail)
  (define (gen-src&dst-directory proj)
    (list (cons #`",|skel|/proj" proj)
	  (cons #`",|skel|/test" "test")
	  (cons #`",|skel|/plugins" "plugins")))
  (define (gen-src&dst-files proj)
    (list (cons #`",|skel|/proj/proj.css" #`",|proj|/,|proj|.css")
	  (cons #`",|skel|/proj/proj.kahua" #`",|proj|/,|proj|.kahua")
	  (cons #`",|skel|/proj/base.kahua" #`",|proj|/base.kahua")
	  (cons #`",|skel|/proj/default.kahua" #`",|proj|/default.kahua")
	  (cons #`",|skel|/proj/page.kahua" #`",|proj|/page.kahua")
	  (cons #`",|skel|/proj/pcont-page.kahua" #`",|proj|/pcont-page.kahua")
	  (cons #`",|skel|/proj/session.kahua" #`",|proj|/session.kahua")
	  (cons #`",|skel|/proj/utility.kahua" #`",|proj|/utility.kahua")
	  (cons #`",|skel|/proj/validator.kahua" #`",|proj|/validator.kahua")
	  (cons #`",|skel|/proj/version.kahua.in" #`",|proj|/version.kahua.in")
	  (cons #`",|skel|/test/test.scm.in" "test/test.scm.in")
	  (cons #`",|skel|/plugins/proj.scm" #`"plugins/,|proj|.scm")
	  (cons #`",|skel|/AUTHORS" "AUTHORS")
	  (cons #`",|skel|/ChangeLog" "ChangeLog")
	  (cons #`",|skel|/DIST" "DIST")
	  (cons #`",|skel|/DIST_EXCLUDE" "DIST_EXCLUDE")
	  (cons #`",|skel|/INSTALL" "INSTALL")
	  (cons #`",|skel|/Makefile.in" "Makefile.in")
	  (cons #`",|skel|/README" "README")
	  (cons #`",|skel|/app-servers" "app-servers")
	  (cons #`",|skel|/configure.ac" "configure.ac")
	  (cons #`",|skel|/install-sh" "install-sh")
	  (cons #`",|skel|/proj-start.in" #`",|proj|-start.in")
	  (cons #`",|skel|/proj-stop.in" #`",|proj|-stop.in")
	  (cons #`",|skel|/proj.conf.in" #`",|proj|.conf.in")
	  (cons #`",|skel|/COPYING" "COPYING")))
  (define get-project-name
    (generate-getter
     "Project Name> "
     #/[_a-zA-Z\-]+/))
  (define get-creator-name
    (generate-getter
     "Creator Name> "
     #/[_a-zA-Z\-]+/))
  (define get-mail-address
    (generate-getter
     "E-Mail Address> "
     #/^[\.a-zA-Z0-9_-]+\@[\.a-zA-Z0-9_-]+\.\w+$/))
  (let ((proj (or proj (get-project-name)))
	(creator (or creator (get-creator-name)))
	(mail (or mail (get-mail-address))))
    (define (copy&replace src dst)
      (call-with-input-file src
	(lambda (in)
	  (call-with-output-file dst
	    (lambda (out)
	      (let lp ((ln (read-line in)))
		(cond ((eof-object? ln) 'done)
		      (else (display (replace ln proj creator mail) out)
			    (newline out)
			    (lp (read-line in))))))))))
    (sys-mkdir proj #o755)
    (sys-chdir proj)
    (for-each (lambda (pair)
		(make-directory* (cdr pair)))
	      (gen-src&dst-directory proj))
    (for-each (lambda (pair)
		(copy&replace (car pair) (cdr pair)))
	      (gen-src&dst-files proj))
    (sys-chmod "DIST" #o755)))

(define (replace str proj creator mail)
  (define (regexp-fold rx proc-nomatch proc-match seed line)
    (let loop ((line line)
	       (seed seed))
      (cond ((string-null? line) seed)
	    ((rx line)
	     => (lambda (m)
		  (let ((pre   (m 'before))
			(post  (m 'after)))
		    (if (string-null? pre)
			(loop post (proc-match m seed))
			(loop post (proc-match m (proc-nomatch pre seed)))))))
	    (else
	     (proc-nomatch line seed)))))
  (define (replace-email line seed)
    (regexp-fold
     #/%%_EMAIL_ADDRESS_%%/
     (lambda (a b) (string-append b a))
     (lambda (match seed)
       (string-append seed mail))
     seed line))
  (define (replace-creator line seed)
    (regexp-fold
     #/%%_CREATOR_NAME_%%/
     replace-email
     (lambda (match seed)
       (string-append seed creator))
     seed line))
  (define (replace-proj-up line seed)
    (regexp-fold
     #/%%_PROJECT_NAME_UP_%%/
     replace-creator
     (lambda (match seed)
       (string-append seed (string-upcase proj)))
     seed line))
  (define (replace-proj line seed)
    (regexp-fold
     #/%%_PROJECT_NAME_%%/
     replace-proj-up
     (lambda (match seed)
       (string-append seed proj))
     seed line))
  (replace-proj str ""))

;
; main
;

(define (main args)
  (define (get-opts arg)
    (or (member "gen" arg)
	(member "generate" arg)
	))
  (let-args (cdr args)
    ((conf-file "c=s")
     (gosh      "gosh=s"))
    (let-args (cddddr args)
	((proj      "p=s")
	 (creator   "u=s")
	 (mail      "m=s"))
      (kahua-init conf-file)
      (let* ((conf (kahua-config))
	     (file (ref conf 'conf-file))
	     (skel (build-path (sys-dirname file) "skel")))
	(match (cdddr args)
	  (("gen" . _) (generate skel proj creator mail))
	  (("generate" . _) (generate skel proj creator mail))
	  (else (usage)))))))

(define (usage)
  (print "kahua-package generate [-p=project] [-c=creator] [-m=mail@addr]")
  (exit 0))

