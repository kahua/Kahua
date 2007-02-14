;; package maintainance shell script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-package.scm,v 1.9.2.3 2007/02/04 07:44:43 bizenn Exp $
(use srfi-13)

(use file.util)
(use gauche.parseopt)
(use util.match)

(use kahua.config)

;;
;; generate a skelton application
;;
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
	  (cons #`",|skel|/plugins" "plugins")
	  (cons #`",|skel|/templates" "templates")))
  (define (gen-src&dst-files proj)
    (list (cons #`",|skel|/proj/proj.css" #`",|proj|/,|proj|.css")
	  (cons #`",|skel|/proj/proj.kahua" #`",|proj|/,|proj|.kahua")
	  (cons #`",|skel|/proj/version.kahua.in" #`",|proj|/version.kahua.in")
	  (cons #`",|skel|/test/test.conf.in" "test/test.conf.in")
	  (cons #`",|skel|/test/test.scm.in" "test/test.scm.in")
	  (cons #`",|skel|/plugins/proj.scm" #`"plugins/,|proj|.scm")
	  (cons #`",|skel|/templates/page.xml" #`"templates/page.xml")
	  (cons #`",|skel|/AUTHORS" "AUTHORS")
	  (cons #`",|skel|/ChangeLog" "ChangeLog")
	  (cons #`",|skel|/DIST" "DIST")
	  (cons #`",|skel|/DIST_EXCLUDE" "DIST_EXCLUDE")
	  (cons #`",|skel|/INSTALL" "INSTALL")
	  (cons #`",|skel|/Makefile.in" "Makefile.in")
	  (cons #`",|skel|/README" "README")
	  (cons #`",|skel|/MESSAGES" "MESSAGES")
	  (cons #`",|skel|/app-servers" "app-servers")
	  (cons #`",|skel|/configure.ac" "configure.ac")
	  (cons #`",|skel|/install-sh" "install-sh")
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
			    (lp (read-line in))))))
	    :encoding 'euc-jp))		; FIXME!!
	:encoding 'euc-jp))		; FIXME!!
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
  (define (replace-proj line seed)
    (regexp-fold
     #/%%_PROJECT_NAME_%%/
     replace-creator
     (lambda (match seed)
       (string-append seed proj))
     seed line))
  (replace-proj str ""))

(define (generate-skel args)
  (let-args args ((creator "creator=s")
		  (mail "mail=s")
		  . projects)
    (let1 skel (build-path (kahua-etc-directory) "skel") ; FIXME!!
      (for-each (cut generate skel <> creator mail) projects))))

;;
;; create site bundle
;;

(define (create-site args)
  (let-args args ((shared "shared")
		  (private "private")
		  (owner "o|owner=s")
		  (group "g|group=s")
		  . sites)
    (for-each (cut kahua-site-create <> :owner owner :group group :shared? shared) sites)))

;;
;; main
;;

(define *command-table*
  `(("create" ,create-site "create [-shared|-private] [-owner=<owner>] [-group=<group>] <site-to-path>")
    ("generate" ,generate-skel "generate [-creator=<creator>] [-mail=<mail@addr>] <project> ...")))

(define (dispatch-command cmd . args)
  (cond ((assoc cmd *command-table*) => (lambda (e) ((cadr e) args)))
	(else (usage))))

(define (main args)
  (let-args (cdr args)
      ((conf-file "c|conf-file=s")
       (site "S|site=s")
       (gosh "gosh=s")
       . restargs)
    (if (< (length restargs) 2)
	(usage)
	(apply dispatch-command restargs))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (display "Usage: kahua-package <command> [<options>] <args>...\n")
      (display "Commans:\n")
      (for-each (lambda (e) (format #t "  ~a\n" (list-ref e 2))) *command-table*)))
  (exit 1))

