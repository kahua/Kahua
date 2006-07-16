;; package maintainance shell script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-package.scm,v 1.1.2.1 2006/07/16 13:13:30 cut-sea Exp $
(use srfi-13)
(use gauche.parseopt)
(use util.match)

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


(define (generate proj creator mail)
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
    'copy&replace-names))

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
     #/##_EMAIL_ADDRESS_##/
     (lambda (a b) (string-append b a))
     (lambda (match seed)
       (string-append seed mail))
     seed line))
  (define (replace-creator line seed)
    (regexp-fold
     #/##_CREATOR_NAME_##/
     replace-email
     (lambda (match seed)
       (string-append seed creator))
     seed line))
  (define (replace-proj line seed)
    (regexp-fold
     #/##_PROJECT_NAME_##/
     replace-creator
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
  (let1 args (get-opts args)
    (let-args (cdr args)
	((conf-file "c=s")
	 (proj      "p=s")
	 (creator   "u=s")
	 (mail      "m=s")
	 (gosh      "gosh=s"))
      (match args
	(("gen" . _) (generate proj creator mail))
	(("generate" . _) (generate proj creator mail))
	(else (usage))))))

(define (usage)
  (print "kahua-package generate [-p=project] [-c=creator] [-m=mail@addr]")
  (exit 0))

