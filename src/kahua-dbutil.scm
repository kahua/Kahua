;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-dbutil.scm,v 1.3 2006/09/07 02:54:55 bizenn Exp $

(use gauche.parseopt)
(use kahua.persistence)

(define (msg fmt . args)
  (apply format #t fmt args))

(define (usage)
  (display "Usage: kahua-dbutil {check|fix} <dbname>\n") (current-error-port)
  (exit 1))

(define (main args)
  (let-args (cdr args)
      ((gosh "gosh=s") . args)
    (let* ((cmd (car args))
	   (do-fix? (cond ((string=? cmd "check") #f)
			  ((string=? cmd "fix" )  #t)
			  (else (usage))))
	   (dbname (cadr args)))
      (if (or (#/^mysql/ dbname) (#/^pg/ dbname) (#/^postgresql/ dbname))
	  (with-db (db dbname)
	    (msg "==Start checking: ~s==\n" db)
	    (msg "Checking kahua_db_idcount...~a\n" (dbutil:check-kahua-db-idcount db do-fix?))
	    (msg "Checking kahua_db_classcount...~a\n" (dbutil:check-kahua-db-classcount db do-fix?))
	    (msg "==Done==\n"))
	  (format (current-error-port) "Sorry, ~s database not yet supported.\n"))
      0)))
