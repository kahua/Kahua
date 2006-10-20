;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-dbutil.scm,v 1.5 2006/10/20 07:36:28 bizenn Exp $

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
      (msg "==Start checking: ~s==\n" dbname)
      (with-db (db dbname)
	(dbutil:check&fix-database db display do-fix?))
      (msg "==Done==\n")
      0)))
