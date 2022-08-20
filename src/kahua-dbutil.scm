;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2006-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software.
;;

(use gauche.parseopt)
(use kahua.persistence)
(use kahua.persistence.efs)
(use kahua.config)

(define (msg fmt . args)
  (apply format #t fmt args))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (display "Usage: kahua-dbutil {check|fix} <dbname>\n")
      (display "       kahua-dbutil upgrade <path-to-fsdb>\n")
      (display "  *sorry but this command is very transitional.\n")
      (exit 1))))

(define (upgrade-fsdb path)
  (dbutil:kahua-db-fs->efs path)
  (display path)
  (newline)
  (exit 0))

(define (main args)
  (let-args (cdr args)
      ((site "S=s")
       (conf-file "c=s")
       (gosh "gosh=s") . args)
    (kahua-common-init site conf-file)
    (let* ((cmd (car args))
           (do-fix? (cond ((string=? cmd "check") #f)
                          ((string=? cmd "fix" )  #t)
                          ((string=? cmd "upgrade")
                           (upgrade-fsdb (cadr args)))
                          (else (usage))))
           (dbname (cadr args)))
      (msg "==Start checking: ~s==\n" dbname)
      (with-db (db dbname)
        (dbutil:check&fix-database db display do-fix?))
      (msg "==Done==\n")
      0)))
