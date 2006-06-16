;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on MySQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: mysql.scm,v 1.1.2.1 2006/06/16 08:13:16 bizenn Exp $

(define-module kahua.persistence.mysql
  (use kahua.persistence.dbi))

(select-module kahua.persistence.mysql)

(define-class <kahua-db-mysql> (<kahua-db-dbi>) ())
(define-method dataval-type ((self <kahua-db-mysql>))
  "longtext")

(define (mysql-lock-table conn table directive)
  (dbi-do conn #`"lock tables ,|table| ,|directive|" '(:pass-through #t)))
(define (mysql-unlock-tables conn)
  (dbi-do conn "unlock tables" '(:pass-through #t)))

(define (with-mysql-table-write-lock conn table thunk)
  (dynamic-wind
      (cut mysql-lock-table conn table "write")
      thunk
      (cut mysql-unlock-tables conn)))

(define-method kahua-db-unique-id-internal ((db <kahua-db-mysql>))
  (guard (e ((else (format (current-error-port) "Error: kahua-db-unique-id-internal: ~a" (ref e 'message)))))
    (let1 conn (ref db 'connection)
      (with-mysql-table-write-lock conn "kahua_db_idcount"
	(lambda ()
	  (let* ((r (dbi-do conn "select value from kahua_db_idcount limit 1" '(:pass-through #t)))
		 (cnt (x->integer (car (map (cut dbi-get-value <> 0) r))))
		 (next (+ cnt 1)))
	    (set! (ref db 'id-counter) next)
	    (dbi-do conn "update kahua_db_idcount set value = ?" '() (x->string next))
	    cnt))))))

(define-method kahua-db-dbi-open ((db <kahua-db-mysql>) conn)
  (define (db-lock conn)
    (dbi-do conn "lock tables kahua_db_idcount write, kahua_db_classes read" '(:pass-through #t)))
  (define db-unlock mysql-unlock-tables)
  (set! (ref db 'connection) conn)
  (dynamic-wind
      (lambda () #t)
      (lambda ()
	(guard (e ((<dbi-exception> e)
		   (dbi-do conn "create table kahua_db_idcount (value integer)" '(:pass-through #t))
		   (mysql-lock-table conn "kahua_db_idcount" "write")
		   (dbi-do
		    conn
		    "create table kahua_db_classes (class_name varchar(255), table_name varchar(255), primary key (class_name))"
		    '(:pass-through #t))
		   (db-lock conn)
		   (dbi-do conn "insert into kahua_db_idcount values (0)" '(:pass-through #t)))
		  (else (raise e)))
	  (db-lock conn))
	(set! (ref db 'id-counter)
	      (and-let* ((r (dbi-do conn "select value from kahua_db_idcount" '(:pass-through #t)))
			 (p (map (cut dbi-get-value <> 0) r))
			 ((not (null? p))))
		(x->integer (car p))))
	(set! (ref db 'table-map)
	      (and-let* ((r (dbi-do conn "select class_name, table_name from kahua_db_classes" '(:pass-through #t))))
		(map (lambda (row)
		       (cons (string->symbol (dbi-get-value row 0))
			     (dbi-get-value row 1)))
		     r)))
	db)
      (cut db-unlock conn)))

(define-method kahua-db-write-id-counter ((db <kahua-db-mysql>))
  #f)					; Dummy

(define-method write-kahua-instance ((db <kahua-db-mysql>)
                                     (obj <kahua-persistent-base>))
  (let1 conn (ref db 'connection)
    (define (table-name)
      (let1 cname (class-name (class-of obj))
	(or (assq-ref (ref db 'table-map) cname)
	    (let1 newtab (format "kahua_~a" (length (ref db 'table-map)))
	      (with-mysql-table-write-lock conn "kahua_db_classes"
		(lambda ()
		  (dbi-do conn "insert into kahua_db_classes values (? , ?)" '() cname newtab)))
	      (dbi-do
	       conn #`"create table ,|newtab| (keyval varchar(255),, dataval ,(dataval-type db),, primary key (keyval))"
	       '(:pass-through #t))
	      (push! (ref db 'table-map) (cons cname newtab))
	      newtab))))

    (let* ((data (call-with-output-string (cut kahua-write obj <>)))
	   (key  (key-of obj))
	   (tab  (table-name)))
      (with-mysql-table-write-lock conn tab
	(lambda ()
	  (if (ref obj '%floating-instance)
	      (dbi-do conn #`"insert into ,|tab| values (?,, ?)" '() key data)
	      (dbi-do conn #`"update ,|tab| set dataval = ? where keyval = ?" '() data key))))
      (set! (ref obj '%floating-instance) #f)
      )))

(provide "kahua/persistence/mysql")
