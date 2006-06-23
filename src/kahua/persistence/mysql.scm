;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on MySQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: mysql.scm,v 1.1.2.2 2006/06/23 05:09:19 bizenn Exp $

(define-module kahua.persistence.mysql
  (use kahua.persistence.dbi))

(select-module kahua.persistence.mysql)

;; ID counter in database.
(define (*create-kahua-db-idcount* db)
  (format "create table kahua_db_idcount (value integer not null) type=~a" (table-type-of db)))
(define-constant *select-kahua-db-idcount*
  "select last_insert_id()")
(define-constant *initialize-kahua-db-idcount*
  "insert into kahua_db_idcount values (-1)")
(define-constant *update-kahua-db-idcount*
  "update kahua_db_idcount set value = last_insert_id(value+1)")

;; Class table counter
(define (*create-kahua-db-classcount* db)
  (format "create table kahua_db_classcount (value integer not null) type=~a" (table-type-of db)))
(define-constant *select-kahua-db-classcount*
  "select last_insert_id()")
(define-constant *initialize-kahua-db-classcount*
  "insert into kahua_db_classcount values (-1)")
(define-constant *update-kahua-db-classcount*
  "update kahua_db_classcount set value = last_insert_id(value+1)")

;; Class - Table mapping table.
(define (*create-kahua-db-classes* db)
  (format "create table kahua_db_classes (
             class_name varchar(255) not null,
             table_name varchar(255) not null,
             constraint pk_kahua_db_classes primary key (class_name),
             constraint uq_kahua_db_classes unique (table_name)
           ) type=~a" (table-type-of db)))
(define-constant *select-kahua-db-classes*
  "select class_name, table_name from kahua_db_classes")
(define-constant *insert-kahua-db-classes*
  "insert into kahua_db_classes values (? , ?)")

(define-constant *create-class-table-format*
  "create table ~a (keyval varchar(255), dataval longtext, primary key (keyval)) type=~a")
(define-constant *insert-class-table-format* "insert into ~a values (?, ?)")
(define-constant *update-class-table-format* "update ~a set dataval = ? where keyval = ?")

(define-class <kahua-db-mysql> (<kahua-db-dbi>)
  ;; Now support :MyISAM and :InnoDB
  ((table-type :init-keyword :table-type :init-value :MyISAM :getter table-type-of)))
(define-method dataval-type ((self <kahua-db-mysql>)) "longtext")

(define (mysql-lock-table conn table directive)
  (dbi-do conn #`"lock tables ,|table| ,|directive|" '(:pass-through #t)))
(define (mysql-unlock-tables conn)
  (dbi-do conn "unlock tables" '(:pass-through #t)))

(define (with-mysql-table-write-lock conn table thunk)
  (dynamic-wind
      (cut mysql-lock-table conn table "write")
      thunk
      (cut mysql-unlock-tables conn)))

(define-method kahua-db-unique-id ((db <kahua-db-mysql>))
  (guard (e ((else (format (current-error-port)
			   "Error: kahua-db-unique-id-internal: ~a" (ref e 'message)))))
    (let1 conn (ref db 'connection)
      (with-mysql-table-write-lock conn "kahua_db_idcount"
	(lambda ()
	  (dbi-do conn *update-kahua-db-idcount* '(:pass-through #t))
	  (let* ((r (dbi-do conn *select-kahua-db-idcount* '(:pass-through #t)))
		 (cnt (x->integer (car (map (cut dbi-get-value <> 0) r))))) ; Oops!!
	    (set! (ref db 'id-counter) cnt)
	    cnt))))))

(define-method kahua-db-dbi-open ((db <kahua-db-mysql>) conn)
  (define (db-lock conn)
    (dbi-do conn "lock tables kahua_db_idcount write, kahua_db_classcount write, kahua_db_classes read" '(:pass-through #t)))
  (define db-unlock mysql-unlock-tables)
  (set! (ref db 'connection) conn)
  (dynamic-wind
      (lambda () #t)
      (lambda ()
	(guard (e ((<dbi-exception> e)
		   (dbi-do conn (*create-kahua-db-idcount* db) '(:pass-through #t))
		   (mysql-lock-table conn "kahua_db_idcount" "write")
		   (dbi-do conn (*create-kahua-db-classes* db) '(:pass-through #t))
		   (dbi-do conn (*create-kahua-db-classcount* db) '(:pass-through #t))
		   (db-lock conn)
		   (dbi-do conn *initialize-kahua-db-idcount* '(:pass-through #t))
		   (dbi-do conn *initialize-kahua-db-classcount* '(:pass-through #t)))
		  (else (raise e)))
	  (db-lock conn))
	(set! (ref db 'id-counter)
	      (and-let* ((r (dbi-do conn "select value from kahua_db_idcount limit 1" '(:pass-through #t)))
			 (p (map (cut dbi-get-value <> 0) r))
			 ((not (null? p))))
		(x->integer (car p))))
	(let1 r (dbi-do conn *select-kahua-db-classes* '(:pass-through #t))
	  (for-each (lambda (row)
		      (hash-table-put! (table-map-of db)
				       (string->symbol (dbi-get-value row 0))
				       (dbi-get-value row 1)))
		    r))
	db)
      (cut db-unlock conn)))

(define-method kahua-db-write-id-counter ((db <kahua-db-mysql>))
  #f)					; Dummy

(define-method write-kahua-instance ((db <kahua-db-mysql>)
                                     (obj <kahua-persistent-base>))
  (let1 conn (ref db 'connection)
    (define (table-name)
      (let* ((class (class-of obj))
	     (cname (class-name class))
	     (table-map (table-map-of db)))
	(with-mysql-table-write-lock conn "kahua_db_classes"
	  (lambda ()
	    (or (class-table-name db class)
		(let1 newtab (format "kahua_~a" (class-table-next-suffix db))
		  (dbi-do conn *insert-kahua-db-classes* '() cname newtab)
		  (dbi-do conn (format *create-class-table-format* newtab (table-type-of db))
			  '(:pass-through #t))
		  (hash-table-put! table-map cname newtab)
		  newtab))))))

    (let* ((data (call-with-output-string (cut kahua-write obj <>)))
	   (key  (key-of obj))
	   (tab  (table-name)))
      (with-mysql-table-write-lock conn tab
	(lambda ()
	  (if (ref obj '%floating-instance)
	      (dbi-do conn (format *insert-class-table-format* tab) '() key data)
	      (dbi-do conn (format *update-class-table-format* tab) '() data key))))
      (set! (ref obj '%floating-instance) #f)
      )))

(provide "kahua/persistence/mysql")
