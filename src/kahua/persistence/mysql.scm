;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on MySQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: mysql.scm,v 1.1.2.7 2006/07/05 13:47:26 bizenn Exp $

(define-module kahua.persistence.mysql
  (use kahua.persistence.dbi))

(select-module kahua.persistence.mysql)

(define-constant *set-default-character-set*
  (format "set character set ~a" (case (gauche-character-encoding)
				   ((utf-8) 'utf8)
				   ((euc-jp) 'ujis)
				   ((sjis) 'sjis)
				   (else   'binary))))

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

(define-constant *db-charset-collation*
  (case (gauche-character-encoding)
    ((utf-8)  '(utf8   utf8_general_ci))
    ((euc-jp) '(ujis   ujis_japanese_ci))
    ((sjis)   '(sjis   sjis_japanese_ci))
    (else     '(binary binary))))

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
  "create table ~a (
    keyval varchar(255) binary not null,
    dataval longtext binary not null,
    constraint pk_~a primary key (keyval)
  ) type=~a")
(define-constant *insert-class-table-format* "insert into ~a values (?, ?)")
(define-constant *update-class-table-format* "update ~a set dataval = ? where keyval = ?")

(define-class <kahua-db-mysql> (<kahua-db-dbi>)
  ;; Now support :MyISAM and :InnoDB
  ((table-type :init-keyword :table-type :init-value :MyISAM :getter table-type-of)))

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
			   "Error: kahua-db-unique-id: ~a" (ref e 'message)))))
    (let1 conn (connection-of db)
      (with-mysql-table-write-lock conn "kahua_db_idcount"
	(lambda ()
	  (dbi-do conn *update-kahua-db-idcount* '(:pass-through #t))
	  (x->integer (car (map (cut dbi-get-value <> 0)
				(dbi-do conn *select-kahua-db-idcount* '(:pass-through #t))))))))))

(define-method class-table-next-suffix ((db <kahua-db-mysql>))
  (guard (e ((else (format (current-error-port)
			   "Error: class-table-next-suffix: ~a" (ref e 'message)))))
    (let1 conn (connection-of db)
      (with-mysql-table-write-lock conn "kahua_db_classcount"
	(lambda ()
	  (dbi-do conn *update-kahua-db-classcount* '(:pass-through #t))
	  (x->integer (car (map (cut dbi-get-value <> 0)
				(dbi-do conn *select-kahua-db-classcount* '(:pass-through #t))))))))))

(define-method kahua-db-dbi-open ((db <kahua-db-mysql>) conn)
  (define (db-lock conn)
    (dbi-do conn "
      lock tables kahua_db_idcount write,
                  kahua_db_classcount write,
                  kahua_db_classes read" '(:pass-through #t)))
  (define db-unlock mysql-unlock-tables)
  (set! (connection-of db) conn)
  (dynamic-wind
      (lambda ()
	(guard (e ((<dbi-exception> e) #f)
		  (else (raise e)))
	  (dbi-do conn *set-default-character-set* '(:pass-through #t))))
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
  (let1 conn (connection-of db)
    (define (table-name)
      (let* ((class (class-of obj))
	     (cname (class-name class))
	     (table-map (table-map-of db)))
	(with-mysql-table-write-lock conn "kahua_db_classes"
	  (lambda ()
	    (or (class->table-name db class)
		(let1 newtab (format "kahua_~a" (class-table-next-suffix db))
		  (dbi-do conn *insert-kahua-db-classes* '() cname newtab)
		  (dbi-do conn (format *create-class-table-format*
				       newtab newtab (table-type-of db))
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
	      (dbi-do conn (format *update-class-table-format* tab) '() data key))
	  ))
      (set! (ref obj '%floating-instance) #f)
      )))

(provide "kahua/persistence/mysql")
