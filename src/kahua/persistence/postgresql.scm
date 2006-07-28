;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on PostgreSQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: postgresql.scm,v 1.1.2.6 2006/07/28 07:43:32 bizenn Exp $

(define-module kahua.persistence.postgresql
  (use kahua.persistence.dbi))

(select-module kahua.persistence.postgresql)

(define-constant *set-default-character-set*
  (and-let* ((client-encoding (case (gauche-character-encoding)
				((utf-8) 'UTF8)
				((euc-jp) 'EUC_JP)
				((sjis)  'SJIS)
				(else #f))))
    (format "set client_encoding=~a" client-encoding)))

(define-class <kahua-db-postgresql> (<kahua-db-dbi>) ())

(define-constant *create-kahua-db-idcount*
  "create sequence kahua_db_idcount start 0 minvalue 0")
(define-constant *select-kahua-db-idcount*
  "select nextval('kahua_db_idcount')")

(define-constant *create-kahua-db-classcount*
  "create sequence kahua_db_classcount start 0 minvalue 0")
(define-constant *select-kahua-db-classcount*
  "select nextval('kahua_db_classcount')")

(define-constant *check-kahua-db-classes*
  "select count(*) from pg_tables where tablename='kahua_db_classes'")

(define-constant *create-kahua-db-classes*
  "create table kahua_db_classes (
     class_name text not null,
     table_name name not null,
     constraint pk_kahua_db_classes primary key (class_name),
     constraint uq_kahua_db_classes unique (table_name)
   )")
(define-constant *insert-kahua-db-classes*
  "insert into kahua_db_classes values (? , ?)")
(define-constant *select-kahua-db-classes*
  "select class_name, table_name from kahua_db_classes")
(define-constant *lock-kahua-db-classes*
  "lock kahua_db_classes in share row exclusive mode")
(define-constant *kahua-db-class-table*
  "select table_name from kahua_db_classes where class_name=?")

(define (create-class-table tabname)
  (format "create table ~a (
             keyval  text not null,
             dataval text not null,
           constraint pk_~a primary key (keyval)
           )" tabname tabname))
(define (lock-class-table tabname)
  (format "lock ~a in share row exclusive mode" tabname))
(define (insert-class-instance tabname)
  (format "insert into ~a values (?, ?)" tabname))
(define (update-class-instance tabname)
  (format "update ~a set dataval = ? where keyval = ?" tabname))

(define-method kahua-db-unique-id ((db <kahua-db-postgresql>))
  (let* ((r (dbi-do (connection-of db) *select-kahua-db-idcount* '(:pass-through #t)))
	 (cnt (x->integer (car (map (cut dbi-get-value <> 0) r)))))
    cnt))

(define-method kahua-db-dbi-open ((db <kahua-db-postgresql>) conn)
  (define (safe-query query)
    (guard (e ((<dbi-exception> e) #f)
	      (else (raise e)))
      (dbi-do conn query '(:pass-through #t))))

  (define (query-classtable)
    (and-let* ((r (safe-query *select-kahua-db-classes*)))
      (map (lambda (row)
	     (cons (string->symbol (dbi-get-value row 0))
		   (dbi-get-value row 1)))
	   r)))

  (define (kahua-db-classes-exists?)
    (and-let* ((r (dbi-do conn *check-kahua-db-classes* '()))
	       (l (map (cut dbi-get-value <> 0) r))
	       ((not (null? l))))
      (positive? (x->integer (car l)))))

  (set! (connection-of db) conn)
  (safe-query *set-default-character-set*)
  ;; check table existence
  (let1 table-map (table-map-of db)
    (for-each (lambda (p)
		(hash-table-put! table-map (car p) (cdr p)))
	      (or (query-classtable)
		  (begin
		    (for-each (cut dbi-do conn <> '(:pass-through #t))
			      `(,*create-kahua-db-classes*
				,*create-kahua-db-idcount*
				,*create-kahua-db-classcount*))
		    (query-classtable)))))
  db)

(define-method kahua-db-write-id-counter ((db <kahua-db-postgresql>))
  #f)

(define-method class-table-next-suffix ((db <kahua-db-postgresql>))
  (let1 r (dbi-do (connection-of db) *select-kahua-db-classcount* '(:pass-through #t))
    (car (map (cut dbi-get-value <> 0) r))))

(define-method write-kahua-instance ((db <kahua-db-postgresql>)
                                     (obj <kahua-persistent-base>))
  (let1 conn (connection-of db)
    (define (table-name)
      (let1 cname (class-name (class-of obj))
	(with-transaction db
	  (lambda (conn)
	    (dbi-do conn *lock-kahua-db-classes* '(:pass-through #t))
	    (or (class->table-name db (class-of obj))
		(let1 newtab (format "kahua_~a" (class-table-next-suffix db))
		  (dbi-do conn *insert-kahua-db-classes* '() cname newtab)
		  (dbi-do conn (create-class-table newtab) '(:pass-through #t))
		  (hash-table-put! (table-map-of db) cname newtab)
		  newtab))))))

	(let* ((data (call-with-output-string (cut kahua-write obj <>)))
	       (key  (key-of obj))
	       (tab  (table-name)))
	  (with-transaction db
	    (lambda (conn)
	      (if (ref obj '%floating-instance)
		  (begin
		    (dbi-do conn (lock-class-table tab) '(:pass-through #t))
		    (dbi-do conn (insert-class-instance tab) '() key data))
		  (dbi-do conn (update-class-instance tab) '() data key))))
	  (set! (ref obj '%floating-instance) #f)
	  )))

;;=================================================================
;; Database Consistency Check and Fix
;;

(define (last-value-of-sequence db name)
  (x->integer (car (map (cut dbi-get-value <> 0)
			(dbi-do (connection-of db) (format "select last_value from ~a" name) '())))))

(define-method replace-kahua-db-idcount ((db <kahua-db-postgresql>) n)
  (let1 conn (connection-of db)
    (guard (e ((<dbi-exception> e) #f))
      (dbi-do conn "drop table kahua_db_idcount" '())
      (dbi-do conn (format "create sequence kahua_db_idcount start ~d minvalue 0" n)
	      '(:pass-through #t)))))

(define-method check-kahua-db-idcount ((db <kahua-db-postgresql>) . maybe-do-fix?)
  (let* ((do-fix? (get-optional maybe-do-fix? #f))
	 (max-id  (max-kahua-key-from-idcount db))
	 (idcount (guard (e ((<dbi-exception> e)
			     (cond (do-fix?
				    (replace-kahua-db-idcount db max-id)
				    max-id)
				   (else -1))))
		    (last-value-of-sequence db "kahua_db_idcount"))))
    (or (and (<= max-id idcount) 'OK)
	(and do-fix?
	     (begin
	       (dbi-do (connection-of db)
		       (format "select setval('kahua_db_idcount', ~d)" max-id)
		       '(:pass-through #t))
	       'FIXED))
	'NG)))

(define-method create-kahua-db-classcount ((db <kahua-db-postgresql>) n)
  (dbi-do (connection-of db) (format "create sequence kahua_db_classcount start ~d minvalue 0" n)
	  '(:pass-through #t)))

(define-method check-kahua-db-classcount ((db <kahua-db-postgresql>) . maybe-do-fix?)
  (let* ((do-fix? (get-optional maybe-do-fix? #f))
	 (max-id (max-table-name-suffix db))
	 (classcount (guard (e ((<dbi-exception> e)
				(cond (do-fix?
				       (create-kahua-db-classcount db max-id)
				       max-id)
				      (else -1))))
		       (last-value-of-sequence db "kahua_db_classcount"))))
    (or (and (<= max-id classcount) 'OK)
	(and do-fix?
	     (begin
	       (dbi-do (connection-of db)
		       (format "select setval('kahua_db_classcount', ~d)" max-id)
		       '(:pass-through #t))
	       'FIXED))
	'NG)))

(provide "kahua/persistence/postgresql")
