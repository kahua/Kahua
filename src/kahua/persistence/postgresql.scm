;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on PostgreSQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: postgresql.scm,v 1.5 2006/10/20 07:36:28 bizenn Exp $

(define-module kahua.persistence.postgresql
  (use kahua.persistence.dbi))

(select-module kahua.persistence.postgresql)

(define-class <kahua-db-postgresql> (<kahua-db-dbi>) ())

(define-method kahua-db-dbi-build-dsn ((db <kahua-db-postgresql>) driver options)
  (format "dbi:pg:~a" options))

(define-method set-default-character-encoding! ((db <kahua-db-postgresql>))
  (and-let* ((client-encoding (case (gauche-character-encoding)
				((utf-8) 'UTF8)
				((euc-jp) 'EUC_JP)
				((sjis)  'SJIS)
				(else #f))))
    (dbi-do (connection-of db)
	    (format "set client_encoding=~a" client-encoding)
	    '(:pass-through #t))))

(define-method lock-tables ((db <kahua-db-postgresql>) . tables)
  (unless (null? tables)
    (let1 query (with-output-to-string
		  (lambda ()
		    (for-each (lambda (spec)
				(when (eq? :write (cdr spec))
				  (format "lock ~a in share row exclusive mode;" (car spec))))
			      (serialize-table-locks tables))))
      (dbi-do (connection-of db) query '(:pass-through #t)))))

(define-method unlock-tables ((db <kahua-db-postgresql>) . tables)
  #t)

(define-method create-kahua-db-idcount ((db <kahua-db-postgresql>))
  (define *create-kahua-db-idcount*
    (format "create sequence ~a start 0 minvalue 0" *kahua-db-idcount*))
  (dbi-do (connection-of db) *create-kahua-db-idcount* '(:pass-through #t)))

(define-method initialize-kahua-db-idcount ((db <kahua-db-postgresql>) n)
  (dbi-do (connection-of db)
	  (format "select setval('~a', ~d)" *kahua-db-idcount* n)
	  '(:pass-through #t)))

(define-constant *select-kahua-db-idcount*
  (format "select nextval('~a')" *kahua-db-idcount*))

(define-method create-kahua-db-classcount ((db <kahua-db-postgresql>))
  (define *create-kahua-db-classcount-format*
    (format "create sequence ~a start ~~d minvalue 0" *kahua-db-classcount*))
  (dbi-do (connection-of db) (format *create-kahua-db-classcount-format* 0) '(:pass-through #t)))
(define-method initialize-kahua-db-classcount ((db <kahua-db-postgresql>) n)
  (define *initialize-kahua-db-classcount-format*
    (format "select setval('~a',~~d)" *kahua-db-classcount*))
  (dbi-do (connection-of db) (format *initialize-kahua-db-classcount-format* n) '(:pass-through #t)))
(define-method select-kahua-db-classcount ((db <kahua-db-postgresql>))
  (define *select-kahua-db-classcount* (format "select nextval('~a')" *kahua-db-classcount*))
  (dbi-do (connection-of db) *select-kahua-db-classcount* '(:pass-through #t)))

(define-constant *check-kahua-db-classes*
  "select count(*) from pg_tables where tablename='kahua_db_classes'")

(define-method create-kahua-db-classes ((db <kahua-db-postgresql>))
  (dbi-do (connection-of db) *create-kahua-db-classes* '(:pass-through #t)))
(define-constant *create-kahua-db-classes*
  (format "create table ~a (
             class_name text not null,
             table_name name not null,
             constraint pk_~a primary key (class_name),
             constraint uq_~a unique (table_name)
           )" *kahua-db-classes* *kahua-db-classes* *kahua-db-classes*))
(define-constant *select-kahua-db-classes*
  "select class_name, table_name from kahua_db_classes")
(define-constant *lock-kahua-db-classes*
  "lock kahua_db_classes in share row exclusive mode")
(define-constant *kahua-db-class-table*
  "select table_name from kahua_db_classes where class_name=?")

(define (lock-class-table tabname)
  (format "lock ~a in share row exclusive mode" tabname))
(define (insert-class-instance tabname)
  (format "insert into ~a (keyval, dataval) values (?, ?)" tabname))
(define (update-class-instance tabname)
  (format "update ~a set dataval = ?, removed = ? where keyval = ?" tabname))

(define-method kahua-db-unique-id ((db <kahua-db-postgresql>))
  (x->integer (car (map (cut dbi-get-value <> 0)
			(dbi-do (connection-of db) *select-kahua-db-idcount* '(:pass-through #t))))))

(define-method class-table-next-suffix ((db <kahua-db-postgresql>))
  (car (map (cut dbi-get-value <> 0) (select-kahua-db-classcount db))))

(define-method create-kahua-class-table ((db <kahua-db-postgresql>)
					 (class <kahua-persistent-meta>))
  (define (create-class-table-sql tabname)
    (format "create table ~a (
               keyval  text not null,
               dataval text not null,
               removed smallint not null default 0,
             constraint pk_~a primary key (keyval)
             )" tabname tabname))
  (let ((cname (class-name class))
	(newtab (format *kahua-class-table-format* (class-table-next-suffix db))))
    (insert-kahua-db-classes db cname newtab)
    (dbi-do (connection-of db) (create-class-table-sql newtab) '(:pass-through #t))
    (add-index-to-table db newtab (format "idx_rmd_~a" newtab) #f "removed")
    (register-to-table-map db cname newtab)
    newtab))

(define-method table-should-be-locked? ((db <kahua-db-postgresql>)
					(obj <kahua-persistent-base>))
  (slot-ref obj '%floating-instance))

(define-method write-kahua-instance ((db <kahua-db-postgresql>)
				     (obj <kahua-persistent-base>)
				     (tab <string>))
  (let ((data (call-with-output-string (pa$ kahua-write obj)))
	(key  (key-of obj)))
    (let1 conn (connection-of db)
      (if (ref obj '%floating-instance)
	  (dbi-do conn (insert-class-instance tab) '() key data)
	  (dbi-do conn (update-class-instance tab) '() data (if (removed? obj) 1 0) key)))
    (set! (ref obj '%floating-instance) #f)
    ))

;;=================================================================
;; Database Consistency Check and Fix
;;

(define (last-value-of-sequence db name)
  (x->integer (car (map (cut dbi-get-value <> 0)
			(dbi-do (connection-of db) (format "select last_value from ~a" name) '())))))

(define-method dbutil:current-kahua-db-idcount ((db <kahua-db-postgresql>))
  (last-value-of-sequence db *kahua-db-idcount*))

(define-method dbutil:fix-kahua-db-idcount ((db <kahua-db-postgresql>) n)
  (initialize-kahua-db-idcount db n))

(define-method dbutil:create-kahua-db-idcount ((db <kahua-db-postgresql>) n)
  (let1 conn (connection-of db)
    (guard (e ((<dbi-exception> e) #f))
      (dbi-do conn (format "drop table ~a" *kahua-db-idcount*) '()))
    (guard (e ((<dbi-exception> e) #f))
      (dbi-do conn (format "create sequence ~a start ~d minvalue 0" *kahua-db-idcount* n)
	      '(:pass-through #t)))))

(define-method dbutil:current-kahua-db-classcount ((db <kahua-db-postgresql>))
  (last-value-of-sequence db *kahua-db-classcount*))

(define-method dbutil:fix-kahua-db-classcount ((db <kahua-db-postgresql>) n)
  (initialize-kahua-db-classcount db n))

(define-method dbutil:create-kahua-db-classcount ((db <kahua-db-postgresql>) n)
  (create-kahua-db-classcount db)
  (dbutil:fix-kahua-db-classcount db n))

(provide "kahua/persistence/postgresql")
