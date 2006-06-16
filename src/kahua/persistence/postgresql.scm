;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on PostgreSQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: postgresql.scm,v 1.1.2.1 2006/06/16 08:13:16 bizenn Exp $

(define-module kahua.persistence.postgresql
  (use kahua.persistence.dbi))

(select-module kahua.persistence.postgresql)

(define-class <kahua-db-postgresql> (<kahua-db-dbi>) ())

(define-method dataval-type ((self <kahua-db-postgresql>))
  "text")

(define-method kahua-db-unique-id-internal ((db <kahua-db-postgresql>))
  (begin0
    (ref db 'id-counter)
    (inc! (ref db 'id-counter))))

(define-method kahua-db-dbi-open ((db <kahua-db-postgresql>) conn)
  (define (safe-query query)
    (guard (e ((<dbi-exception> e) #f)
	      (else (raise e)))
      (dbi-do conn query '(:pass-through #t))))

  (define (query-idcount)
    (and-let* ((r (safe-query "select value from kahua_db_idcount"))
	       (p (map (cut dbi-get-value <> 0) r))
	       ((not (null? p))))
      (x->integer (car p))))

  (define (query-classtable)
    (and-let* ((r (safe-query "select class_name, table_name from kahua_db_classes")))
      (map (lambda (row)
	     (cons (string->symbol (dbi-get-value row 0))
		   (dbi-get-value row 1)))
	   r)))
  (set! (ref db 'connection) conn)
  ;; check table existence
  (let1 z (query-idcount)
    (unless z
      ;; this is the first time we use db.
      ;; TODO: error check
      (for-each
       (cut dbi-do conn <> '(:pass-through #t))
       '("create table kahua_db_classes (class_name varchar(255), table_name varchar(255), primary key (class_name))"
	 "create table kahua_db_idcount (value integer)"
	 "insert into kahua_db_idcount values (0)"))
      (let1 zz (query-idcount)
	(unless zz
	  (error "couldn't initialize database"))
	(set! z zz)))
    (set! (ref db 'id-counter) z)
    (set! (ref db 'table-map) (query-classtable))
    db))

(define-method kahua-db-write-id-counter ((db <kahua-db-postgresql>))
  (dbi-do (ref db 'connection)
          "update kahua_db_idcount set value = ?" '() (ref db 'id-counter)))

(define-method write-kahua-instance ((db <kahua-db-postgresql>)
                                     (obj <kahua-persistent-base>))
  (define (table-name)
    (let1 cname (class-name (class-of obj))
      (or (assq-ref (ref db 'table-map) cname)
          (let1 newtab (format "kahua_~a" (length (ref db 'table-map)))
            (dbi-do
             (ref db 'connection)
             "insert into kahua_db_classes values (? , ?)" '() cname newtab)
            (dbi-do
             (ref db 'connection)
             #`"create table ,|newtab| (keyval varchar(255),, dataval ,(dataval-type db),, primary key (keyval))"
             '(:pass-through #t))
            (push! (ref db 'table-map) (cons cname newtab))
            newtab))))

  (let* ((data (call-with-output-string (cut kahua-write obj <>)))
         (key  (key-of obj))
         (tab  (table-name)))
    (if (ref obj '%floating-instance)
        (dbi-do
         (ref db 'connection)
         #`"insert into ,|tab| values (?,, ?)" '() key data)
      (dbi-do
       (ref db 'connection)
       #`"update ,|tab| set dataval = ? where keyval = ?" '() data key))
    (set! (ref obj '%floating-instance) #f)
    ))

(provide "kahua/persistence/postgresql")
