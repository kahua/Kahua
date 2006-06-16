;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on DBI abstract storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: dbi.scm,v 1.1.2.1 2006/06/16 08:13:16 bizenn Exp $

(define-module kahua.persistence.dbi
  (extend kahua.persistence
	  dbi util.list
	  gauche.collection
	  gauche.logger)
  (export <kahua-db-dbi>
	  dataval-type
	  kahua-db-unique-id-internal
	  lock-db
	  unlock-db
	  kahua-db-open
	  kahua-db-dbi-open
	  kahua-db-write-id-counter
	  kahua-db-close
	  read-kahua-instance
	  write-kahua-instance
	  make-kahua-collection))

(select-module kahua.persistence.dbi)

;; DBI-based persistent store (optional)
;;  Currently, only one server per backend database type can be 
;;  used simultaneously, since a driver will be created as a
;;  singleton.
;;  NB: DBI bridge is _temporary_.  The current implementation
;;  isn't efficient, and also it has hazards when multiple
;;  processes access to the same DB.  The data format in DB
;;  will be changed in future in incompatible way.
(define-class <kahua-db-dbi> (<kahua-db>)
  ((driver     :allocation :each-subclass :init-value #f)
   (user       :allocation :each-subclass :init-value #f)
   (password   :allocation :each-subclass :init-value #f)
   (options    :allocation :each-subclass :init-value #f)
   (connection :init-value #f)
   (table-map  :init-value '())  ;; class-name -> table map
   ))

(define (kahua-dbi-warn fname)
  (format (current-error-port)
	  "** ~a should be overridden in concrete database driver module.\n" fname))

(define-method dataval-type ((self <kahua-db-dbi>))
  (kahua-dbi-warn "dataval-type")
  "text")

;(when (library-exists? 'dbi :strict? #f)
;  (autoload dbi
;            <dbi-exception>
;            dbi-do
;            dbi-make-driver dbi-make-connection dbi-close dbi-get-value dbi-escape-sql))

(define-method initialize ((db <kahua-db-dbi>) initargs)
  (next-method)
  (unless (ref db 'driver)
    (let1 m (#/(.*?):(?:([^:]+)(?::([^:]*)(?::(.*))?)?)?/ (ref db 'path))
      (unless m (errorf "unsupported database driver path: ~a" (ref db 'path)))
      (set! (ref db 'driver)   (dbi-make-driver (m 1)))
      (set! (ref db 'user)     (m 2))
      (set! (ref db 'password) (m 3))
      (set! (ref db 'options)  (m 4))
      (log-format "DBI(~a) setup: user ~a, options ~a" (m 1) (m 2) (m 4))
      )))

(define-method kahua-db-unique-id-internal ((db <kahua-db-dbi>))
  (kahua-dbi-warn "kahua-db-unique-id-internal")
  (begin0
    (ref db 'id-counter)
    (inc! (ref db 'id-counter))))

(define-method lock-db ((db <kahua-db-dbi>)) #t)
(define-method unlock-db ((db <kahua-db-dbi>)) #t)

(define-method kahua-db-open ((db <kahua-db-dbi>))
  (let1 conn (dbi-make-connection (ref db 'driver)
				  (ref db 'user)
				  (ref db 'password)
				  (ref db 'options))
    (set! (ref db 'active) #t)
    (kahua-db-dbi-open db conn)))

(define-method kahua-db-dbi-open ((db <kahua-db-dbi>) conn)
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

  (kahua-dbi-warn "kahua-db-dbi-open")
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

(define-method kahua-db-write-id-counter ((db <kahua-db-dbi>))
  (kahua-dbi-warn "kahua-db-write-id-counter")
  (dbi-do (ref db 'connection)
          "update kahua_db_idcount set value = ?" '() (ref db 'id-counter)))

(define-method kahua-db-close ((db <kahua-db-dbi>) commit)
  (if commit
      (kahua-db-sync db)
    (kahua-db-rollback db))
  (dbi-close (ref db 'connection))
  (set! (ref db 'connection) #f)
  (set! (ref db 'modified-instances) '())
  (set! (ref db 'active) #f))

(define-method read-kahua-instance ((db <kahua-db-dbi>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))

  (and-let* ((tab (assq-ref (ref db 'table-map) (class-name class)))
             (r (dbi-do
                 (ref db 'connection)
                 #`"select dataval from ,|tab| where keyval = ?" '() key))
             (rv  (map (cut dbi-get-value <> 0) r))
             ((not (null? rv))))
    (call-with-input-string (car rv) read)))

(define-method write-kahua-instance ((db <kahua-db-dbi>)
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

  (kahua-dbi-warn "write-kahua-instance")
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

(define-method make-kahua-collection ((db <kahua-db-dbi>)
                                      class opts)
  (let1 tab (assq-ref (ref db 'table-map) (class-name class))
    (if (not tab)
      (make <kahua-collection> :instances '())
      (let* ((r (dbi-do (ref db 'connection)
                        #`"select keyval from ,|tab|" '(:pass-through #t)))
             (keys (if r (map (cut dbi-get-value <> 0) r) '())))
        (make <kahua-collection>
          :instances (map (cut find-kahua-instance class <>) keys))))))

(provide "kahua/persistence/dbi")
