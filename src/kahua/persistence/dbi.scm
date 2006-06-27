;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on DBI abstract storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: dbi.scm,v 1.1.2.3 2006/06/27 02:34:05 bizenn Exp $

(define-module kahua.persistence.dbi
  (extend kahua.persistence
	  dbi util.list
	  gauche.collection
	  gauche.logger)
  (export <kahua-db-dbi>
	  kahua-db-unique-id
	  lock-db
	  unlock-db
	  kahua-db-open
	  kahua-db-dbi-open
	  kahua-db-write-id-counter
	  kahua-db-close
	  read-kahua-instance
	  write-kahua-instance
	  make-kahua-collection
	  class-name->table-name
	  class-table-next-suffix
	  with-transaction))

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
  ((dsn        :init-value #f :accessor dsn-of)
   (user       :init-value #f :accessor user-of)
   (password   :init-value #f :accessor password-of)
   (connection :init-value #f :accessor connection-of)
   (table-map  :init-form (make-hash-table) :getter table-map-of)
   ))

(define (kahua-dbi-warn fname)
  (format (current-error-port)
	  "** ~a should be overridden in concrete database driver module.\n" fname))

(define-method dataval-type ((self <kahua-db-dbi>))
  (kahua-dbi-warn "dataval-type")
  "text")

(define-method initialize ((db <kahua-db-dbi>) initargs)
  (next-method)
  (unless (dsn-of db)
    (let1 m (#/(.*?):(?:([^:]+)(?::([^:]*)(?::(.*))?)?)?/ (ref db 'path))
      (unless m (errorf "unsupported database driver path: ~a" (ref db 'path)))
      (set! (dsn-of db) (format "dbi:~a:~a" (m 1) (m 4)))
      (set! (user-of db)     (m 2))
      (set! (password-of db) (m 3))
      (log-format "DBI(~a) setup: user ~a, options ~a" (m 1) (m 2) (m 4))
      )))

(define-method with-transaction ((db <kahua-db-dbi>) proc)
  (let1 conn (connection-of db)
    (guard (e (else (dbi-do conn "rollback" '(:pass-through #t))))
      (dbi-do conn "start transaction" '(:pass-through #t))
      (begin0
	(proc conn)
	(dbi-do conn "commit" '(:pass-through #t))))))

(define-method kahua-db-unique-id ((db <kahua-db-dbi>))
  (kahua-dbi-warn "kahua-db-unique-id")
  (begin0
    (ref db 'id-counter)
    (inc! (ref db 'id-counter))))

(define-method lock-db ((db <kahua-db-dbi>)) #t)
(define-method unlock-db ((db <kahua-db-dbi>)) #t)

(define-method kahua-db-open ((db <kahua-db-dbi>))
  (let1 conn (dbi-connect (dsn-of db)
			  :username (user-of db)
			  :password (password-of db))
    (set! (active? db) #t)
    (kahua-db-dbi-open db conn)))

(define-constant *create-kahua-db-classes*
  "create table kahua_db_classes (
     class_name varchar(255) not null,
     table_name varchar(255) not null,
     constraint pk_kahua_db_classes primary key (class_name),
     constraint uq_kahua_db_classes unique (table_name)
  )")
(define-constant *create-kahua-db-idcount*
  "create table kahua_db_idcount (value integer)")
(define-constant *initialize-kahua-db-idcount*
  "insert into kahua_db_idcount values (0)")

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
  (set! (connection-of db) conn)
  ;; check table existence
  (let1 z (query-idcount)
    (unless z
      ;; this is the first time we use db.
      ;; TODO: error check
      (for-each
       (cut dbi-do conn <> '(:pass-through #t))
       `(,*create-kahua-db-classes*
	 ,*create-kahua-db-idcount*
	 ,*initialize-kahua-db-idcount*))
      (let1 zz (query-idcount)
	(unless zz
	  (error "couldn't initialize database"))
	(set! z zz)))
    (set! (ref db 'id-counter) z)
    (for-each (lambda (p)
		(hash-table-put! (table-map-of db) (car p) (cdr p)))
	      (query-classtable))
    db))

(define-constant *update-kahua-db-idcount*
  "update kahua_db_idcount set value = ?")

(define-method kahua-db-write-id-counter ((db <kahua-db-dbi>))
  (kahua-dbi-warn "kahua-db-write-id-counter")
  (dbi-do (connection-of db) *update-kahua-db-idcount* '() (ref db 'id-counter)))

(define-method kahua-db-close ((db <kahua-db-dbi>) commit)
  (if commit
      (kahua-db-sync db)
    (kahua-db-rollback db))
  (dbi-close (connection-of db))
  (set! (connection-of db) #f)
  (set! (ref db 'modified-instances) '())
  (set! (active? db) #f))

(define-method class-table-next-suffix ((db <kahua-db-dbi>))
  (let1 r (dbi-do (connection-of db) "select count(*) from kahua_db_classes" '())
    (car (map (cut dbi-get-value <> 0) r))))

(define-constant *class-table-name*
  "select table_name from kahua_db_classes where class_name=?")
(define-constant (select-class-instance tabname)
  (format "select dataval from ~a where keyval=?" tabname))

(define-method class-name->table-name ((db <kahua-db-dbi>) (class <kahua-persistent-meta>))
  (let ((cname  (class-name class))
	(table-map (table-map-of db)))
    (or (hash-table-get table-map cname #f)
	(and-let* ((conn (connection-of db))
		   (r (dbi-do conn *class-table-name* '() cname))
		   (l (map (cut dbi-get-value <> 0) r))
		   ((not (null? l)))
		   (tabname (car l)))
	  (hash-table-put! table-map cname tabname)
	  tabname))))

(define-method read-kahua-instance ((db <kahua-db-dbi>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (and-let* ((conn (connection-of db))
	     (tab (class-name->table-name db class))
             (r (dbi-do conn (select-class-instance tab) '() key))
             (rv  (map (cut dbi-get-value <> 0) r))
             ((not (null? rv))))
    (call-with-input-string (car rv) read)))

(define-method write-kahua-instance ((db <kahua-db-dbi>)
                                     (obj <kahua-persistent-base>))
  (define (table-name)
    (let* ((class (class-of obj))
	   (cname (class-name class))
	   (conn (connection-of db)))
      (or (class-name->table-name db class)
          (let1 newtab (format "kahua_~a" (class-table-next-suffix db))
            (dbi-do conn "insert into kahua_db_classes values (? , ?)" '() cname newtab)
            (dbi-do conn #`"create table ,|newtab| (keyval varchar(255),, dataval ,(dataval-type db),, primary key (keyval))" '(:pass-through #t))
            (push! (ref db 'table-map) (cons cname newtab))
            newtab))))

  (kahua-dbi-warn "write-kahua-instance")
  (let* ((data (call-with-output-string (cut kahua-write obj <>)))
         (key  (key-of obj))
         (tab  (table-name)))
    (if (ref obj '%floating-instance)
        (dbi-do (connection-of db)
		#`"insert into ,|tab| values (?,, ?)" '() key data)
      (dbi-do (connection-of db)
	      #`"update ,|tab| set dataval = ? where keyval = ?" '() data key))
    (set! (ref obj '%floating-instance) #f)
    ))

(define-method make-kahua-collection ((db <kahua-db-dbi>)
                                      class opts)
  (let* ((conn (connection-of db))
	 (tab (class-name->table-name db class)))
    (if (not tab)
      (make <kahua-collection> :instances '())
      (let* ((r (dbi-do (connection-of db)
                        #`"select keyval from ,|tab|" '(:pass-through #t)))
             (keys (if r (map (cut dbi-get-value <> 0) r) '())))
        (make <kahua-collection>
          :instances (map (cut find-kahua-instance class <>) keys))))))

(provide "kahua/persistence/dbi")
