;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on DBI abstract storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: dbi.scm,v 1.8 2006/09/25 09:15:43 bizenn Exp $

(define-module kahua.persistence.dbi
  (use srfi-1)
  (use kahua.util)
  (extend kahua.persistence
	  dbi util.list
	  gauche.collection
	  gauche.logger)
  (export <kahua-db-dbi>
	  set-default-character-encoding!
	  kahua-db-dbi-open
	  with-dbi-transaction
	  serialize-table-locks
	  lock-tables
	  unlock-tables
	  with-locking-tables
	  table-should-be-locked?

	  *kahua-db-classes*
	  create-kahua-db-classes
	  select-kahua-db-classes
	  select-all-kahua-db-classes
	  insert-kahua-db-classes
	  kahua-class->table-name*

	  *kahua-db-idcount*
	  create-kahua-db-idcount
	  *kahua-db-classcount*
	  class-table-next-suffix
	  *kahua-class-table-format*
	  create-kahua-class-table
	  create-kahua-db-classcount
	  register-to-table-map

	  ;; Database Consistency Check and Fix.
	  dbutil:current-kahua-db-classcount
	  dbutil:fix-kahua-db-classcount
	  dbutil:create-kahua-db-classcount
	  dbutil:current-kahua-db-idcount
	  dbutil:fix-kahua-db-idcount
	  dbutil:create-kahua-db-idcount
	  ;; Utility
	  safe-execute

	  add-column-to-table
	  drop-column-from-table
	  add-index-to-table
	  drop-index-from-table
	  ))

(select-module kahua.persistence.dbi)

(define-condition-type <kahua-persistence-dbi-error> <kahua-error> #f)

;; Debug Facility
;(define dbi-do% dbi-do)
;(define (dbi-do conn sql opts . params)
;  (format (current-error-port) "SQL: ~a\n" sql)
;  (apply dbi-do% conn sql opts params))

;; DBI-based persistent store (optional)
;;  Currently, only one server per backend database type can be 
;;  used simultaneously, since a driver will be created as a
;;  singleton.
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

(define-method with-dbi-transaction ((db <kahua-db-dbi>) proc)
  (let1 conn (connection-of db)
    (guard (e (else
	       (dbi-do conn "rollback" '(:pass-through #t))
	       (raise e)))
      (dbi-do conn "start transaction" '(:pass-through #t))
      (begin0
	(proc conn)
	(dbi-do conn "commit" '(:pass-through #t))))))

(define-method lock-db ((db <kahua-db-dbi>)) #t)
(define-method unlock-db ((db <kahua-db-dbi>)) #t)
(define (serialize-table-locks specs)
  (reverse!
   (fold (lambda (e r)
	   (let* ((e (if (pair? e)
			 e
			 (cons e :write)))
		  (table (car e)))
	     (cond ((assoc table r string=?) =>
		    (lambda (old)
		      (unless (eq? (cdr old) (cdr e))
			(set-cdr! old :write))
		      r))
		   (else (cons e r)))))
	 '()
	 specs)))
(define-generic lock-tables)
(define-generic unlock-tables)
(define-method with-locking-tables ((db <kahua-db>)
				    thunk
				    . tables)
  (dynamic-wind
      (cut apply lock-tables db tables)
      thunk
      (cut apply unlock-tables db tables)))

(define-method kahua-db-open ((db <kahua-db-dbi>))
  (let1 conn (dbi-connect (dsn-of db)
			   :username (user-of db)
			   :password (password-of db))
    (set! (active? db) #t)
    (kahua-db-dbi-open db conn)))

(define-method kahua-db-reopen ((db <kahua-db-dbi>))
  (define (hash-table-clear! ht)
    (hash-table-for-each ht (lambda (k v) (hash-table-delete! ht k))))
  (hash-table-clear! (table-map-of db))
  (kahua-db-open db))

(define-method kahua-db-ping ((db <kahua-db-dbi>))
  (safe-execute (cut dbi-do (connection-of db)
		     "select class_name from kahua_db_classes where class_name is NULL"
		     '(pass-through #t))))

(define-generic set-default-character-encoding!)

(define (safe-execute thunk)
  (guard (e ((<dbi-exception> e) #f)
	    (else (raise e)))
    (thunk)))

(define-method kahua-db-dbi-open ((db <kahua-db-dbi>) conn)
  (define (query-classtable)
    (and-let* ((r (safe-execute (cut select-all-kahua-db-classes db))))
      (map (lambda (row)
	     (list (string->symbol (dbi-get-value row 0))
		   (dbi-get-value row 1)))
	   r)))

  (set! (connection-of db) conn)
  (set-default-character-encoding! db)
  ;; check table existence
  (with-locking-db db
    (lambda _
      (for-each (pa$ apply register-to-table-map db)
		(or (query-classtable)
		    (begin
		      (kahua-db-create db)
		      (query-classtable))))))
  db)

(define-method kahua-db-create ((db <kahua-db-dbi>))
  (safe-execute (cut create-kahua-db-classes db))
  (safe-execute (cut create-kahua-db-classcount db))
  (safe-execute (cut create-kahua-db-idcount db)))

(define-method kahua-db-close ((db <kahua-db-dbi>) commit?)
  (dbi-close (connection-of db))
  (set! (connection-of db) #f)
  (set! (active? db) #f))

(define-method start-kahua-db-transaction ((db <kahua-db-dbi>))
  (next-method))
(define-method finish-kahua-db-transaction ((db <kahua-db-dbi>) commit?)
  (if commit?
      (kahua-db-sync db)
      (kahua-db-rollback db))
  (next-method))

;;
;; kahua_db_classes: Persistent class name <-> table name mapping.
;;
;; [Spec]
;; Column Name	|	Type	|	Constraint
;; ------------------------------------------------
;; class_name	|	text	|	primary key
;; table_name	|	text	|	unique
(define-constant *kahua-db-classes* "kahua_db_classes")

;; You would like to override this in a concrete driver.
(define-method create-kahua-db-classes ((db <kahua-db-dbi>))
  (define *create-kahua-db-classes*
    (format
      "create table ~a (
         class_name varchar(255) not null,
         table_name varchar(255) not null,
         constraint pk_~a primary key (class_name),
         constraint uq_~a unique (table_name)
       )"
      *kahua-db-classes* *kahua-db-classes* *kahua-db-classes*))
  (dbi-do (connection-of db) *create-kahua-db-classes* '(:pass-through #t)))

;; Maybe you don't need to override this.
(define-method select-kahua-db-classes ((db <kahua-db-dbi>) cname)
  (define *select-kahua-db-classes*
    (format "select table_name from ~a where class_name = ?" *kahua-db-classes*))
  (and-let* ((l (map (cut dbi-get-value <> 0)
		     (dbi-do (connection-of db) *select-kahua-db-classes* '() cname)))
	     ((not (null? l))))
    (car l)))

;; Ditto.
(define-method select-all-kahua-db-classes ((db <kahua-db-dbi>))
  (define *select-all-kahua-db-classes*
    (format "select class_name, table_name from ~a" *kahua-db-classes*))
  (dbi-do (connection-of db) *select-all-kahua-db-classes* '(:pass-through #t)))

;; Ditto.
(define-method insert-kahua-db-classes ((db <kahua-db-dbi>)
					(cname <symbol>)
					(tname <string>))
  (define *insert-kahua-db-classes*
    (format "insert into ~a values (?, ?)" *kahua-db-classes*))
  (dbi-do (connection-of db) *insert-kahua-db-classes* '() cname tname))

;;
;; kahua_db_idcount: ID counter for Object ID.
;;
;; It doesn't have to be a table. It may be a sequence.
(define-constant *kahua-db-idcount* "kahua_db_idcount")
(define-generic create-kahua-db-idcount)
(define-generic initialize-kahua-db-idcount)

;;
;; kahua_db_classcount: counter for class table suffix number.
;;
(define-constant *kahua-db-classcount* "kahua_db_classcount")
(define-generic class-table-next-suffix)
(define-generic create-kahua-db-classcount)
(define-generic initialize-kahua-db-classcount)
(define-generic select-kahua-db-classcount)

;;
;; kahua_\d+: Table storing instances.
;;
(define-constant *kahua-class-table-format* "kahua_~d")
(define-generic create-kahua-class-table)

(define-method register-to-table-map ((db <kahua-db-dbi>)
				      (cname <symbol>)
				      (tabname <string>))
  (hash-table-put! (table-map-of db) cname tabname))
(define-method get-from-table-map ((db <kahua-db-dbi>)
				   (cname <symbol>))
  (hash-table-get (table-map-of db) cname #f))

(define-method kahua-class->table-name ((db <kahua-db-dbi>)
				 (class <kahua-persistent-meta>))
  (let1 cname  (class-name class)
    (or (get-from-table-map db cname)
	(and-let* ((tabname (select-kahua-db-classes db cname)))
	  (register-to-table-map db cname tabname)
	  tabname))))

(define-method kahua-class->table-name* ((db <kahua-db-dbi>)
					 (class <kahua-persistent-meta>))
  (with-dbi-transaction db
    (lambda (conn)
      (with-locking-tables db
	(lambda ()
	  (or (kahua-class->table-name db class)
	      (create-kahua-class-table db class)))
	*kahua-db-classes*))))

;; You must override kahua-db-unique-id.
(define-method kahua-db-write-id-counter ((db <kahua-db-dbi>)) #f)

(define-method read-kahua-instance ((db <kahua-db-dbi>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (define (select-class-instance tabname)
    (format "select dataval from ~a where keyval=?" tabname))
  (and-let* ((conn (connection-of db))
	     (tab (kahua-class->table-name db class))
             (r (dbi-do conn (select-class-instance tab) '() key))
             (rv  (map (cut dbi-get-value <> 0) r))
             ((not (null? rv))))
    (call-with-input-string (car rv) read)))

(define-method kahua-persistent-instances ((db <kahua-db-dbi>) class keys filter-proc include-removed-object?)
  (let ((cn (class-name class))
	(icache (ref db 'instance-by-key))
	(conn (connection-of db)))
    (define (%select-instances tab where)
      (format "select keyval, dataval from ~a ~a" tab where))
    (define (%make-where-clause keys include-removed-object?)
      (let* ((keys-cond (%make-keys-condition keys))
	     (removed-cond (%make-removed-condition include-removed-object?)))
	(if (or keys-cond removed-cond)
	    (with-output-to-string
	      (lambda ()
		(display " where ")
		(when keys-cond
		  (display keys-cond)
		  (when removed-cond
		    (display " and ")))
		(when removed-cond
		  (display removed-cond))))
	    "")))
    (define (%make-keys-condition keys)
      (cond ((not keys) #f)
	    ((null? keys) "keyval is NULL")
	    (else (format "keyval in (~a)" (string-join (map (lambda _ "?") keys) ",")))))
    (define (%make-removed-condition include-removed-object?)
      (if include-removed-object?
	  #f
	  "removed = 0"))
    (define (%find-kahua-instance row)
      (let1 k (dbi-get-value row 0)
	(or (hash-table-get icache (cons cn k) #f)
	    (let1 v (call-with-input-string (dbi-get-value row 1) read)
	      (set! (ref v '%floating-instance) #f)
	      v))))
    (or (and-let* ((tab (kahua-class->table-name db class))
		   (r (apply dbi-do conn (%select-instances tab (%make-where-clause keys include-removed-object?))
			     '() (or keys '()))))
	  (filter-map1 (lambda (row)
			 (and-let* ((obj (%find-kahua-instance row))
				    ((or include-removed-object?
					 (not (removed? obj)))))
			   (filter-proc obj)))
		       r))
	'())))

(define-method write-kahua-instance ((db <kahua-db-dbi>)
				     (obj <kahua-persistent-base>))
  ;; You must override the method below.
  (write-kahua-instance db obj (kahua-class->table-name* db (class-of obj))))

(define-generic table-should-be-locked?)

(define-method write-kahua-modified-instances ((db <kahua-db-dbi>))
  (and-let* ((obj&table (map (lambda (obj)
			       (list obj (kahua-class->table-name* db (class-of obj))))
			     (reverse! (modified-instances-of db))))
	     ((not (null? obj&table)))
	     (tables (append! (filter-map (lambda (e)
					    (and (table-should-be-locked? db (car e))
						 (cadr e)))
					  obj&table)
			      (map! (cut cons <> :read) (hash-table-values (table-map-of db))))))
    (with-dbi-transaction db
      (lambda _
	(apply with-locking-tables db
	       (lambda ()
		 (for-each (pa$ apply write-kahua-instance db) obj&table))
	       tables)))))

(define-method add-column-to-table ((db <kahua-db-dbi>)
				    (table <string>)
				    (colname <string>)
				    (type <symbol>)
				    . args)
  (let-keywords* args
      ((nullable? #t)
       (default   #f))
    (let1 conn (connection-of)
      (dbi-do conn
	      (with-output-to-string
		(lambda ()
		  (display "ALTER TABLE ")
		  (display table)
		  (display " ADD ")
		  (display colname)
		  (write-char #\space)
		  (display type)
		  (unless nullable? (display " NOT NULL"))
		  (when default
		    (cond ((string? default) (format #t " DEFAULT '~a'" (dbi-escape-sql conn default)))
			  ((number? default) (format #t " DEFAULT ~d" default))
			  ((eq? :NULL default) (display " DEFAULT NULL"))))))
	      '(:pass-through #t)))))

(define-method drop-column-from-table ((db <kahua-db-dbi>)
				       (table <string>)
				       (colname <string>))
  (dbi-do (connection-of db) (format "alter table ~a drop ~a" table colname) '(pass-through #t)))

(define-method add-index-to-table ((db <kahua-db-dbi>)
				   (table <string>)
				   (idx-name <string>)
				   (unique?  <boolean>)
				   . cols)
  (unless (null? cols)
    (let1 sql (format "create ~a index ~a on ~a (~a)"
		      (if unique? 'unique "") idx-name table
		      (string-join cols ","))
      (dbi-do (connection-of db) sql '(:pass-through #t)))))
(define-method drop-index-from-table ((db <kahua-db-dbi>)
				      (table <string>)
				      (idx-name <string>))
  (dbi-do (connection-of db) (format "drop index ~a on ~a" idx-name table)))

;;=================================================================
;; Database Consistency Check and Fix
;;

(define-method max-table-name-suffix ((db <kahua-db-dbi>))
  (let* ((conn (connection-of db))
	 (r (dbi-do conn "select table_name from kahua_db_classes" '())))
    (apply max (map (lambda (row)
		      (rxmatch-case (dbi-get-value row 0)
			(#/^kahua_(\d+)$/ (#f d) (x->integer d))
			(else -1)))
		    r))))

(define-method dbutil:current-kahua-db-classcount ((db <kahua-db-dbi>))
  (x->integer (car (map (cut dbi-get-value <> 0)
			(dbi-do (connection-of db) "select value from kahua_db_classcount" '())))))

(define-method dbutil:fix-kahua-db-classcount ((db <kahua-db-dbi>) n)
  (dbi-do (connection-of db) "update kahua_db_classcount set value=?" '() n))

(define-method dbutil:create-kahua-db-classcount ((db <kahua-db-dbi>) n)
  (let1 conn (connection-of db)
    (guard (e ((<dbi-exception> e) #t))
      (create-kahua-db-classcount db))
    (initialize-kahua-db-classcount db n)))

(define-method dbutil:check-kahua-db-classcount ((db <kahua-db-dbi>) . maybe-do-fix?)
  (call/cc (lambda (ret)
	     (let* ((do-fix? (get-optional maybe-do-fix? #f))
		    (max-suffix (max-table-name-suffix db))
		    (classcount (guard (e ((<dbi-exception> e)
					   (cond (do-fix?
						  (dbutil:create-kahua-db-classcount db max-suffix)
						  (ret 'FIXED))
						 (ret 'NG))))
				  (dbutil:current-kahua-db-classcount db))))
	       (or (and (>= classcount max-suffix) 'OK)
		   (and do-fix?
			(dbutil:fix-kahua-db-classcount db max-suffix)
			'FIXED)
		   'NG)))))

(define-method load-all-kahua-tables ((db <kahua-db-dbi>) ht)
  (define-method enumerate-kahua-class-table ((db <kahua-db-dbi>))
    (map (lambda (row)
	   (list (dbi-get-value row 0) (dbi-get-value row 1)))
	 (dbi-do (connection-of db) "select class_name, table_name from kahua_db_classes" '())))
  (define-method load-kahua-table ((db <kahua-db-dbi>) ht class table)
    (let1 class-sym (string->symbol class)
      (for-each (lambda (row)
		  (hash-table-push! ht (dbi-get-value row 0) (cons class-sym (dbi-get-value row 1))))
		(dbi-do (connection-of db)
			(format "select keyval, dataval from ~a" table) '()))))
  (for-each (lambda (class&table)
	      (apply load-kahua-table db ht class&table))
	    (enumerate-kahua-class-table db))
  ht)

(define-method max-kahua-key-from-idcount ((db <kahua-db-dbi>))
  (let1 ht (load-all-kahua-tables db (make-hash-table 'equal?))
    (hash-table-fold ht (lambda (k v r)
			  (rxmatch-case k
			    (#/^\d+$/ (d) (max (x->integer d) r))
			    (else         r)))
		     -1)))

(define-method dbutil:current-kahua-db-idcount ((db <kahua-db-dbi>))
  (x->integer (car (map (cut dbi-get-value <> 0)
			(dbi-do (connection-of db) "select value from kahua_db_idcount" '())))))

(define-method dbutil:fix-kahua-db-idcount ((db <kahua-db-dbi>) n)
  (dbi-do (connection-of db) "update kahua_db_idcount set value = ?" '() n))

(define-method dbutil:create-kahua-db-idcount ((db <kahua-db-dbi>) n)
  (let1 conn (connection-of db)
    (safe-execute (cut create-kahua-db-idcount db))
    (initialize-kahua-db-idcount db n)))

(define-method dbutil:check-kahua-db-idcount ((db <kahua-db-dbi>) . maybe-do-fix?)
  (call/cc (lambda (ret)
	     (let* ((do-fix? (get-optional maybe-do-fix? #f))
		    (max-id (max-kahua-key-from-idcount db))
		    (idcount (guard (e ((<dbi-exception> e)
					(cond (do-fix?
					       (dbutil:create-kahua-db-idcount db max-id)
					       (ret 'FIXED))
					      (else (ret 'NG)))))
			       (dbutil:current-kahua-db-idcount db))))
	       (or (and (>= idcount max-id) 'OK)
		   (and do-fix?
			(dbutil:fix-kahua-db-idcount db max-id)
			'FIXED)
		   'NG)))))

(define-method dbutil:add-removed-flag-column ((db <kahua-db-dbi>)
					       (tabname <string>))
  (dbi-do (connection-of db)
	  (format "alter table ~a add removed smallint not null default 0" tabname)
	  '(:pass-through #t)))

(define-method dbutil:check-removed-flag-column ((db <kahua-db-dbi>)
						 (tabname <string>) . maybe-do-fix?)
  (let1 do-fix? (get-optional maybe-do-fix? #f)
    (if (guard (e (else #f))
	  (map identity (dbi-do (connection-of db)
				(format "select removed from ~a where keyval = NULL" tabname)))
	  #t)
	'OK
	(if do-fix?
	    (and (dbutil:add-removed-flag-column db tabname) 'FIXED)
	    'NG))))

(define-method dbutil:check-removed-flag-column-for-all-tables ((db <kahua-db-dbi>) . maybe-do-fix?)
  (let1 do-fix? (get-optional maybe-do-fix? #f)
    (hash-table-map (table-map-of db)
      (lambda (k v)
	(list k v (dbutil:check-removed-flag-column db v do-fix?))))))

(provide "kahua/persistence/dbi")
