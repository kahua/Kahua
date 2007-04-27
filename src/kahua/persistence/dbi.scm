;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on DBI abstract storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: dbi.scm,v 1.16.2.3 2007/02/21 07:12:11 bizenn Exp $

(define-module kahua.persistence.dbi
  (use srfi-1)
  (use kahua.util)
  (extend kahua.persistence
	  dbi util.list
	  gauche.collection
	  gauche.parameter
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

	  kahua-db-dbi-build-dsn
	  initialize-kahua-db-idcount
	  initialize-kahua-db-classcount
	  ;; Database Consistency Check and Fix.
	  dbutil:current-kahua-db-classcount
	  dbutil:fix-kahua-db-classcount
	  dbutil:create-kahua-db-classcount
	  dbutil:current-kahua-db-idcount
	  dbutil:fix-kahua-db-idcount
	  dbutil:create-kahua-db-idcount
	  dbutil:fix-instance-table-structure
	  ;; Utility
	  safe-execute

	  ;; Index slot handling
	  slot-name->column-name
	  create-index-column
	  change-index-type
	  drop-index-column

	  add-column-to-table
	  drop-column-from-table
	  add-index-to-table
	  drop-index-from-table
	  ))

(select-module kahua.persistence.dbi)

(define-condition-type <kahua-db-dbi-error> <kahua-db-error> kahua-db-dbi-error?)
(define (kahua-db-dbi-error fmt . args)
  (apply errorf <kahua-db-dbi-error> fmt args))

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
  (define (kahua-db-dbi-parse-path path)
    (rxmatch-if (#/(.*?):(?:([^:]+)(?::([^:]*)(?::(.*))?)?)?/ path)
	(#f driver user password options)
      (values driver (or user "") (or password "") (or options ""))
      (values #f #f #f #f)))

  (next-method)
  (unless (dsn-of db)
    (receive (d u p o) (kahua-db-dbi-parse-path (path-of db))
      (unless d (errorf "unsupported database driver path: ~a" (path-of db)))
      (set! (dsn-of db) (kahua-db-dbi-build-dsn db d o))
      (set! (user-of db) u)
      (set! (password-of db) p)
      (log-format "DBI(~a): user ~a" (dsn-of db) u))))

(define-method kahua-db-dbi-build-dsn ((db <kahua-db-dbi>) driver options)
  (format "dbi:~a:~a" driver options))

(define transactional? (make-parameter #t))
(define-method with-dbi-transaction ((db <kahua-db-dbi>) proc)
  (define (do-start-transaction conn)
    (when (transactional?)
      (dbi-do conn "start transaction" '(:pass-through #t))))
  (define (do-commit conn)
    (when (transactional?)
      (dbi-do conn "commit" '(:pass-through #t))))
  (define (do-rollback conn)
    (when (transactional?)
      (dbi-do conn "rollback" '(:pass-through #t))))
  (let1 conn (connection-of db)
    (guard (e (else
	       (do-rollback conn)
	       (raise e)))
      (do-start-transaction conn)
      (begin0
	(parameterize ((transactional? #f))
	  (proc conn))
	(do-commit conn)))))

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
		     '(:pass-through #t))))

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
					(cn <symbol>))
  (or (get-from-table-map db cn)
      (and-let* ((tabname (select-kahua-db-classes db cn)))
	(register-to-table-map db cn tabname)
	tabname)))

(define-method kahua-class->table-name ((db <kahua-db-dbi>)
					(class <kahua-persistent-meta>))
  (kahua-class->table-name db (class-name class)))

;; If the class does not exist, create the table of this class.
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

(define-method read-kahua-instance ((db <kahua-db-dbi>) (class <kahua-persistent-meta>)
				    (id <integer>))
  (define (query tab)
    (format "select dataval from ~a where id=?" tab))
  (and-let* ((conn (connection-of db))
	     (tab (kahua-class->table-name db class))
	     (r (dbi-do conn (query tab) '() id))
	     (rv (map (cut dbi-get-value <> 0) r))
	     ((not (null? rv))))
    (read-from-string (car rv))))

(define-method read-kahua-instance ((db <kahua-db-dbi>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>) . may-be-include-removed-object?)
  (define (query tab)
    (format "select dataval from ~a where keyval=?" tab))
  (define (query-removed tab)
    (format "select dataval from ~a where removed>0" tab))
  (and-let* ((conn (connection-of db))
	     (tab (kahua-class->table-name db class)))
    (or (and-let* ((r (dbi-do conn (query tab) '() key))
		   (rv  (map (cut dbi-get-value <> 0) r))
		   ((not (null? rv))))
	  (read-from-string (car rv)))
	(and (get-optional may-be-include-removed-object? #f)
	     (and-let* ((r (dbi-do conn (query-removed tab) '()))
			(rv (map (cut dbi-get-value <> 0) r))
			((not (null? rv))))
	       (let/cc ret
		 (for-each (lambda (s)
			     (let1 o (read-from-string s)
			       (and (equal? key (key-of o)) (ret o))))
			   rv)
		 #f))))))

(define-method kahua-persistent-instances ((db <kahua-db-dbi>) class opts . may-be-sweep?)
  (define (%select-instances tabname where-clause)
    (format "select id, dataval from ~a ~a" tabname where-clause))
  (define (%make-where-clause index keys)
    (let1 conditions `(,(%make-index-condition index)
		       ,(%make-keys-condition keys)
		       "removed=0")
      (format "where ~a"
	      (string-join (filter identity conditions) " and "))))
  (define (%make-keys-condition keys)
    (and keys
	 (if (null? keys) "keyval is NULL"
	     (format "keyval in (~a)" (string-join (map (lambda _ "?") keys) ",")))))
  (define (%make-index-condition index)
    (and index
	 (format "~a=?" (slot-name->column-name (car index)))))
  (define (%make-sql-parameters index keys)
    (let1 keys (or keys '())
      (if index
	  (cons (with-output-to-string
		  (cut index-value-write (cdr index))) keys)
	  keys)))
  (define (%find-kahua-instance row filter-proc)
    (and-let* ((id (x->integer (dbi-get-value row 0)))
	       ((not (read-id-cache db id)))
	       (obj (read-from-string (dbi-get-value row 1))))
      (filter-proc obj)))

  ;; main
  (let-keywords* opts ((index #f)
		       (keys #f)
		       (predicate #f)
		       (include-removed-object? #f)
		       (subclasses #f)	; ignore(to avoid WARNING)
		       )
    (or (and-let* ((tab (kahua-class->table-name db class))
		   (conn (connection-of db)))
	  (receive (filter-proc res)
	      (cond ((or include-removed-object? (and index (get-optional may-be-sweep? #f)))
		     (values (make-kahua-collection-filter class opts)
			     (dbi-do conn (%select-instances tab "") '())))
		    (else
		     (values (make-kahua-collection-filter class `(:predicate ,predicate))
			     (apply dbi-do conn (%select-instances tab (%make-where-clause index keys))
				    '() (%make-sql-parameters index keys)))))
	    (filter-map1 (cut %find-kahua-instance <> filter-proc) res)))
	'())))

(define-method write-kahua-instance ((db <kahua-db-dbi>)
				     (obj <kahua-persistent-base>))
  (write-kahua-instance db obj (kahua-class->table-name* db (class-of obj))))

(define-method write-kahua-instance ((db <kahua-db-dbi>)
				     (obj <kahua-persistent-base>)
				     (tabname <string>))
  (let* ((conn (connection-of db))
	 (class (class-of obj))
	 (islots (filter-map (lambda (s)
			       (and (slot-definition-option s :index #f)
				    (slot-definition-name s)))
			     (class-slots class)))
	 (id (kahua-persistent-id obj))
	 (data  (call-with-output-string (pa$ kahua-write obj))))
    (define (dbi-do-insert-instance)
      (let1 icolumns (map slot-name->column-name islots)
	(if (removed? obj)
	    (dbi-do conn
		    (format "insert into ~a (id,keyval,dataval,removed~a) values (?,NULL,?,1~a)"
			    tabname
			    (string-join icolumns "," 'prefix)
			    (string-join (map (lambda _ "NULL") icolumns) "," 'prefix))
		    '() id data)
	    (let* ((ivalues (map (lambda (sn)
				   (with-output-to-string
				     (lambda ()
				       (index-value-write (slot-ref obj sn))))) islots))
		   (insert-sql (format "insert into ~a (id,keyval,dataval,removed~a) values (?,?,?,0~a)"
				       tabname (string-join icolumns "," 'prefix)
				       (string-join (map (lambda _ "?") icolumns) "," 'prefix))))
	      (apply dbi-do conn insert-sql '() id (key-of obj) data ivalues)))))
    (define (dbi-do-update-instance)
      (let* ((icolumns (map (lambda (sn)
			      (format "~a=?" (slot-name->column-name sn))) islots))
	     (ivalues (map (lambda (sn)
			     (with-output-to-string
			       (lambda ()
				 (index-value-write (slot-ref obj sn))))) islots))
	     (update-sql (format "update ~a set keyval=?,dataval=?,removed=0~a where id=~d"
				 tabname (string-join icolumns "," 'prefix) id)))
	(apply dbi-do conn update-sql '() (key-of obj) data ivalues)))
    (define (dbi-do-remove-instance)
      (let* ((icolumns (map (lambda (sn)
			      (format "~a=NULL" (slot-name->column-name sn))) islots))
	     (remove-sql (format "update ~a set ~keyval=NULL,removed=1,dataval=?~a where id=?"
				 tabname (string-join icolumns "," 'prefix))))
	(dbi-do conn remove-sql '() data (kahua-persistent-id obj))))

    (cond ((floating-instance? obj) (dbi-do-insert-instance))
	  ((removed? obj)           (dbi-do-remove-instance))
	  (else                     (dbi-do-update-instance)))
    (touch-down-instance! obj)
    (set! (ref obj '%modified-index-slots) '())))

(define-generic table-should-be-locked?)

(define-method write-kahua-modified-instances ((db <kahua-db-dbi>))
  (define (drop-old-index-value conn obj table)
    (let ((columns (filter-map (lambda (i)
				 (case (list-ref i 1)
				   ((:drop :modify)
				    (format "~a=NULL" (slot-name->column-name (list-ref i 0))))
				   (else #f)))
			       (ref obj '%modified-index-slots))))
      (unless (null? columns)
	(dbi-do conn (format "update ~a set ~a where id=~d"
			     table (string-join columns ",") (kahua-persistent-id obj))
		'(:pass-through #t)))))

  (and-let* ((obj&table (map (lambda (obj)
			       (list obj (kahua-class->table-name* db (class-of obj))))
			     (reverse! (modified-instances-of db))))
	     ((not (null? obj&table)))
	     (tables (append! (filter-map (lambda (e)
					    (and (table-should-be-locked? db (car e))
						 (cadr e)))
					  obj&table)
			      (map! (cut cons <> :read) (hash-table-values (table-map-of db))))))
    (receive (metainfo&table obj&table)
	(partition (lambda (o&t) (is-a? (car o&t) <kahua-persistent-metainfo>)) obj&table)
      (with-dbi-transaction db
	(lambda _
	  (apply with-locking-tables db
		 (lambda ()
		   (for-each (lambda (m&t)
			       (let ((m (car m&t))
				     (t (cadr m&t)))
				 (kahua-update-index! db m)
				 (write-kahua-instance db m t))) metainfo&table)
		   (for-each (pa$ apply drop-old-index-value (connection-of db)) obj&table)
		   (for-each (pa$ apply write-kahua-instance db) obj&table))
		 tables))))
    ))

(define (slot-name->column-name slot-name)
  (with-string-io (symbol->string slot-name)
    (lambda ()
      (port-for-each (lambda (b)
		       (cond ((char-set-contains? #[0-9a-zA-Z] (integer->char b))
			      (write-byte b))
			     (else (format #t "_~2,'0x" b))))
		     read-byte))))

(define-generic create-index-column)
(define-generic change-index-type)
(define-generic drop-index-column)
(define-method make-index-updater ((db <kahua-db-dbi>)
				   (class <kahua-persistent-meta>)
				   slot-names)
  (define (build-update-string tabname slot-names)
    (format "update ~a set ~a where id=?"
	    tabname
	    (string-join (map (lambda (sn)
				(format "~a=?" (slot-name->column-name sn)))
			      slot-names)
			 ",")))
  (let ((conn (connection-of db))
	(update (build-update-string (kahua-class->table-name* db class) slot-names)))
    (lambda (o)
      (let ((vals (map (lambda (sn)
			 (with-output-to-string
			   (lambda ()
			     (index-value-write (slot-ref o sn)))))
		       slot-names))
	    (id (kahua-persistent-id o)))
	(apply dbi-do conn update '() `(,@vals ,id))))))
(define-method kahua-interp-index-translator ((db <kahua-db-dbi>) class translator)
  (let1 slots-to-be-updated (filter-map
			     (apply$ (lambda (sn dir idx)
				       (case dir
					 ((:drop)   (drop-index-column db class sn)       #f)
					 ((:modify) (change-index-type db class sn idx)   sn)
					 ((:add)    (create-index-column db class sn idx) sn))))
			     translator)
    (unless (null? slots-to-be-updated)
      (for-each (make-index-updater db class slots-to-be-updated)
		(make-kahua-collection db class '())))))

(define-method add-column-to-table ((db <kahua-db-dbi>)
				    (table <string>)
				    (colname <string>)
				    (type <symbol>)
				    . args)
  (let-keywords* args
      ((nullable? #t)
       (default   #f))
    (let1 conn (connection-of db)
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
  (dbi-do (connection-of db) (format "alter table ~a drop ~a" table colname) '(:pass-through #t)))

(define-method add-index-to-table ((db <kahua-db-dbi>)
				   (table <string>)
				   (idx-name <string>)
				   (unique?  <boolean>)
				   . cols)
  (unless (null? cols)
    (let1 sql (format "create index ~a ~a on ~a (~a)"
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

;; Class counter(kahua_db_classcount)
;;
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

(define-method dbutil:check-class-counter ((db <kahua-db-dbi>) do-fix?)
  (define (max-class-id cn r)
    (or (and-let* ((tabname (kahua-class->table-name db cn))
		   (m (#/^kahua_(\d+)$/ tabname)))
	  (max r (x->integer (m 1))))
	r))
  (let/cc ret
    (let* ((maxid (dbutil:persistent-classes-fold db max-class-id -1))
	   (classcount (guard (e ((<dbi-exception> e)
				  (or (and do-fix? (dbutil:create-kahua-db-classcount db maxid) (ret 'FIXED))
				      (ret 'NG))))
			 (dbutil:current-kahua-db-classcount db))))
      (cond ((= maxid classcount) 'OK)
	    (else
	     (or (and do-fix? (dbutil:fix-kahua-db-classcount db maxid) 'FIXED)
		 'NG))))))

;; Check Max ID from all instances data.
;;
(define-method dbutil:current-kahua-db-idcount ((db <kahua-db-dbi>))
  (x->integer (car (map (cut dbi-get-value <> 0)
			(dbi-do (connection-of db) "select value from kahua_db_idcount" '())))))

(define-method dbutil:fix-kahua-db-idcount ((db <kahua-db-dbi>) n)
  (dbi-do (connection-of db) "update kahua_db_idcount set value = ?" '() n))

(define-method dbutil:create-kahua-db-idcount ((db <kahua-db-dbi>) n)
  (let1 conn (connection-of db)
    (safe-execute (cut create-kahua-db-idcount db))
    (initialize-kahua-db-idcount db n)))

(define-method dbutil:check-id-counter ((db <kahua-db-dbi>) do-fix?)
  (define (max-id cn r)
    (let ((tabname (kahua-class->table-name db cn))
	  (conn (connection-of db)))
      (fold (lambda (row r)
	      (let* ((obj (read-from-string (dbi-get-value row 0)))
		     (id (ref obj 'id)))
		(max id r)))
	    r
	    (dbi-do conn (format "select dataval from ~a" tabname) '()))))
  (let/cc ret
    (let* ((maxid (dbutil:persistent-classes-fold db max-id -1))
	   (idcount (guard (e ((<dbi-exception> e)
			       (or (and do-fix? (dbutil:create-kahua-db-idcount db (+ maxid 1)) (ret 'FIXED))
				   (ret 'NG))))
		      (dbutil:current-kahua-db-idcount db))))
      (cond ((= (+ maxid 1) idcount) 'OK)
	    (else
	     (or (and do-fix? (dbutil:fix-kahua-db-idcount db (+ maxid 1)) 'FIXED)
		 'NG))))))

;; Removed flag column (named "removed") on each class table.
;;
(define-method dbutil:check-removed-flag-facility ((db <kahua-db-dbi>) do-fix?)
  (define (check-removed-flag-column cn r)
    (let ((tabname (kahua-class->table-name db cn))
	  (conn (connection-of db)))
      (if (guard (e (else #f))
	    (map identity (dbi-do conn (format "select removed from ~a where keyval is NULL" tabname))))
	  r
	  (or (and do-fix? (add-removed-flag-column conn tabname) 'FIXED)
	      'NG))))
  (define (add-removed-flag-column conn tabname)
    (dbi-do conn (format "alter table ~a add removed smallint not null default 0" tabname)
	    '(:pass-through #t))
    (dbi-do conn (format "create index idx_rmd_~a on ~a (removed)" tabname tabname)
	    '(:pass-through #t)))
  (dbutil:persistent-classes-fold db check-removed-flag-column 'OK))

(define-generic dbutil:fix-instance-table-structure)
(define-method dbutil:check-id-column ((db <kahua-db-dbi>) do-fix?)
  (define (check-id-column cn r)
    (let ((tabname (kahua-class->table-name db cn))
	  (conn (connection-of db)))
      (if (guard (e (else #f))
	    (map identity (dbi-do conn (format "select id from ~a where keyval is NULL" tabname))))
	  r
	  (or (and do-fix? (dbutil:fix-instance-table-structure db tabname) 'FIXED)
	      'NG))))
  (dbutil:persistent-classes-fold db check-id-column 'OK))

(define-constant *proc-table*
  `((,dbutil:check-id-counter . "Checking kahua_db_idcount... ")
    (,dbutil:check-class-counter . "Checking kahua_db_classcount... ")
    (,dbutil:check-id-column . "Checking ID column and indexes... ")
    (,dbutil:check-removed-flag-facility . "Checking removed flags... ")
    ))

(define-method dbutil:check&fix-database ((db <kahua-db-dbi>) writer do-fix?)
  (for-each (lambda (e)
	      (let ((do-check (car e))
		    (msg-prefix (cdr e)))
		(writer msg-prefix)
		(writer (do-check db do-fix?))
		(writer "\n")))
	    *proc-table*)
  )

(provide "kahua/persistence/dbi")
