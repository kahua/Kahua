;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on MySQL storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: mysql.scm,v 1.10.2.2 2007/05/23 16:03:02 bizenn Exp $

(define-module kahua.persistence.mysql
  (use srfi-1)
  (use kahua.persistence.dbi))

(select-module kahua.persistence.mysql)

(define-class <kahua-db-mysql> (<kahua-db-dbi>)
  ;; Now support :MyISAM and :InnoDB
  ((table-type :init-keyword :table-type :init-value :MyISAM :getter table-type-of)))

(define-method set-default-character-encoding! ((db <kahua-db-mysql>))
  (safe-execute
   (lambda ()
     (dbi-do (connection-of db)
	     (format "set character set ~a" (case (gauche-character-encoding)
					      ((utf-8)  'utf8)
					      ((euc-jp) 'ujis)
					      ((sjis)   'sjis)
					      (else     'binary)))
	     '(:pass-through #t)))))

(define-constant *db-charset-collation*
  (case (gauche-character-encoding)
    ((utf-8)  '(utf8   utf8_general_ci))
    ((euc-jp) '(ujis   ujis_japanese_ci))
    ((sjis)   '(sjis   sjis_japanese_ci))
    (else     '(binary binary))))

;; ID counter in database.
(define-method create-kahua-db-idcount ((db <kahua-db-mysql>))
  (define *create-kahua-db-idcount*
    (format "create table ~a (value integer not null) type=~a" *kahua-db-idcount* (table-type-of db)))
  (define *initialize-kahua-db-idcount*
    (format "insert into ~a values (-1)" *kahua-db-idcount*))
  (let1 conn (connection-of db)
    (dbi-do conn *create-kahua-db-idcount* '(:pass-through #t))
    (dbi-do conn *initialize-kahua-db-idcount* '(:pass-through #t))))

(define-method initialize-kahua-db-idcount ((db <kahua-db-mysql>) n)
  (dbi-do (connection-of db)
	  (format "update ~a set value = ?" *kahua-db-idcount*)
	  '() n))


;; Class table counter
(define-method create-kahua-db-classcount ((db <kahua-db-mysql>))
  (define *create-kahua-db-classcount*
    (format "create table ~a (value integer not null) type=~a" *kahua-db-classcount* (table-type-of db)))
  (define *initialize-kahua-db-classcount*
    (format "insert into ~a values (-1)" *kahua-db-classcount*))
  (let1 conn (connection-of db)
    (dbi-do conn *create-kahua-db-classcount* '(:pass-through #t))
    (dbi-do conn *initialize-kahua-db-classcount* '(:pass-through #t))))
(define-method initialize-kahua-db-classcount ((db <kahua-db-mysql>) n)
  (define *initialize-kahua-db-classcount*
    (format "update ~a set value = ?" *kahua-db-classcount*))
  (dbi-do (connection-of db) *initialize-kahua-db-classcount* '() n))

;; Class - Table mapping table.
(define-method create-kahua-db-classes ((db <kahua-db-mysql>))
  (define create-kahua-db-classes-sql
    (format "create table ~a (
               class_name varchar(255) not null,
               table_name varchar(255) not null,
               constraint pk_~a primary key (class_name),
               constraint uq_~a unique (table_name)
             ) type=~a" *kahua-db-classes* *kahua-db-classes* *kahua-db-classes* (table-type-of db)))
  (dbi-do (connection-of db) create-kahua-db-classes-sql '(:pass-through #t)))

(define-method lock-tables ((db <kahua-db-mysql>) . tables)
  (define (construct-table-lock-sql specs)
    (and (not (null? specs))
	 (call-with-output-string
	   (lambda (out)
	     (format out "lock tables ~a ~a" (caar specs) (cdar specs))
	     (for-each (lambda (spec)
			 (format out ",~a ~a" (car spec) (cdr spec)))
		       (cdr specs))))))
  (and-let* ((query (construct-table-lock-sql (serialize-table-locks tables))))
    (dbi-do (connection-of db) query '(:pass-through #t))))

(define-method unlock-tables ((db <kahua-db-mysql>) . tables)
  (unless (null? tables)
    (dbi-do (connection-of db) "unlock tables" '(:pass-through #t))))

(define-method kahua-db-unique-id ((db <kahua-db-mysql>))
  (define *select-kahua-db-idcount* "select last_insert_id()")
  (define *update-kahua-db-idcount*
    (format "update ~a set value = last_insert_id(value+1)" *kahua-db-idcount*))
  (with-dbi-transaction db
    (lambda (conn)
      (with-locking-tables db
	(lambda ()
	  (dbi-do conn *update-kahua-db-idcount* '(:pass-through #t))
	  (x->integer (car (map (cut dbi-get-value <> 0)
				(dbi-do conn *select-kahua-db-idcount* '(:pass-through #t))))))
	*kahua-db-idcount*))))

(define-method class-table-next-suffix ((db <kahua-db-mysql>))
  (define *select-kahua-db-classcount* "select last_insert_id()")
  (define *update-kahua-db-classcount*
    (format "update ~a set value = last_insert_id(value+1)" *kahua-db-classcount*))
  (guard (e (else (format (current-error-port)
			  "Error: class-table-next-suffix: ~a" (ref e 'message))))
    (with-dbi-transaction db
      (lambda (conn)
	(with-locking-tables db
	  (lambda ()
	    (dbi-do conn *update-kahua-db-classcount* '(:pass-through #t))
	    (x->integer (car (map (cut dbi-get-value <> 0)
				  (dbi-do conn *select-kahua-db-classcount* '(:pass-through #t))))))
	  *kahua-db-classcount*)))))

(define-method create-kahua-class-table ((db <kahua-db-mysql>)
					 (class <kahua-persistent-meta>))
  (define (create-class-table-sql tabname slots)
    (receive (columns indexes)
	(fold2 (lambda (s columns indexes)
		 (or (and-let* ((index (slot-definition-option s :index #f))
				(colname (slot-name->column-name (slot-definition-name s))))
		       (values (cons (format "~a longtext binary" colname) columns)
			       (case index
				 ((:unique) (cons (format "constraint unique (~a(255))" colname) indexes))
				 ((:any) (cons (format "index (~a(255))" colname) indexes))
				 (else indexes))))
		     (values columns indexes)))
	       '()
	       '()
	       slots)
      (format "
create table ~a (
 id      integer not null,
 keyval  longtext binary,
 dataval longtext binary not null,
 removed smallint not null default 0,
~a
 constraint primary key (id),
 constraint unique      (keyval(255)),
 index                  (removed)
~a
) type=~a"
	      tabname
	      (string-join columns "," 'suffix)
	      (string-join indexes "," 'prefix)
	      (table-type-of db))))
  (let ((cname (class-name class))
	(newtab (format *kahua-class-table-format* (class-table-next-suffix db))))
    (insert-kahua-db-classes db cname newtab)
    (dbi-do (connection-of db)
	    (create-class-table-sql newtab (class-slots class))
	    '(:pass-through #t))
    (register-to-table-map db cname newtab)
    newtab))

(define-method table-should-be-locked? ((db <kahua-db-mysql>)
					(obj <kahua-persistent-base>))
  #t)					; Always should lock the table.

;;
;; Index handling
;;
(define-method create-index-column ((db <kahua-db-mysql>)
				    (class <kahua-persistent-meta>)
				    slot-name index-type)
  (let ((conn (connection-of db))
	(tabname (kahua-class->table-name* db class))
	(colname (slot-name->column-name slot-name)))
    (dbi-do conn (format "alter table ~a add ~a longtext" tabname colname) '(:pass-through #t))
    (dbi-do conn (format "alter table ~a add ~a index (~a(255))"
			 tabname (if (eq? index-type :unique) "unique" "") colname)
	    '(:pass-through #t))))
(define-method change-index-type ((db <kahua-db-mysql>)
				  (class <kahua-persistent-meta>)
				  slot-name index-type)
  (let ((conn (connection-of db))
	(tabname (kahua-class->table-name* db class))
	(colname (slot-name->column-name slot-name)))
    (dbi-do conn (format "alter table ~a drop index ~a" tabname colname) '(:pass-through #t))
    (dbi-do conn (format "alter table ~a add ~a index (~a(255))"
			 tabname (if (eq? index-type :unique) "unique" "") colname)
	    '(:pass-through #t))))
(define-method drop-index-column ((db <kahua-db-mysql>)
				  (class <kahua-persistent-meta>)
				  slot-name)
  (let ((conn (connection-of db))
	(tabname (kahua-class->table-name* db class))
	(colname (slot-name->column-name slot-name)))
    (dbi-do conn (format "alter table ~a drop ~a" tabname colname) '(:pass-through #t))))

;;
;; for maintainance database structure.
;;
(define-method dbutil:fix-instance-table-structure ((db <kahua-db-mysql>) tabname)
  (define (warn tabname e) (format (current-error-port) "Fail: ~a: ~a\n" tabname (ref e 'message)))
  (define add-id-column (format "alter table ~a add id integer not null" tabname))
  (define drop-primary-key (format "alter table ~a drop primary key" tabname))
  (define modify-keyval-type (format "alter table ~a modify keyval longtext null" tabname))
  (define add-unique (format "alter table ~a add constraint unique (keyval(255))" tabname))
  (define update-table (format "update ~a set id = ? where keyval = ?" tabname))
  (define select-table (format "select keyval, dataval from ~a" tabname))
  (define set-primary-key (format "alter table ~a add constraint primary key (id)" tabname))
  (let1 conn (connection-of db)
    (for-each (lambda (stmt)
		(guard (e (else (warn tabname e)))
		  (dbi-do conn stmt '(:pass-through #t))))
	      `(,add-id-column ,drop-primary-key ,modify-keyval-type ,add-unique))
    (for-each (lambda (r)
		(let ((k (dbi-get-value r 0))
		      (o (read-from-string (dbi-get-value r 1))))
		  (dbi-do conn update-table '() (ref o 'id) k)))
	      (dbi-do conn select-table '()))
    (guard (e (else (warn tabname e) #f))
      (dbi-do conn set-primary-key '(:pass-through #t))
      #t)
    ))

(provide "kahua/persistence/mysql")
