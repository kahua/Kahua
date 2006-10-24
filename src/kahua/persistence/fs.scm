;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on File System storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: fs.scm,v 1.14 2006/10/24 06:14:53 bizenn Exp $

(define-module kahua.persistence.fs
  (use srfi-1)
  (use srfi-13)
  (use file.util)
  (use gauche.fcntl)
  (use gauche.collection)
  (use gauche.charconv)
  (use gauche.logger)
  (use kahua.persistence)
  (use kahua.util))

(select-module kahua.persistence.fs)

;; Physical Database Structure
;;
;; path-to-db/                                 : root of database
;; path-to-db/id-counter                       : file holding object ID counter value
;; path-to-db/%%character-encoding             : file holding character encoding
;; path-to-db/kahua-persistent-metainfo/       : <kahua-persistent-metainfo>
;; path-to-db/kahua-user/                      : <kahua-user> (but optional)
;; path-to-db/tmp/                             : temporary directory for update safely

;; filesystem-based persistent store (default)

(define-constant *alive* "%%alive")
(define-constant *key*   "%%key")

(define mk-dbdir (cut make-directory* <> #o770))

(define-class <kahua-db-fs> (<kahua-db>)
  ((id-counter :init-value 0 :accessor id-counter-of)
   (character-encoding :init-value (gauche-character-encoding) :getter character-encoding-of)
   (lock-port  :init-value #f :accessor lock-port-of) ;; port opened on lock file
   (mode :init-value :readwrite :accessor mode-of) ;; :readwrite or :readonly
   (real-path                 :getter real-path-of)
   (id-counter-path           :getter id-counter-path-of)
   (character-encoding-path   :getter character-encoding-path-of)
   (lock-path                 :getter lock-path-of)
   (tmp-path                  :getter tmp-path-of)
   ))

(define-method initialize ((db <kahua-db-fs>) initargs)
  (define (build-real-path db-path)
    (cond ((#/^fs:/ db-path) => (lambda (m) (rxmatch-after m)))
	  (else db-path)))
  (define (build-lock-path db-path)
    (build-path (sys-dirname db-path) (string-append (sys-basename db-path) ".lock")))
  (define (build-id-counter-path path)
    (build-path path "id-counter"))
  (define (build-character-encoding-path path)
    (build-path path "%%character-encoding"))
  (define (build-tmp-path path)
    (build-path path "tmp"))

  (next-method)
  (slot-set! db 'real-path (build-real-path (path-of db)))
  (let1 path (real-path-of db)
    (slot-set! db 'lock-path (build-lock-path path))
    (slot-set! db 'id-counter-path (build-id-counter-path path))
    (slot-set! db 'character-encoding-path (build-character-encoding-path path))
    (slot-set! db 'tmp-path (build-tmp-path path))))

(define-method kahua-db-unique-id ((db <kahua-db-fs>))
  (begin0
    (id-counter-of db)
    (inc! (id-counter-of db))))

;; write id-counter or kahua-instance into kahua-db-fs safely.
(define (%call-writer-to-file-safely file tmpbase writer encoding)
  (receive (out tmp) (sys-mkstemp tmpbase)
    (let1 out (if encoding
		  (wrap-with-output-conversion out encoding)
		  out)
      (guard (e ((else (sys-unlink tmp) (raise e))))
	(writer out)
	(close-output-port out)
	(sys-rename tmp file)))))

(define-constant *lock-db-fs* (make <sys-flock> :type F_WRLCK))
(define-constant *unlock-db-fs* (make <sys-flock> :type F_UNLCK))
(define-method lock-db ((db <kahua-db-fs>))
  (let1 lock-file (lock-path-of db)
    (let1 lock-port (open-output-file lock-file :if-exists? :append)
      (define (try-lock retry)
        (cond ((zero? retry) #f)
              ((sys-fcntl lock-port F_SETLK *lock-db-fs*)
               (set! (lock-port-of db) lock-port)
	       #t)
              (else (sys-sleep 1) (try-lock (- retry 1)))))
      (try-lock 10))))
(define-method unlock-db ((db <kahua-db-fs>))
  (and-let* ((lock-port (lock-port-of db)))
    (sys-fcntl lock-port F_SETLK *unlock-db-fs*)
    (close-output-port lock-port)
    (set! (lock-port-of db) #f)
    #t))

(define (with-locking-output-file out thunk)
  (dynamic-wind
      (cut sys-fcntl out F_SETLKW *lock-db-fs*)
      thunk
      (cut sys-fcntl out F_SETLK *unlock-db-fs*)))

(define-method kahua-db-create ((db <kahua-db-fs>))
  ;; There could be a race condition here, but it would be very
  ;; low prob., so for now it should be OK.
  (let1 tmp (tmp-path-of db)
    (mk-dbdir tmp)
    (with-output-to-file (id-counter-path-of db) (cut write (id-counter-of db))
			 :if-exists :error)
    (with-output-to-file (character-encoding-path-of db) (cut write (character-encoding-of db))
			 :if-exists :error))
  db)

(define-method kahua-db-open ((db <kahua-db-fs>))
  (define (read-character-encoding db)
    (let1 cefile (character-encoding-path-of db)
      (if (file-is-regular? cefile)
	  (let1 ce (with-input-from-file cefile read)
	    (unless (symbol? ce)
	      (error "kahua-db-open: symbol required but got as character-encoding: " ce))
	    (unless (ces-upper-compatible? (gauche-character-encoding) ce)
	      (log-format "DB character encoding ~a differ from native ~a" ce (gauche-character-encoding))
	      (log-format "You should convert it into native encoding ~a" (gauche-character-encoding)))
	    ce)
	  (let1 ce (gauche-character-encoding)
	    (with-output-to-file cefile (cut write ce))
	    ce))))

  (unless (lock-db db)
    (error "kahua-db-open: couldn't obtain database lock: " db))
  (if (file-is-directory? (real-path-of db))
      (slot-set! db 'character-encoding (read-character-encoding db))
      (kahua-db-create db))
  (set! (active? db) #t)
  (unlock-db db)
  db)

(define-method kahua-db-close ((db <kahua-db-fs>) commit?)
  (set! (active? db) #f))

(define-method kahua-db-reopen ((db <kahua-db-fs>))
  db)

(define-method kahua-db-ping ((db <kahua-db-fs>))
  #t)

(define-method start-kahua-db-transaction ((db <kahua-db-fs>))
  (define (read-id-counter db)
    (let1 cnt (with-input-from-file (id-counter-path-of db) read)
      (unless (number? cnt)
	(error "kahua-db-open: number required but got as id-counter: " cnt))
      cnt))

  (next-method)
  (set! (id-counter-of db) (read-id-counter db)))

(define-method finish-kahua-db-transaction ((db <kahua-db-fs>) commit?)
  (if commit?
      (kahua-db-sync db)
      (kahua-db-rollback db))
  (next-method))

(define (class-name->path-component cn)
  (string-trim-both (symbol->string cn) #[<>]))

(define (data-path db cn . key)
  (apply build-path (real-path-of db)
	 (class-name->path-component cn) key))

(define (alive-path db cn . key)
  (apply build-path (data-path db cn) *alive* key))

(define (create-alive-directory* db class)
  (let1 alive-path (alive-path db (class-name class))
    (unless (file-is-directory? alive-path)
      (mk-dbdir alive-path)
      (let1 c (make-kahua-collection db class '(:include-removed-object? #t))
	(for-each (lambda (i)
		    (unless (removed? i)
		      (let1 k (key-of i)
			(sys-symlink (build-path ".." k) (build-path alive-path k)))))
		  c)))))

(define (create-class-directory* db class)
  (let1 class-path (data-path db (class-name class))
    (unless (file-is-directory? class-path)
      (mk-dbdir class-path)
      (create-alive-directory* db class))))

(define (maintain-alive-link db obj)
  (let* ((class (class-of obj))
	 (key (key-of obj))
	 (link-path (alive-path db (class-name class) key)))
    (if (removed? obj)
	(sys-unlink link-path)
	(unless (file-exists? link-path)
	  (sys-symlink (build-path ".." key) link-path)))))

(define-method read-kahua-instance ((db <kahua-db-fs>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>) . opts)
  (let1 path (data-path db (class-name class) key)
    (and (file-exists? path)
         (call-with-input-file path
	   (lambda (in)
	     (with-port-locking in (cut read in)))
	   :encoding (character-encoding-of db)))))

(define-method write-kahua-instance ((db <kahua-db-fs>)
                                     (obj <kahua-persistent-base>))
  (create-class-directory* db (class-of obj))
  (let* ((file-path (data-path db (class-name (class-of obj)) (key-of obj)))
	 (writer (lambda (out)
		   (with-port-locking out (cut kahua-write obj out)))))
    (if (ref obj '%floating-instance)
	(guard (e (else (error <kahua-persistence-error>
			       :message (format "duplicate key: ~s" (key-of obj)))))
	  (call-with-output-file file-path
	    writer
	    :if-exists :error
	    :encoding (character-encoding-of db)))
	(%call-writer-to-file-safely file-path
				     (tmp-path-of db)
				     writer
				     (character-encoding-of db)))
    (maintain-alive-link db obj)
    (set! (ref obj '%floating-instance) #f)))

(define-method kahua-db-write-id-counter ((db <kahua-db-fs>))
  (%call-writer-to-file-safely (id-counter-path-of db)
			       (tmp-path-of db)
			       (pa$ write (id-counter-of db)) #f))


(define-method kahua-persistent-instances ((db <kahua-db-fs>) class keys filter-proc include-removed-object?)
  (let* ((cn (class-name class))
	 (icache (ref db 'instance-by-key))
	 (target-dir (if include-removed-object?
			 (data-path db cn)
			 (data-path db cn *alive*)))
	 (dir-filter (if include-removed-object?
			 file-is-regular?
			 file-is-symlink?)))
    (filter-map (lambda (k)
		  (and-let* ((obj (find-kahua-instance class k include-removed-object?)))
		    (filter-proc obj)))
		(or keys
		    (if (file-is-directory? target-dir)
			(directory-list target-dir :children? #t
					:filter dir-filter :filter-add-path? #t)
			'())))))

;;=================================================================
;; Database Consistency Check and Fix
;;

(define-method dbutil:instance-files-fold ((db <kahua-db-fs>) class-name proc knil)
  (fold proc knil
	(directory-list (data-path db class-name) :add-path? #t :children? #t
			:filter file-is-regular? :filter-add-path? #t)))

;; Sweep all instances' object id and compare max of them with id-counter
;;
(define-method dbutil:check-id-counter ((db <kahua-db-fs>) do-fix?)
  (let* ((ce (character-encoding-of db))
	 (max-id (dbutil:persistent-classes-fold
		  db (lambda (cn r)
		       (dbutil:instance-files-fold
			db cn (lambda (p r)
				(max r (ref (with-input-from-file p read :encoding ce) 'id)))
			r))
		  0)))
    (cond ((> (id-counter-of db) max-id)                  'OK)
	  (do-fix? (set! (id-counter-of db) (+ max-id 1)) 'FIXED)
	  (else                                           'NG))))

;; Sweep all instances data as raw string, and check each data character
;; encoding is match with (character-encoding-of db).
;;
(define-method dbutil:check-character-encoding ((db <kahua-db-fs>) do-fix?)
  (define (string-compatible? bs ces1 ces2)
    (guard (e (else #f))
      (let ((s1 (ces-convert bs ces1))
	    (s2 (ces-convert bs ces2)))
	(string=? s1 s2))))
  (define (convert p bs from to)
    (cond (do-fix?
	   (%call-writer-to-file-safely p (tmp-path-of db)
					(pa$ display (ces-convert bs from))
					to)
	   'FIXED)
	  (else 'NG)))
  (let1 ce (character-encoding-of db)
    (dbutil:persistent-classes-fold
     db (lambda (cn r)
	  (dbutil:instance-files-fold
	   db cn (lambda (p r)
		   (let* ((s (sys-stat p))
			  (size (ref s 'size))
			  (data (with-input-from-file p (cut read-block size)))
			  (gce (ces-guess-from-string data "*JP")))
		     (if (or (ces-equivalent? ce gce)
			     (string-compatible? data ce gce))
			 r
			 (convert p data gce ce))))
	   r))
     'OK)))

;; Check alive directory (%%alive under each class directory) and removed flag of
;; all instances.
;;
(define-method dbutil:check-removed-flag-facility ((db <kahua-db-fs>) do-fix?)
  (define (%fix-alive-path apath removed? r)
    (cond (removed?
	   (if (file-exists? apath)
	       (cond (do-fix? (sys-remove apath) 'FIXED)
		     (else                       'NG))
	       r))
	  ((file-is-symlink? apath) r)
	  (do-fix? (sys-symlink (build-path ".." (sys-basename apath)) apath) 'FIXED)
	  (else                                                               'NG)))
  (define (%fix-alive-directory adir)
    (and do-fix? (mk-dbdir adir) 'FIXED))
  (let1 ce (character-encoding-of db)
    (dbutil:persistent-classes-fold
     db (lambda (cn r)
	  (let1 adir (alive-path db cn)
	    (if (or (file-is-directory? adir)
		    (%fix-alive-directory adir))
		(dbutil:instance-files-fold
		 db cn (lambda (p r)
			 (let* ((i (with-input-from-file p read :encoding ce))
				(removed? (ref i 'removed?))
				(apath (alive-path db cn (sys-basename p))))
			   (%fix-alive-path apath removed? r)))
		 r)
		'NG)))
     'OK)))

(define-constant *proc-table*
  `((,dbutil:check-character-encoding . "Database character encoding... ")
    (,dbutil:check-id-counter         . "Database ID counter... ")
    (,dbutil:check-removed-flag-facility . "Database removed flags... ")))

(define-method dbutil:check&fix-database ((db <kahua-db-fs>) writer do-fix?)
  (for-each (lambda (e)
	      (let ((do-check (car e))
		    (msg-prefix (cdr e)))
		(writer msg-prefix)
		(writer (do-check db do-fix?))
		(writer "\n")))
	    *proc-table*))

(provide "kahua/persistence/fs")
