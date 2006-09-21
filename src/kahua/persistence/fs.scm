;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on File System storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: fs.scm,v 1.9 2006/09/21 08:52:36 bizenn Exp $

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

;; filesystem-based persistent store (default)
(define-class <kahua-db-fs> (<kahua-db>)
  ((id-counter :init-value 0 :accessor id-counter-of)
   (character-encoding :init-value (gauche-character-encoding) :accessor character-encoding-of)
   (id-counter-path :accessor id-counter-path-of)
   (character-encoding-path :accessor character-encoding-path-of)
   (lock-path  :init-value #f :accessor lock-path-of)
   (lock-port  :init-value #f :accessor lock-port-of) ;; port opened on lock file
   (tmp-path   :init-value #f :accessor tmp-path-of)
   (mode :init-value :readwrite :accessor mode-of) ;; :readwrite or :readonly
   ))

(define-method initialize ((db <kahua-db-fs>) initargs)
  (define (build-lock-path db-path)
    (build-path (sys-dirname db-path) (string-append (sys-basename db-path) ".lock")))
  (define (build-id-counter-path path)
    (build-path path "id-counter"))
  (define (build-character-encoding-path path)
    (build-path path "%%character-encoding"))
  (define (build-tmp-path path)
    (build-path path "tmp"))

  (next-method)
  (let1 path (path-of db)
    (set! (lock-path-of db) (build-lock-path path))
    (set! (id-counter-path-of db) (build-id-counter-path path))
    (set! (character-encoding-path-of db) (build-character-encoding-path path))
    (set! (tmp-path-of db) (build-tmp-path path))))

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
      (with-port-locking out
	(lambda ()
	  (guard (e ((else (sys-unlink tmp) (raise e))))
	    (writer out)
	    (close-output-port out)
	    (sys-rename tmp file)))))))

(define-constant *lock-db-fs* (make <sys-flock> :type F_WRLCK))
(define-constant *unlock-db-fs* (make <sys-flock> :type F_UNLCK))
(define-method lock-db ((db <kahua-db-fs>))
  (let1 lock-file (lock-path-of db)
    (make-directory* (sys-dirname lock-file))
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

(define-method kahua-db-create ((db <kahua-db-fs>))
  ;; There could be a race condition here, but it would be very
  ;; low prob., so for now it should be OK.
  (let1 tmp (tmp-path-of db)
    (make-directory* tmp)
    (%call-writer-to-file-safely (id-counter-path-of db) tmp
				 (pa$ write (id-counter-of db)) #f)
    (%call-writer-to-file-safely (character-encoding-path-of db) tmp
				 (pa$ write (character-encoding-of db)) #f))
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
  (if (file-is-directory? (path-of db))
      (set! (character-encoding-of db) (read-character-encoding db))
      (kahua-db-create db))
  (set! (active? db) #t)
  (unlock-db db)
  db)

(define-method kahua-db-close ((db <kahua-db-fs>) commit?)
  (set! (active? db) #f))

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

(define-method read-kahua-instance ((db <kahua-db-fs>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (let1 path (data-path db class key)
    (and (file-exists? path)
         (call-with-input-file path
	   (lambda (in)
	     (with-port-locking in (cut read in)))
	   :encoding (character-encoding-of db)))))

(define-method write-kahua-instance ((db <kahua-db-fs>)
                                     (obj <kahua-persistent-base>))
  (let* ((class-path  (data-path db (class-of obj))))
    (make-directory* class-path)
    (%call-writer-to-file-safely (build-path class-path (key-of obj))
				 (tmp-path-of db)
                                 (pa$ kahua-write obj)
				 (character-encoding-of db))
    (set! (ref obj '%floating-instance) #f)))

(define-method kahua-db-write-id-counter ((db <kahua-db-fs>))
  (%call-writer-to-file-safely (id-counter-path-of db)
			       (tmp-path-of db)
			       (pa$ write (id-counter-of db)) #f))


(define (data-path db class . key)
  (apply build-path
         (path-of db)
         (string-trim-both (x->string (class-name class)) #[<>])
         key))

(define-method kahua-persistent-instances ((db <kahua-db-fs>) class keys filter-proc)
  (let ((cn (class-name class))
	(icache (ref db 'instance-by-key)))
    (filter-map (lambda (k) (filter-proc (or (hash-table-get icache (cons cn k) #f)
					     (find-kahua-instance class k))))
		(or keys
		    (if (file-is-directory? (data-path db class))
			(directory-list (data-path db class) :children? #t)
			'())))))

(provide "kahua/persistence/fs")
