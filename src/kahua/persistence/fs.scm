;;; -*- mode: scheme; coding: utf-8 -*-
;; Persistent on File System storage
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: fs.scm,v 1.3 2006/08/02 04:24:19 bizenn Exp $

(define-module kahua.persistence.fs
  (use srfi-1)
  (use srfi-13)
  (use file.util)
  (use gauche.fcntl)
  (use gauche.collection)
  (use kahua.persistence)
  (use kahua.util))

(select-module kahua.persistence.fs)

;; filesystem-based persistent store (default)
(define-class <kahua-db-fs> (<kahua-db>)
  ((id-counter :init-keyword :id-counter :init-value 0)
   (lock-port  :init-value #f :accessor lock-port-of) ;; port opened on lock file
   ))

(define-method kahua-db-unique-id ((db <kahua-db-fs>))
  (begin0
    (ref db 'id-counter)
    (inc! (ref db 'id-counter))))

(define (id-counter-path path)
  (build-path path "id-counter"))

;; write id-counter or kahua-instance into kahua-db-fs safely.
(define (%call-writer-to-file-safely file tmpbase writer)
  (receive (out tmp) (sys-mkstemp tmpbase)
    (with-error-handler
     (lambda (e) (sys-unlink tmp) (raise e))
     (lambda ()
       (writer out)
       (close-output-port out)
       (sys-rename tmp file)))))

;; lock mechanism - we need more robust one later, but just for now...
(define (lock-file-path path)
  (build-path path "lock"))

(define-constant *lock-db-fs* (make <sys-flock> :type F_WRLCK))
(define-constant *unlock-db-fs* (make <sys-flock> :type F_UNLCK))
(define-method lock-db ((db <kahua-db-fs>))
  (let1 lock-file (lock-file-path (ref db 'path))
    (unless (file-exists? lock-file)
      ;; This is an old db.  This is only transitional, and may
      ;; be called very rarely, so we just leave this though unsafe.
      (with-output-to-file lock-file (lambda () (newline))))
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

(define-method kahua-db-open ((db <kahua-db-fs>))
  (let* ((path (ref db 'path))
	 (cntfile (id-counter-path path)))
    (if (file-is-directory? path)
	(if (file-is-regular? cntfile)
	    (let1 cnt (with-input-from-file cntfile read)
	      (unless (number? cnt)
		(error "kahua-db-open: number required but got as id-counter: " cnt))
	      (set! (active? db) #t)
	      (set! (ref db 'id-counter) cnt)
	      (unless (lock-db db)
		(error "kahua-db-open: couldn't obtain database lock: " path)))
	    (error "kahua-db-open: path is not a db: " path))
	(begin
	  ;; There could be a race condition here, but it would be very
	  ;; low prob., so for now it should be OK.
	  (make-directory* path)
	  (let1 tmp-path (build-path path "tmp/")
	    (make-directory* tmp-path)
	    (%call-writer-to-file-safely cntfile tmp-path (pa$ write 0)))
	  (set! (active? db) #t)
	  (unless (lock-db db)
	    (error "kahua-db-open: couldn't obtain database lock: " path)))
	))
  db)

(define-method kahua-db-close ((db <kahua-db-fs>) commit)
  (if commit
      (kahua-db-sync db)
    (kahua-db-rollback db))
  (unlock-db db)
  (set! (ref db 'modified-instances) '())
  (set! (active? db) #f))

(define-method read-kahua-instance ((db <kahua-db-fs>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (let1 path (data-path db class key)
    (and (file-exists? path)
         (call-with-input-file path read))))

(define-method write-kahua-instance ((db <kahua-db-fs>)
                                     (obj <kahua-persistent-base>))
  (let* ((class-path  (data-path db (class-of obj))))
    (make-directory* class-path)
    (%call-writer-to-file-safely (build-path class-path (key-of obj))
                                 (build-path (ref db 'path) "tmp/")
                                 (pa$ kahua-write obj))
    (set! (ref obj '%floating-instance) #f)))

(define-method kahua-db-write-id-counter ((db <kahua-db-fs>))
  (let1 db-path (ref db 'path)
    (%call-writer-to-file-safely (id-counter-path db-path)
                                 (build-path db-path "tmp/")
                                 (pa$ write (ref db 'id-counter)))))


(define (data-path db class . key)
  (apply build-path
         (ref db 'path)
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
