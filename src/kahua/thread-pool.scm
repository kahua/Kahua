;; -*- mode: scheme; coding: utf-8 -*-
;;
;; kahua.thread-pool - Thread Pooling Facility
;;
;;  Copyright (c) 2006-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(define-module kahua.thread-pool
  (use srfi-1)
  (use util.queue)
  (extend gauche.threads)
  (export <thread-pool>
	  make-thread-pool
	  thread-pool-add-task
	  thread-pool-inc!
	  thread-pool-dec!
	  thread-pool-wait-all
	  thread-pool-finish-all))
(select-module kahua.thread-pool)

(define-class <thread-pool> ()
  ((pool  :init-keyword :pool  :init-value '())
   (queue :init-keyword :queue :init-form (make-queue))
   (mutex :init-keyword :mutex :init-form (make-mutex))
   (cv    :init-keyword :cv    :init-form (make-condition-variable))
   (task  :init-keyword :task)))

(define (make-thread-pool num)
  (define (do-task queue mutex cv)
    (call/cc (lambda (finish)
	       (let1 t (current-thread)
		 (do () (#f)
		   (mutex-lock! mutex)
		   (cond ((thread-specific t)
			  (mutex-unlock! mutex)
			  (finish))
			 ((queue-empty? queue)
			  (mutex-unlock! mutex cv))
			 (else
			  (let1 thunk (dequeue! queue)
			    (mutex-unlock! mutex)
			    (thunk))))))))
    #t)
  (let* ((queue (make-queue))
	 (mutex (make-mutex))
	 (cv    (make-condition-variable))
	 (task  (cute do-task queue mutex cv))
	 (pool  (list-tabulate num (lambda _
				     (let1 t (make-thread task)
				       (thread-specific-set! t #f)
				       (thread-start! t))))))
    (make <thread-pool>
      :pool pool
      :queue queue
      :mutex mutex
      :cv cv
      :task task)))

(define (thread-pool-add-task tp thunk)
  (let ((queue (slot-ref tp 'queue))
	(mutex (slot-ref tp 'mutex))
	(cv    (slot-ref tp 'cv)))
    (with-locking-mutex mutex
      (lambda ()
	(enqueue! queue thunk)
	(condition-variable-signal! cv)))))

(define (thread-pool-inc! tp . maybe-num)
  (let ((task (slot-ref tp 'task))
	(mutex (slot-ref tp 'mutex)))
    (with-locking-mutex mutex
      (lambda ()
	(slot-set! tp 'pool
		   (let loop ((num (get-optional maybe-num 1))
			      (pool (slot-ref tp 'pool)))
		     (if (<= num 0)
			 pool
			 (loop (- num 1)
			       (cons (let1 t (make-thread task)
				       (thread-specific-set! t #f)
				       (thread-start! t)
				       t)
				     pool)))))))))

(define (thread-pool-dec! tp . maybe-num)
  (with-locking-mutex (slot-ref tp 'mutex)
    (lambda ()
      (slot-set! tp 'pool
		 (let loop ((num (get-optional maybe-num 1))
			    (pool (slot-ref tp 'pool)))
		   (if (or (<= num 0) (null? pool))
		       pool
		       (begin
			 (thread-specific-set! (car pool) #t)
			 (loop (- num 1) (cdr pool)))))))))

(define (thread-pool-wait-all tp . maybe-interval)
  (let ((mutex (slot-ref tp 'mutex))
	(cv    (slot-ref tp 'cv))
	(queue (slot-ref tp 'queue))
	(sleep (cute sys-nanosleep (get-optional maybe-interval 5e8))))
    (call/cc (lambda (done)
	       (do () (#f)
		 (with-locking-mutex mutex
		   (lambda () (when (queue-empty? queue) (done))))
		 (sleep))))))

(define (thread-pool-finish-all tp . maybe-timeout)
  (let ((timeout (get-optional maybe-timeout #f))
	(pool (slot-ref tp 'pool))
	(cv   (slot-ref tp 'cv)))
    (for-each (lambda (t) (thread-specific-set! t #t)) pool)
    (for-each (lambda (t)
		(condition-variable-broadcast! cv)
		(unless (thread-join! t timeout #f)
		  (thread-terminate! t)))
	      pool)))

(provide "kahua/thread-pool")
