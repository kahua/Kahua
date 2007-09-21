;;; Copyright (c) 2004 Hisazumi, Kenji. All rights reserved.
;;;
;;; Query language for kahua.persistence
;;;

(use srfi-1)
(use srfi-2)
(use kahua.persistence)
(use gauche.collection)

(define-plugin "query"
  (version "0.1")
  (export QUERY
	  FROM: WHERE: ORDERBY: PRJ: ref:
	  =: <: >: %%: <=: >=:
	  and: or:)
  (depend #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Predicates

;; utilities
(define class-pred-alist
  (list '(#f =: <: >: %%: <=: >=:)
        (list <string> string=? string<? string>? (cut string-scan <> <>) string<=? string>=?)
        (list <integer> = < > #f <= >=)
	(list <boolean> eq? #f #f #f #f #f)
	(list <null> eq? #f #f #f #f #f)
	))

(define (class->pred class n)
  (car (drop (assq class class-pred-alist) n)))

(define (eval-args obj args)
  (map (lambda (a)
         (if (procedure? a) 
             (a obj)
             a))
       args))

(define (make-predicate c->p)
  (lambda (a . rest)
    (let* ((args     (cons a rest))
           (literal  (remove procedure? args))
           (class    (class-of (car literal))) ;;XXX check types
           (cpred    (c->p class)))
      
      (cond ((= (length args) (length literal))
	     (lambda (_)
	       (apply cpred args)))
	    ((or (eq? class <boolean>) (eq? class <null>))
	     (lambda (n)
	       (apply cpred (eval-args n args))))
	    (#t
	     (lambda (o)
	       (let1 evaled-args (eval-args o args)
		 (and (every (lambda (v)
			       (and (not (null? v))
				    v))
			     evaled-args)
		      (apply cpred evaled-args)))))))))

(define-macro (define-predicates)
  `(begin
     ,@(let ((n 0))
         (map (lambda (p)
                (set! n (+ n 1))
                `(define ,p (make-predicate (cut class->pred <> ,n))))
              (cdar class-pred-alist)))))

(define-predicates)

(define (and: . args)
  (lambda (o)
    (let loop ((args args))
      (cond ((null? args) #t)
            (((car args) o) (loop (cdr args)))
            (else #f)))))

(define (or: . args)
  (lambda (o)
    (let loop ((args args))
      (cond ((null? args) #f)
            (((car args) o) #t)
            (else (loop (cdr args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; QUERY API
;;;
(define (QUERY . args)
  (define (clause name)
    (let1 cl (assq name args)
      (and (pair? cl) (cadr cl))))
  
  (let* ((tables (let1 tc (assq 'from args)
                   (unless (pair? tc) (error "table was not specified"))
                   (cdr tc)))
         (col    (coerce-to <list> (make-kahua-collection (car tables))))
         (where  (clause 'where))
         (fltd   (if where (filter where col) col))
         (order  (clause 'orderby))
         (std    (if order (sort fltd order) fltd))
         (prj    (clause 'projection))
         (prjd   (if prj  (map-in-order prj std) std)))
    prjd))

(define (FROM: tab . rest)
  `(from ,tab ,@rest))

(define (WHERE: args)
  `(where ,args))

(define-method ORDERBY: ((dir <symbol>) (acsr <procedure>))
  (cond ((eq? dir 'DESC)
         `(orderby ,(lambda (a b)
                      (> (compare (acsr a) (acsr b)) 0))))
        (else
         `(orderby ,(lambda (a b)
                      (< (compare (acsr a) (acsr b)) 0))))))

(define-method ORDERBY: ((args <procedure>))
  (ORDERBY: 'INC args))

(define (PRJ: args)
  `(projection ,args))

(define-method ref: ((slot <symbol>))
  (lambda (obj)
    (ref obj slot)))

(define-method ref: ((class <class>) (slot <symbol>))
  (lambda (obj)
    (unless (is-a? obj class) (errorf "type unmatch ~S:~S" class obj))
    (ref obj slot)))

(define-method ref: ((proc <procedure>) (slot <symbol>))
  (lambda (obj)
    (ref (proc obj) slot)))

