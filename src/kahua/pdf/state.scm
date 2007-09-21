;; State monad

(define-module kahua.pdf.state
  (use srfi-1)
  (use srfi-11)
  (use kahua.pdf.monad)
  (export <state-monad>
          >>=
          >>
          unit/state
          get/state
          put/state
          update/state
          run/state
          exec/state
          eval/state
          )
)

(select-module kahua.pdf.state)

(define-class <state-monad> (<monad>) ())

(define (unit/state x) 
  (make <state-monad> :monad (lambda (state) (cons x state))))

(define-method >>= ((st0 <state-monad>) f)
  (make <state-monad>
    :monad (lambda (s0)	     
	     (let-values (((x s1) (car+cdr (run/state st0 s0))))
               (run/state (f x) s1)))))
(define-method >> ((st0 <state-monad>) (st1 <state-monad>))
  (>>= st0 (lambda (_) st1)))

(define get/state 
  (make <state-monad> :monad (lambda (state) (cons state state))))
(define (put/state state)
  (make <state-monad> :monad (lambda (_) (cons #f state))))
(define (update/state uf)
  (>>= get/state (lambda (s) (put/state (uf s)))))

(define (run/state st s0) ((monad-of st) s0))
(define (exec/state s0 st) (cdr (run/state st s0)))
(define (eval/state s0 st) (car (run/state st s0)))

(define (sequence/state>>= sts)
  (if (null? sts)
      (unit/state #f)
      (>>= (car sts)
	   (lambda (x) 
	     (>>= (sequence/state>>= (cdr sts))
		  (lambda (xs) (unit/state (cons x xs))))))))

(define (sequence/state>> sts)
  (fold-right >> (unit/state #f) sts))

(define (map/state>>= f xs)
  (sequence/state>>= (map f xs)))

(define (map/state>> f xs)
  (sequence/state>> (map f xs)))

(provide "kahua/pdf/state")
