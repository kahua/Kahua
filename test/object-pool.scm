;; -*- coding: utf-8 ; mode: scheme -*-
;; test kahua.object-pool

(use gauche.test)

(test-start "kahua.object-pool")

(use kahua.object-pool)
(test-module 'kahua.object-pool)

(define *a* #f)
(define *b* #f)

(test-section "First: instances are created every initializer.")

(define-class <a-class> (<kahua:object-pool-mixin> <kahua:read-only-mixin>)
  ((a :init-keyword :a :read-only #t)
   (b :init-keyword :b))
  :key-of (lambda (_ initargs)
            (get-keyword* :a initargs #f)))

(test* "First object" <a-class> (let1 a (make <a-class> :a "a" :b "a")
                                  (set! *a* a)
                                  (class-of a)))
(test* "slot a" "a" (slot-ref *a* 'a) string=?)
(test* "slot b" "a" (slot-ref *a* 'b) string=?)

(test* "Second object" <a-class> (let1 b (make <a-class> :a "b" :b "b")
                                   (set! *b* b)
                                   (class-of b)))
(test* "slot a" "b" (slot-ref *b* 'a) string=?)
(test* "slot b" "b" (slot-ref *b* 'b) string=?)

(test* "identical?" #f (eq? *a* *b*))

(test-section "Second: re-initialization of pooled objects.")

(test* "Re-initialize" *a* (make <a-class> :a "a" :b "b") eq?)
(test* "slot a" "a" (slot-ref *a* 'a) string=?)
(test* "slot b" "b" (slot-ref *a* 'b) string=?)

(test* "Re-initialize" *b* (make <a-class> :a "b" :b "a") eq?)
(test* "slot a" "b" (slot-ref *b* 'a) string=?)
(test* "slot b" "a" (slot-ref *b* 'b) string=?)

(test-section "Third: inheritance")

(define-class <b-class> (<a-class>) ())
(define *aa* #f)
(define *bb* #f)

(test* "First object" <b-class> (let1 a (make <b-class> :a "a" :b "a")
                                  (set! *aa* a)
                                  (class-of a)))
(test* "Identical?" #f (eq? *a* *aa*))
(test* "slot a" "a" (slot-ref *aa* 'a) string=?)
(test* "slot b" "a" (slot-ref *aa* 'b) string=?)

(test* "cannot modify read only slot" *test-error* (slot-set! *aa* 'a "b"))
(test* "can modify slot which is not specified as read-only"
       "b" (begin (slot-set! *aa* 'b "b") (slot-ref *aa* 'b)) string=?)

(test-end)
