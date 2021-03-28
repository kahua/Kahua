;; -*- mode: scheme; coding: utf-8 -*-
;;
;; kahua.object-pool - Simple Object Pooling Facility (VERY EXPERIMENTAL)
;;
;;  Copyright (c) 2008 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(define-module kahua.object-pool
  (use gauche.parameter)
  (export <kahua:object-pool-meta>
          <kahua:object-pool-mixin>
          <kahua:read-only-meta>
          <kahua:read-only-mixin>))

(select-module kahua.object-pool)

(define-class <kahua:object-pool-meta> (<class>)
  ((%%object-table :init-form (make-hash-table 'equal?))
   (%%key-of :init-keyword :key-of
             :init-value (lambda (class initargs) (x->string initargs)))))

(define-class <kahua:object-pool-mixin> () () :metaclass <kahua:object-pool-meta>)

;; NOTE: This is NOT MT-safe!
(define-method make ((class <kahua:object-pool-meta>) . initargs)
  (let ((table (slot-ref class '%%object-table))
        (key ((slot-ref class '%%key-of) class initargs)))
    (cond ((hash-table-get table key #f) =>
           (lambda (o)
             (initialize o initargs)
             o))
          (else
           (let1 o (next-method)
             (hash-table-put! table key o)
             o)))))

(define-class <kahua:read-only-meta> (<class>) ())
(define-class <kahua:read-only-mixin> () () :metaclass <kahua:read-only-meta>)

(define *slot-protected?* (make-parameter #t))

(define-method initialize ((self <kahua:read-only-mixin>) initargs)
  (parameterize ((*slot-protected?* #f))
    (next-method)))

(define-method compute-get-n-set ((class <kahua:read-only-meta>) slot)
  (let* ((acc (compute-slot-accessor class slot (next-method)))
         (getter (cut slot-ref-using-accessor <> acc))
         (bound? (cut slot-bound-using-accessor? <> acc))
         (setter (if (slot-definition-option slot :read-only #f)
                     (lambda (o v)
                       (if (and (bound? o) (*slot-protected?*))
                           (errorf "slot ~s is already initialized." (slot-definition-name slot))
                           (slot-set-using-accessor! o acc v)))
                     (cut slot-set-using-accessor! <> acc <>))))
    (list getter setter bound? #t)))

(provide "kahua/object-pool")
