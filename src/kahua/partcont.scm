;; Implements partial continuation
;;
;;  Copyright (c) 2004-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

;; Cf.
;; Martin Gasbichler and Michael Sperber, Final Shift for Call/cc:
;; Direct implementation of Shift and Reset, ICFP '02, 2002.
;; http://citeseer.ist.psu.edu/gasbichler02final.html

;; Changes from Gasbichler & Sperber papers:
;;
;; 1. Renaming primitives to "stand out"
;;  (reset/pc expr)      == (reset expr)
;;  (call/pc (lambda (k) expr ...)) == (shift k (begin expr ...))
;;  (let/pc k expr ...)  == (shift k (begin expr ...))
;;
;; 2. the meta continuation can handle multiple values

(define-module kahua.partcont
  (use gauche.parameter)
  (export reset/pc call/pc let/pc))
(select-module kahua.partcont)

(define meta-continuation
  (make-parameter
   (lambda _ (error "stale meta-continuation invoked"))))

(define-syntax reset/pc
  (syntax-rules ()
    ((reset/pc expr)
     (%reset (lambda () expr)))))

(define-syntax let/pc
  (syntax-rules ()
    ((let/pc kont . body)
     (call/pc (lambda (kont) . body)))))

(define (%abort thunk)
  (receive v (thunk)
    (apply (meta-continuation) v)))

(define (%reset thunk)
  (let1 save (meta-continuation)
    (call/cc
     (lambda (k)
       (meta-continuation (lambda vals
                            (meta-continuation save)
                            (apply k vals)))
       (%abort thunk)))))

(define (call/pc proc)
  (call/cc
   (lambda (k)
     (%abort (lambda ()
               (proc (lambda vals (reset/pc (apply k vals)))))))))

(provide "kahua/partcont")
