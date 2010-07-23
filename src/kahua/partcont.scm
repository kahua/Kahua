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
  (extend gauche.partcont)
  (export let/pc))
(select-module kahua.partcont)

(define-syntax let/pc
  (syntax-rules ()
    ((let/pc kont . body)
     (call/pc (lambda (kont) . body)))))

(provide "kahua/partcont")
