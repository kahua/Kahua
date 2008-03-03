;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2006 Tatsuya BIZENN.
;;  Copyright (c) 2006-2007 Kahua Project, All rights reserved.
;;
;; dsl for simple page
;;

(use srfi-13)
(use kahua.elem)

(define-plugin "dsl-simple"
  (version "0.1")
  (export define-main-entry
	  define-page
	  define-main-page

	  selfish

	  readln/
	  readtext/
	  submit/
	  reset/
	  )
  (depend #f))

(define-macro (define-main-entry ent . body)
  `(begin
     (define-entry ,ent ,@body)
     (initialize-main-proc ,(car ent))))

(define-macro (define-page ent . body)
  (let1 ttl (string-upcase (x->string (car ent)))
    `(define-entry ,ent
       (html/ (head/ (title ,ttl))
	      (body/ (h1/ (a/cont/ (@@/ (cont ,@ent)) ,ttl)) ,@body)))))

(define-macro (define-main-page ent . body)
  (let1 ttl (string-upcase (x->string (car ent)))
    `(define-main-entry ,ent
       (html/ (head/ (title/ ,ttl))
	      (body/ (h1/ (a/cont/ (@@/ (cont ,@ent)) ,ttl)) ,@body)))))

(define-syntax selfish
  (syntax-rules (form/cont/ a/cont/)
    ((_ self (form/cont/ (fn args expr ...) body ...))
     (form/cont/ (@/ (id self))
       (@@/ (target self) (keep #t)
	    (parts-cont (fn args (selfish self expr) ...))) body ...))
    ((_ self (a/cont/ (fn args expr ...) elm ...))
     (a/cont/ (@/ (id self))
       (@@/ (target self) (keep #t)
	    (parts-cont (fn args (selfish self expr) ...))) elm ...))
    ((_ self (tag node ...))
     (tag (selfish self node ...)))
    ((_ self elm) elm)))

(define-macro (readln/ name)
  (let1 n (x->string name)
    `(input/ (@/ (name ,n)))))

(define-macro (readtext/ name)
  (let1 n (x->string name)
    `(textarea/ (@/ (name ,n)))))

(define (submit/)
  (input/ (@/ (type "submit"))))

(define (reset/)
  (input/ (@/ (type "reset"))))

;;; Local Variables:
;;; mode: scheme
;;; coding: utf-8-unix
;;; End:

