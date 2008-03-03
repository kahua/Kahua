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
	  readpass/
	  fileref/
	  check/
	  check-set/
	  radio-set/
	  readtext/
	  submit/
	  reset/
	  dropdown/
	  multisel/
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
    `(label/ ,#`",n :" (input/ (@/ (name ,n))))))

(define-macro (readpass/ pass)
  (let1 p (x->string pass)
    `(label/ ,#`",p :" (input/ (@/ (type "password") (name ,p))))))

(define-macro (fileref/ file)
  (let1 f (x->string file)
    `(label/ ,#`",f :" (input/ (@/ (type "file") (name ,f))))))

(define-macro (check/ cbox)
  (let1 c (x->string cbox)
    `(label/ (input/ (@/ (type "checkbox") (name ,c))) ,c)))

(define-macro (check-set/ name lst)
  `(let ((n ,(x->string name))
	 (ls (map x->string ,lst)))
     (label/ #`",n :"
	     (map/ (lambda (v)
		     (label/ (input/ (@/ (type "checkbox") (name v))) v))
		   ls))))

(define-macro (radio-set/ name lst)
  `(let* ((nm ,(x->string name))
	  (ls (map x->string ,lst)))
     (label/ #`",nm :"
	     (map/ (lambda (v)
		     (label/ (input/ (@/ (type "radio") (name nm) (value v))) v))
		   ls))))

(define-macro (readtext/ name)
  (let1 n (x->string name)
    `(label/ ,#`",n :" (textarea/ (@/ (name ,n))))))

(define (submit/)
  (input/ (@/ (type "submit"))))

(define (reset/)
  (input/ (@/ (type "reset"))))

(define-macro (dropdown/ name lst)
  `(let* ((nm ,(x->string name))
	  (ls (map x->string ,lst)))
     (label/ #`",nm :"
	     (select/
	      (@/ (name nm))
	      (map/ (lambda (v) (option/ (@/ (value v)) v)) ls)))))

(define-macro (multisel/ name lst)
  `(let* ((nm ,(x->string name))
	  (l (min 5 (length ,lst)))
	  (ls (map x->string ,lst)))
     (label/ #`",nm :"
	     (select/
	      (@/ (name nm) (multiple #t) (size l))
	      (map/ (lambda (v) (option/ (@/ (value v)) v)) ls)))))



;;; Local Variables:
;;; mode: scheme
;;; coding: utf-8-unix
;;; End:

