;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2008 Katsutoshi Itoh
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
	  modifytext/
	  comment/
	  submit/
	  post/
	  confirm/
;;	  reset/
	  dropdown/
	  multisel/
	  )
  (depend #f))

;; define-entry and set the entry as main-proc
;;
(define-macro (define-main-entry ent . body)
  `(begin
     (define-entry ,ent ,@body)
     (initialize-main-proc ,(car ent))))

;; define simple page
;; title head and h1 link to top are auto set
;;
(define-macro (define-page ent . body)
  (let1 ttl (string-upcase (x->string (car ent)))
    `(define-entry ,ent
       (html/ (head/ (title ,ttl))
	      (body/ (h1/ (a/cont/ (@@/ (cont ,@ent)) ,ttl)) ,@body)))))

;; define simple page as main-proc
;;
(define-macro (define-main-page ent . body)
  (let1 ttl (string-upcase (x->string (car ent)))
    `(define-main-entry ,ent
       (html/ (head/ (title/ ,ttl))
	      (body/ (h1/ (a/cont/ (@@/ (cont ,@ent)) ,ttl)) ,@body)))))

;; self update macro
;;
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

;; read line
;;
(define-macro (readln/ name)
  (let1 n (x->string name)
    `(label/ ,#`",n :" (input/ (@/ (name ,n))))))

;; read password
;;
(define-macro (readpass/ pass)
  (let1 p (x->string pass)
    `(label/ ,#`",p :" (input/ (@/ (type "password") (name ,p))))))

;; file upload
;;
(define-macro (fileref/ file)
  (let1 f (x->string file)
    `(label/ ,#`",f :" (input/ (@/ (type "file") (name ,f))))))

;; one checkbox
;;
(define-macro (check/ cbox)
  (let1 c (x->string cbox)
    `(label/ (input/ (@/ (type "checkbox") (name ,c))) ,c)))

;; checkbox set
;;
(define-macro (check-set/ name lst)
  `(let ((n ,(x->string name))
	 (ls (map x->string ,lst)))
     (label/ #`",n :"
	     (map/ (lambda (v)
		     (label/
		      (input/ (@/ (type "checkbox") (name v)))
		      v))
		   ls))))

;; radio buttons
;;
(define-macro (radio-set/ name lst)
  `(let* ((nm ,(x->string name))
	  (ls (map x->string ,lst)))
     (label/ #`",nm :"
	     (map/ (lambda (v)
		     (label/
		      (input/ (@/ (type "radio") (name nm) (value v)))
		      v))
		   ls))))
;; read text(multi line)
;;
(define-macro (readtext/ name)
  (let1 n (x->string name)
    `(label/ ,#`",n :" (textarea/ (@/ (name ,n))))))

;; modify text(multi line)
;;
(define-macro (modifytext/ name text)
  (let ((n (x->string name))
	(val text))
    `(label/ ,#`",n :" (textarea/ (@/ (name ,n)) ,val))))

;; comment
;;
(define-macro (comment/ name val)
  (let1 n (x->string name)
    `(let ()
       (define (comment-sub/ val)
	 (form/cont/ (@/ (id ,n))
	   (@@/ (target ,n)
		(parts-cont (entry-lambda (:keyword preview save ,name)
			      (if preview
				  (comment-sub/ ,name)
				  (node-set/
				    (pre/ ,name)
				    (comment-sub/ ""))))))
	   (pre/ val)
	   (textarea/ (@/ (name ,n)) val)
	   (post/ "preview") (post/ "save")))
       (comment-sub/ ,val))))

;; submit button
;;
(define (submit/)
  (input/ (@/ (type "submit") (name "submit"))))

;; post
;;
(define-macro (post/ val)
  (let1 n (x->string val)
    `(input/ (@/ (type "submit") (name ,n) (value ,val)))))

;; submit with confirm
;;
(define (confirm/ msg)
  (input/ (@/ (type "submit") (name "submit")
	      (onclick #`"confirm(',msg')"))))

;; reset button
;;
;;(define (reset/)
;;  (input/ (@/ (type "reset") (name "reset"))))

;; drop down selector
;;
(define-macro (dropdown/ name lst)
  `(let* ((nm ,(x->string name))
	  (ls (map x->string ,lst)))
     (label/ (@/ (style "display: block")) #`",nm :"
	     (select/
	      (@/ (name nm))
	      (map/ (lambda (v)
		      (option/ (@/ (value v)) v)) ls)))))

;; multi selector like as dropdown
;;
(define-macro (multisel/ name lst)
  `(let* ((nm ,(x->string name))
	  (l (min 5 (length ,lst)))
	  (ls (map x->string ,lst)))
     (label/ (@/ (style "display: block")) #`",nm :"
	     (select/
	      (@/ (name nm) (multiple #t) (size l))
	      (map/ (lambda (v)
		      (option/ (@/ (value v)) v)) ls)))))



;;; Local Variables:
;;; mode: scheme
;;; coding: utf-8-unix
;;; End:

