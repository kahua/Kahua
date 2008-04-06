;; -*- mode: scheme; coding: utf-8 -*-
;;
;;  Copyright (c) 2005-2008 Katsutoshi Itoh
;;  Copyright (c) 2006-2007 Kahua Project, All rights reserved.
;;
;; dsl for simple page
;;
(use srfi-1)
(use srfi-13)
(use srfi-19)
(use gauche.collection)
(use util.list)
(use kahua.elem)

(define-plugin "dsl-simple"
  (version "0.1")
  (export anonpage
	  page
	  define-main-entry
	  define-page
	  define-main-page
	  define-anonmain-page

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
	  button/
	  confirm/
;;	  reset/
	  dropdown/
	  multisel/

	  ;; calendar
	  make-month
	  make-day
	  first-day-of-month
	  next-month
	  prev-month
	  days-of-month
	  date-slices-of-month
	  date->ymd
	  ymd->date
	  calendar/
	  )
  (depend #f))

;; anonymous page
(define (anonpage . body)
  (html/ (head/ (title/ "no title")) (apply body/ body)))

;; page
(define (page ttl . body)
  (html/ (head/ (title/ ttl)) (apply body/ body)))

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
       (page ,ttl
	     (h1/ (a/cont/ (@@/ (cont ,@ent)) ,ttl)) ,@body))))

;; define simple page as main-proc
;;
(define-macro (define-main-page ent . body)
  (let1 ttl (string-upcase (x->string (car ent)))
    `(define-main-entry ,ent
       (page ,ttl
	     (h1/ (a/cont/ (@@/ (cont ,@ent)) ,ttl)) ,@body))))

(define-macro (define-anonmain-page ent . body)
  (let1 ttl (string-upcase (x->string (car ent)))
    `(define-main-entry ,ent
       (anonpage ,@body))))

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
(define (readln/ var)
  (label/ #`",var :" (input/ (@/ (name var)))))

;; read password
;;
(define (readpass/ var)
  (label/ #`",var :" (input/ (@/ (type "password") (name var)))))

;; file upload
;;
(define (fileref/ var)
  (label/ #`",var :" (input/ (@/ (type "file") (name var)))))

;; one checkbox
;;
(define (check/ var)
  (label/ (input/ (@/ (type "checkbox") (name var))) var))

;; checkbox set
;;
(define (check-set/ var lst)
  (label/ #`",var :"
	  (map/ (lambda (v)
		  (label/
		   (input/ (@/ (type "checkbox") (name v)))
		   v))
		lst)))

;; radio buttons
;;
(define (radio-set/ var lst)
  (label/ #`",var :"
	  (map/ (lambda (v)
		  (label/
		   (input/ (@/ (type "radio") (name var) (value v)))
		   v))
		lst)))

;; read text(multi line)
;;
(define (readtext/ var)
  (label/ #`",var :" (textarea/ (@/ (name var)))))

;; modify text(multi line)
;;
(define (modifytext/ var text)
  (label/ #`",var :" (textarea/ (@/ (name var)) text)))

;; comment
;;
(define (comment/ var val)
  (define (comment-sub/ val)
    (form/cont/ (@/ (id var))
      (@@/ (target var)
	   (parts-cont (entry-lambda (:keyword preview save)
			 (if preview
			     (comment-sub/ (kahua-context-ref var))
			     (node-set/
			       (pre/ (kahua-context-ref var))
			       (comment-sub/ ""))))))
      (pre/ val)
      (textarea/ (@/ (name var)) val)
      (button/ "preview") (button/ "save")))
  (comment-sub/ val))



;; submit button
;;
(define (submit/)
  (input/ (@/ (type "submit") (name "submit"))))

;; button
;;
(define (button/ val)
  (input/ (@/ (type "submit") (name val) (value val))))

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
(define (dropdown/ var lst)
  (label/ #`",var :"
	  (select/
	   (@/ (name var))
	   (map/ (lambda (v)
		   (option/ (@/ (value v)) v))
		 lst))))

;; multi selector like as dropdown
;;
(define (multisel/ var lst)
  (let1 l (min 5 (length lst))
    (label/ #`",var :"
	    (select/
	     (@/ (name var) (multiple #t) (size l))
	     (map/ (lambda (v)
		     (option/ (@/ (value v)) v))
		   lst)))))

(define (value/ var val)
  (input/ (@/ (type "hidden") (name var) (value val))))

;;-------------------------------------------
;; calendar
;;
(define (make-month m y)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (make-day y m d)
  (make-date 0 0 0 0 d m y (date-zone-offset (current-date))))

(define (first-day-of-month date)
  (make-month (date-month date) (date-year date)))

(define (next-month date)
  (if (= (date-month date) 12)
      (make-month 1 (+ (date-year date) 1))
      (make-month (+ (date-month date) 1) (date-year date))))

(define (prev-month date)
  (if (= (date-month date) 1)
      (make-month 12 (- (date-year date) 1))
      (make-month (- (date-month date) 1) (date-year date))))

(define (days-of-month date)
  (inexact->exact
   (- (date->modified-julian-day (next-month date))
      (date->modified-julian-day (first-day-of-month date)))))

(define (date-slices-of-month date)
  (slices (append (make-list (date-week-day (first-day-of-month date)) #f)
		  (iota (days-of-month date) 1))
	  7 #t #f))

(define (date->ymd date) (date->string date "~Y/~m/~d"))

(define (ymd->date ymd) (string->date ymd "~Y/~m/~d"))

(define (calendar/ @id ttl date . selected-date)
  (let1 selected-date (get-optional selected-date #f)
    (table/
     (@/ (id #`",|@id|-tbl"))
     (thead/
      (tr/ (td/ (@/ (colspan 3)) ttl)
	   (td/ (@/ (colspan 4))
		(value/ @id selected-date))))
     (tbody/
      (tr/ (td/ (a/cont/
		    (@@/ (target #`",|@id|-tbl")
			 (keep #t)
			 (fragment #`",|@id|-tbl") ;; for test
			 (parts-cont
			  (cut calendar/ @id ttl (prev-month date) selected-date)))
		  "←"))
	   (td/ (@/ (colspan 5) (align "center"))
		#`",(date-year date)/,(date-month date)")
	   (td/ (a/cont/
		    (@@/ (target #`",|@id|-tbl")
			 (keep #t)
			 (fragment #`",|@id|-tbl") ;; for test
			 (parts-cont
			  (cut calendar/ @id ttl (next-month date) selected-date)))
		  "→"))))
     (tr/ (map/ td/ "SMTWTFS"))
     (map/ (lambda (w)
	     (tr/ (map/ (lambda (d)
			  (let1 ymd #`",(date-year date)/,(date-month date)/,d"
			    (td/ (@/ (bgcolor (if (equal? ymd selected-date)
						  "pink" "white")))
				 (a/cont/
				   (@@/ (target #`",|@id|-tbl")
					(keep #t)
					(parts-cont
					 (cut calendar/ @id ttl date ymd)))
				   (@/ (style "text-decoration: none"))

				   d))))
			w)))
	   (date-slices-of-month date)))))


;;; Local Variables:
;;; mode: scheme
;;; coding: utf-8-unix
;;; End:

