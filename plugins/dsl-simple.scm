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

	  value/  ;; input as type='hidden'
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

;; Low Level
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (mask pred? item ilst)
  (map (lambda (i)
         (if (pred? i item) i #f))
       ilst))

(define (include? item ilst pred?)
  (if (null? ilst) #f
      (if (pred? item (car ilst)) #t
          (include? item (cdr ilst) pred?))))

(define (make-date-lite y m d)
  (make-date 0 0 0 0 d m y (date-zone-offset (current-date))))

(define (make-month-lite y m)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (make-year-lite y)
  (make-date 0 0 0 0 1 1 y (date-zone-offset (current-date))))

(define (today)
  (let1 d (current-date)
    (make-date-lite (date-year d) (date-month d) (date-day d))))

(define (current-month date)
  (make-month-lite (date-year date) (date-month date)))

(define (prev-month date)
  (if (= (date-month date) 1)
      (make-month-lite (dec (date-year date)) 12)
      (make-month-lite (date-year date) (dec (date-month date)))))

(define (next-month date)
  (if (= (date-month date) 12)
      (make-month-lite (inc (date-year date)) 1)
      (make-month-lite (date-year date) (inc (date-month date)))))

(define (nth-month-before n date)
  (if (= n 0) date (nth-month-before (dec n) (prev-month date))))

(define (nth-month-after n date)
  (if (= n 0) date (nth-month-after (dec n) (next-month date))))

(define (days-of-month date)
  (inexact->exact
   (- (date->modified-julian-day (next-month date))
      (date->modified-julian-day (current-month date)))))

(define (first-date-of-month date)
  (make-date-lite (date-year date) (date-month date) 1))

(define (last-date-of-month date)
  (make-date-lite (date-year date) (date-month date) (days-of-month date)))

(define (same-date? d1 d2)
  (and (and (date? d1) (date? d2))
       (= (date-day d1) (date-day d2))
       (= (date-month d1) (date-month d2))
       (= (date-year d1) (date-year d2))))

(define (same-month? d1 d2)
  (and (and (date? d1) (date? d2))
       (= (date-month d1) (date-month d2))
       (= (date-year d1) (date-year d2))))

(define (same-year? d1 d2)
  (and (and (date? d1) (date? d2))
       (= (date-year d1) (date-year d2))))

(define (dates-of-month date)
  (let ((y (date-year date))
        (m (date-month date)))
    (map (lambda (d)
           (make-date-lite y m d))
         (iota (days-of-month date) 1))))

(define (date-slices-of-month date . flag)
  (let1 flag (get-optional flag #f)
    (let* ((cday1 date)
           (pday1 (prev-month date))
           (cmonth (dates-of-month date))
           (pmonth (dates-of-month (prev-month date)))
           (nmonth (dates-of-month (next-month date))))
      (let1 pcn (slices (append (make-list (date-week-day pday1) #f)
                                pmonth cmonth nmonth)
                        7 #t #f)
        (let rec ((pcn pcn)
                  (cal '()))
          (if (null? pcn)
              (if flag (reverse cal)
                  (map (lambda (w)
                         (mask same-month? date w))
                       (reverse cal)))
              (if (include? date (car pcn) same-month?)
                  (rec (cdr pcn) (cons (car pcn) cal))
                  (rec (cdr pcn) cal))))))))

(define (dates-of-week date . flag)
  (let1 flag (get-optional flag #f)
    (let* ((wd (date-week-day date))
           (sunday (nth-day-before wd date)))
      (let rec ((week '())
                (day sunday)
                (n 7))
        (if (= n 0)
            (if flag (reverse week)
                (mask same-month? date (reverse week)))
            (rec (cons day week) (next-day day) (dec n)))))))

(define current-week dates-of-week)

(define (prev-week date . flag)
  (let1 flag (get-optional flag #f)
    (let* ((wd (date-week-day date))
           (saturday (nth-day-before (+ wd 1) date)))
      (let rec ((week '())
                (day saturday)
                (n 7))
        (if (= n 0)
            (if flag week
                (mask same-month? date week))
            (rec (cons day week) (prev-day day) (dec n)))))))

(define (next-week date . flag)
  (let1 flag (get-optional flag #f)
    (let* ((wd (date-week-day date))
           (sunday (nth-day-after (- 7 wd) date)))
      (let rec ((week '())
                (day sunday)
                (n 7))
        (if (= n 0)
            (if flag (reverse week)
                (mask same-month? date (reverse week)))
            (rec (cons day week) (next-day day) (dec n)))))))

(define (current-day date)
  (make-date-lite (date-year date) (date-month date) (date-day date)))

(define (prev-day date)
  (if (same-date? date
                  (first-date-of-month date))
      (last-date-of-month (prev-month date))
      (make-date-lite (date-year date)
                      (date-month date)
                      (dec (date-day date)))))

(define (next-day date)
  (if (same-date? date
                  (last-date-of-month date))
      (first-date-of-month (next-month date))
      (make-date-lite (date-year date)
                      (date-month date)
                      (inc (date-day date)))))

(define (nth-day-before n date)
  (if (= n 0) date (nth-day-before (dec n) (prev-day date))))

(define (nth-day-after n date)
  (if (= n 0) date (nth-day-after (dec n) (next-day date))))

(define (nth-week-before n date . flag)
  (let1 flag (get-optional flag #f)
    (if (= n 0)
        (current-week date flag)
        (nth-week-before (dec n) (car (prev-week date #t)) flag))))

(define (nth-week-after n date . flag)
  (let1 flag (get-optional flag #f)
    (if (= n 0)
        (current-week date flag)
        (nth-week-after (dec n) (car (next-week date #t)) flag))))

(define (yesterday) (prev-day (today)))
(define (tomorrow) (next-day (today)))

(define (current-year date)
  (make-year-lite (date-year date)))

(define (prev-year date)
  (make-year-lite (dec (date-year date))))

(define (next-year date)
  (make-year-lite (inc (date-year date))))

(define (nth-year-before n date)
  (if (= n 0) date (nth-year-before (dec n) (prev-year date))))

(define (nth-year-after n date)
  (if (= n 0) date (nth-year-after (dec n) (next-year date))))

(define (date=? . args)
  (apply = (map date->modified-julian-day args)))

(define (date<? . args)
  (apply < (map date->modified-julian-day args)))

(define (date<=? . args)
  (apply <= (map date->modified-julian-day args)))

(define (date>? . args)
  (apply > (map date->modified-julian-day args)))

(define (date>=? . args)
  (apply >= (map date->modified-julian-day args)))

;
; holiday
;
(define holy-day (make-date-lite 1948 7 20))
(define compensating-holiday (make-date-lite 1973 4 12))

(define int floor)
(define (make-equinox p1979 p2099 p2150)
  (define max-day 99)
  (lambda (yy)
    (let ((v1 (* 0.242194 (- yy 1980)))
          (v2 (int (/ (- yy 1983) 4)))
          (v3 (int (/ (- yy 1980) 4))))
      (cond ((<= yy 1947) max-day)
            ((<= yy 1979) (- (int (+ p1979 v1)) v2))
            ((<= yy 2099) (- (int (+ p2099 v1)) v3))
            ((<= yy 2150) (- (int (+ p2150 v1)) v3))
            (else max-day)))))

(define spring-equinox (make-equinox 20.8357 20.8431 21.851))
(define autumnal-equinox (make-equinox 23.2588 23.2488 24.2488))

(define (date->sys-time date)
  (time->seconds (date->time-utc date)))
(define (sys-time->date time)
  (time-utc->date (seconds->time time)))

(define (holiday? t)
  (define %workday 0)
  (define %saturday 1)
  (define %sunday 2)
  (define %holiday 3)
  (define %compensate 4)
  (define %holy 5)
  (define (prev-day d)
    (modified-julian-day->date
     (- (date->modified-julian-day d) 1.0)))

  (let ((yy (date-year t))
        (mm (date-month t))
        (dd (date-day t))
        (ww (date-week-day t)))

    (let ((r %workday)
	  (m #f))
      (define (set-holy! msg) (set! r %holy) (set! m msg))
      (define (set-holiday! msg) (set! r %holiday) (set! m msg))
      (define (set-compensate! msg) (set! r %compensate) (set! m msg))

      (case ww
        ((6) (set! r %saturday))
        ((0) (set! r %sunday)))

      (if (date<? t holy-day) r
          (case mm
            ((1) (case dd
                   ((1) (set-holy! "元旦"))
                   (else (if (>= yy 2000)
                             (if (= (int (/ (- dd 1) 7)) ww 1)
                                 (set-holy! "成人の日"))
                             (if (= dd 15) (set-holy! "成人の日"))))))
            ((2) (case dd
                   ((11) (if (>= yy 1967) (set-holy! "建国記念の日")))
                   ((24) (if (= yy 1989) (set-holy! "昭和天皇の大喪の礼")))))
            ((3) (if (= dd (spring-equinox yy))
                     (set-holy! "春分の日")))
            ((4) (case dd
                   ((29) (set-holy! (cond ((>= yy 2007) "昭和の日")
					  ((>= yy 1989) "みどりの日")
					  (else "天皇誕生日"))))
                   ((10) (if (= yy 1959) (set-holy! "皇太子明仁親王の結婚の儀")))))
            ((5) (case dd
                   ((3) (set-holy! "憲法記念日"))
                   ((4) (cond ((>= yy 2007) (set-holy! "みどりの日"))
			      ((and (> ww 1) (>= yy 1986))
			       (set-holiday! "国民の休日"))))
                   ((5) (set-holy! "こどもの日"))
		   ((6) (if (and (>= yy 2007) (or (= ww 2) (= ww 3)))
			    (set-compensate! "振替休日")))))
            ((6) (if (and (= yy 1993) (= dd 9))
                     (set-holy! "皇太子徳仁親王の結婚の儀")))
            ((7) (cond ((>= yy 2003)
                        (if (and (= (int (/ (- dd 1) 7)) 2) (= ww 1))
                            (set-holy! "海の日")))
                       ((>= yy 1996)
                        (if (= dd 20) (set-holy! "海の日")))))
            ((9) (if (= dd (autumnal-equinox yy))
                     (set-holy! "秋分の日")
                     (cond ((>= yy 2003)
                            (if (and (= (int (/ (- dd 1) 7)) 2) (= ww 1))
                                (set-holy! "敬老の日")
                                (if (and (= ww 2)
                                         (= dd (- (autumnal-equinox yy) 1)))
                                    (set-holiday! "国民の休日"))))
                           ((>= yy 1966) (if (= dd 15) (set-holy! "敬老の日"))))))
            ((10) (cond ((>= yy 2000) (if (= (int (/ (- dd 1) 7)) ww 1)
					  (set-holy! "体育の日")))
                        ((>= yy 1966) (if (= dd 10) (set-holy! "体育の日")))))
            ((11) (case dd
                    ((3) (set-holy! "文化の日"))
                    ((23) (set-holy! "勤労感謝の日"))
                    ((12) (if (= yy 1990) (set-holy! "即位礼正殿の儀")))))
            ((12) (case dd
                    ((23) (if (>= yy 1989) (set-holy! "天皇誕生日")))))))

      (if (and (<= r %holiday) (= ww 1))
          (if (date>=? t compensating-holiday)
              (if (= (holiday? (prev-day t)) %holy)
                  (set-compensate! "振替休日"))))

      (values r m))))


;; Middle Level
(define (date->ymd date) (date->string date "~Y/~m/~d"))

(define (ymd->date ymd) (string->date ymd "~Y/~m/~d"))

;;
;; calendar/
;;
(define (calendar/ @id ttl date . selected-date)
  ;;
  (let1 selected-date (get-optional selected-date #f)
    ;; calc date class
    (define (class-of d)
      (receive (typ msg) (holiday? d)
	(string-join
	 (list (list-ref '("workday" "saturday" "sunday" "holiday" "compensate" "holy") typ)
	       (if (date=? (today) d) "today" "")
	       (if (and selected-date (date=? selected-date d)) "target" "")
	       (if (same-month? d date) "" "other-month")))))
    ;; calendar body
    (table/
     (@/ (id #`",|@id|-tbl") (class "calendar"))
     (thead/
      (tr/ (td/ (@/ (colspan 3) (align "left")) ttl)
	   (td/ (@/ (colspan 4))
		(if selected-date (value/ @id (date->ymd selected-date)) ""))))
     (tbody/
      (tr/ (th/ (a/cont/
		    (@@/ (target #`",|@id|-tbl") (keep #t)
			 (parts-cont
			  (cut calendar/ @id ttl (nth-month-before 12 date) selected-date)))
		  (&/ 'laquo)))
	   (th/ (a/cont/
		    (@@/ (target #`",|@id|-tbl") (keep #t)
			 (parts-cont
			  (cut calendar/ @id ttl (prev-month date) selected-date)))
		  (&/ 'lsaquo)))
	   (th/ (@/ (colspan 3) (align "center"))
		#`",(date-year date)/,(date-month date)")
	   (th/ (a/cont/
		    (@@/ (target #`",|@id|-tbl") (keep #t)
			 (parts-cont
			  (cut calendar/ @id ttl (next-month date) selected-date)))
		  (&/ 'rsaquo)))
	   (th/ (a/cont/
		    (@@/ (target #`",|@id|-tbl") (keep #t)
			 (parts-cont
			  (cut calendar/ @id ttl (nth-month-after 12 date) selected-date)))
		  (&/ 'raquo)))))
     (tr/ (map/ (lambda (w a) (th/ (@/ (align "center") (class a)) w))
		'("S" "M" "T" "W" "T" "F" "S")
		'("sunday" "workday" "workday" "workday" "workday" "workday" "saturday")))
     (map/ (lambda (w)
	     (tr/ (map/ (lambda (d)
			  (td/ (@/ (class (class-of d)) )
			       (a/cont/
				   (@@/ (target #`",|@id|-tbl") (keep #t)
					(parts-cont
					 (cut calendar/ @id ttl date d)))
				 (date-day d))))
			w)))
	   (date-slices-of-month date #t)))))


;;; Local Variables:
;;; mode: scheme
;;; coding: utf-8-unix
;;; End:

