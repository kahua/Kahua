;; PDF utilities
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: util.scm,v 1.2 2004/03/03 04:45:34 shiro Exp $

;; This file includes a code taken from Bruce Butterfield (bab@entricom.com)'s
;; scm-pdf, which is derived from Marc Battyani's cl-pdf.  The following
;; is the original copyright notice of cl-pdf.

;;;  cl-pdf is a Common Lisp library for generating PDF files.
;;;
;;;  It is distributed under a FreeBSD style license
;;;  (if you want another license contact me) marc.battyani@fractalconcept.com
;;;
;;;  Copyright (c) 2002 Marc Battyani. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without modification, are
;;;  permitted provided that the following conditions are met:
;;;
;;;  Redistributions of source code must retain the above copyright notice, this list of
;;;  conditions and the following disclaimer.
;;;
;;;  Redistributions in binary form must reproduce the above copyright notice, this list of
;;;  conditions and the following disclaimer in the documentation and/or other materials 
;;;  provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE MARC BATTYANI ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
;;;  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARC BATTYANI OR
;;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;;  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  The latest version is at http://www.fractalconcept.com/asp/html/cl-pdf.html
;;;  You can contact me at marc.battyani@fractalconcept.com or marc@battyani.net


(define-module kahua.pdf.util
  (use gauche.parameter)
  (use gauche.collection)
  (use gauche.uvector)
  (use math.const)
  (use srfi-1)
  (use srfi-13)
  (use srfi-11)
  (use kahua.pdf.srfi-48)
  (export set-page-stream
	  in-text-mode
          set-font
	  define-pdf-op
	  define-pdf-op/48
	  move-to-next-line
	  draw-text
	  draw-jtext
	  draw-combo-text
	  draw-char
	  move-text
	  draw-text-on-next-line
	  set-text-rendering-mode
	  set-char-spacing
	  set-text-x-scale
	  set-text-leading
	  set-text-rise
	  set-text-matrix
	  draw-and-adjust-string
	  escape
	  with-saved-state
	  rotate
	  translate
	  scale
	  set-line-width
	  set-line-cap
	  set-line-join
	  set-dash-pattern
	  set-mitter-limit
	  move-to
	  line-to
	  bezier-to
	  bezier2-to
	  bezier3-to
	  close-path
	  basic-rect
	  stroke
	  close-and-stroke
	  fill-path
	  close-and-fill
	  even-odd-fill
	  fill-and-stroke
	  even-odd-fill-and-stroke
	  close-fill-and-stroke
	  close-even-odd-fill-and-stroke
	  end-path-no-op
	  clip-path
	  even-odd-clip-path
	  set-gray-stroke
	  set-gray-fill
	  set-rgb-stroke
	  set-rgb-fill
	  set-cymk-stroke
	  set-cymk-fill
	  +2pi+
	  +pi/2+
	  arc
	  pie
	  circle
	  ellipse
	  rectangle
	  dotimes
	  polyline
	  regular-polygon
	  star
	  ))

(select-module kahua.pdf.util)
  
(define *page-stream* (make-parameter #f))
  
(define (set-page-stream port)
  (*page-stream* port))

;; text functions
(define (ff d n)
  (let1 f (expt 10.0 d)
    (/ (round (* n f)) f)))

(define-syntax in-text-mode
  (syntax-rules ()
    ((_ arg ...)
     (begin
       (format (*page-stream*) "BT~%")
       arg ...
       (format (*page-stream*) "ET~%")))))

(define (set-font font size)
  (format-48 (*page-stream*) "~a ~6,2F Tf~%" font size))
  
(define-syntax define-pdf-op
  (syntax-rules ()
    ((_ name tmpl)
     (define name
       (lambda ()
	 (format (*page-stream*) tmpl)
	 (format (*page-stream*) "~%"))))
    ((_ name (arg ...) tmpl)
     (define name
       (lambda (arg ...)
	 (format (*page-stream*) tmpl arg ...))))))

(define-syntax define-pdf-op/48
  (syntax-rules ()
    ((_ name tmpl)
     (define name
       (lambda ()
	 (format-48 (*page-stream*) tmpl)
	 (format-48 (*page-stream*) "~%"))))
    ((_ name (arg ...) tmpl)
     (define name
       (lambda (arg ...)
	 (format-48 (*page-stream*) tmpl arg ...))))))

(define-pdf-op move-to-next-line " T*~%")
(define-pdf-op draw-text (str) "(~a) Tj~%")
(define-pdf-op/48 move-text (dx dy) "~16,3F ~16,3F Td~%")
(define-pdf-op/48 draw-text-on-next-line (string) "(~a) '~%")
(define-pdf-op set-text-rendering-mode (mode) "~d Tr~%")
(define-pdf-op/48 set-char-spacing (space) "~16,3F Tc~%")
(define-pdf-op/48 set-text-x-scale (scale) "~16,3F Tz~%")
(define-pdf-op/48 set-text-leading (space) "~16,3F TL~%")
(define-pdf-op/48 set-text-rise (rise) "~16,3F Ts~%")
(define-pdf-op/48 set-text-matrix (a b c d e f)
  "~16,3F ~16,3F ~16,3F ~16,3F ~16,3F ~16,3F Tm~%")
  
(define (draw-and-adjust-string strings)
  (format (*page-stream*) "[ ")
  (for-each
   (lambda (str)
     (if (number? str)
	 (format-48 (*page-stream*) "~16,3F " (ff 3 str))
	 (format (*page-stream*) "(~a) " str)))
   strings)
  (format (*page-stream*) "] TJ"))
  

;; escape special characters in strings
(define escape-table 
  (map (lambda (x y)
	 (cons (string->regexp x) y))
       '("\\(" "\\)" "\\\\")
       '("\\\\(" "\\\\)" "\\\\\\")))
      
(define (escape str)
  (foldr (lambda (esc str)
	   (regexp-replace* (car esc) str (cdr esc)))
	 str
	 escape-table))
  
;; graphic functions
  
(define-syntax with-saved-state
  (syntax-rules ()
    ((_ arg ...)
     (begin
       (format (*page-stream*) "q~%")
       arg ...
       (format (*page-stream*) "Q~%")))))

(define (rotate deg)
  (let* ((angle (/ (* pi deg) 180.0))
	 (s (sin angle))
	 (c (cos angle)))
    (format-48 (*page-stream*) 
	    "~16,3F ~16,3F ~16,3F ~16,3F 0.0 0.0 cm~%"
	    c s (- s) c)))
  
(define-pdf-op/48 translate (dx dy) "1.0 0.0 0.0 1.0 ~16,3F ~16,3F cm~%")
(define-pdf-op/48 scale (sx sy) " ~16,3F 0.0 0.0 ~16,3F 0.0 0.0 cm~%")
(define-pdf-op/48 set-line-width (width) "~16,3F w~%")
(define-pdf-op set-line-cap (mode) "~d J~%")
(define-pdf-op set-line-join (mode) "~d j~%")
(define-pdf-op set-dash-pattern (dash-array phase) "[~{~d~^ ~}] ~d~%")
(define-pdf-op/48 set-mitter-limit (limit) "~16,3F M~%")
(define-pdf-op/48 move-to (x y) "~16,3F ~16,3F m~%")
(define-pdf-op/48 line-to (x y) "~16,3F ~16,3F l~%")
(define-pdf-op/48 bezier-to (x1 y1 x2 y2 x3 y3) "~16,3F ~16,3F ~16,3F ~16,3F ~16,3F ~16,3F c~%")
(define-pdf-op/48 bezier2-to (x2 y2 x3 y3) "~16,3F ~16,3F ~16,3F ~16,3F v~%")
(define-pdf-op/48 bezier3-to (x1 y1 x3 y3) "~16,3F ~16,3F ~16,3F ~16,3F y~%")
(define-pdf-op close-path " h")
(define-pdf-op/48 basic-rect (x y dx dy) "~16,3F ~16,3F ~16,3F ~16,3F re~%")
(define-pdf-op stroke " S")
(define-pdf-op close-and-stroke " s")
(define-pdf-op fill-path " f")
(define-pdf-op close-and-fill " h f")
(define-pdf-op even-odd-fill " f*")
(define-pdf-op fill-and-stroke " B")
(define-pdf-op even-odd-fill-and-stroke " B*")
(define-pdf-op close-fill-and-stroke " b")
(define-pdf-op close-even-odd-fill-and-stroke " b*")
(define-pdf-op end-path-no-op " n")
(define-pdf-op clip-path " W")
(define-pdf-op even-odd-clip-path " W*")
(define-pdf-op/48 set-gray-stroke (gray) "~16,3F G~%")
(define-pdf-op/48 set-gray-fill (gray) "~16,3F g~%")
(define-pdf-op/48 set-rgb-stroke (r g b) "~16,3F ~16,3F ~16,3F RG~%")
(define-pdf-op/48 set-rgb-fill (r g b) "~16,3F ~16,3F ~16,3F rg~%")
(define-pdf-op/48 set-cymk-stroke (c y m k) "~16,3F ~16,3F ~16,3F ~16,3F K~%")
(define-pdf-op/48 set-cymk-fill (c y m k) "~16,3F ~16,3F ~16,3F ~16,3F k~%")
  
;; geometry
  
(define +2pi+ (* 2 pi))
(define +pi/2+ (/ pi 2))
  
(define (arc center-x center-y radius start extent)
  (move-to (+ center-x (* radius (cos start)))
	   (+ center-y (* radius (sin start))))
  (arc-to center-x center-y radius start extent)
  (line-to center-x center-y))

(define (pie center-x center-y radius start extent)
  (move-to center-x center-y)
  (line-to (+ center-x (* radius (cos start)))
	   (+ center-y (* radius (sin start))))
  (arc-to center-x center-y radius start extent)
  (line-to center-x center-y))

(define (circle center-x center-y radius)
  (move-to (+ center-x radius) center-y)
  (arc-to center-x center-y radius 0 +2pi+))

(define (ellipse center-x center-y radius-a radius-b)
  (move-to (+ center-x radius-a) center-y)
  (let ((kappa (* 4 (/ (- (sqrt 2) 1) 3.0))))
    (bezier-to (+ center-x radius-a) (+ center-y (* kappa radius-b))
	       (+ center-x (* kappa radius-a)) (+ center-y radius-b)
	       center-x (+ center-y radius-b))
    (bezier-to (- center-x (* kappa radius-a)) (+ center-y radius-b)
	       (- center-x radius-a) (+ center-y (* kappa radius-b))
	       (- center-x radius-a) center-y)
    (bezier-to (- center-x radius-a) (- center-y (* kappa radius-b))
	       (- center-x (* kappa radius-a)) (- center-y radius-b)
	       center-x (- center-y radius-b))
    (bezier-to (+ center-x (* kappa radius-a)) (- center-y radius-b)
	       (+ center-x radius-a) (- center-y (* kappa radius-b))
	       (+ center-x radius-a) center-y)))

(define (rectangle x y dx dy radius)
  (if (zero? radius)
      (basic-rect x y dx dy)
      (begin
	(move-to (+ x dx) (- (+ y dy) radius))
	(polyline (list (list x y) (list (+ x dx) y)
			(list (+ x dx) (+ y dy)) (list x (+ y dy)))
		  radius #t))))
  
(define-syntax dotimes
  (syntax-rules ()
    ((_ (index maxval) body ...)
     (do ((index 0 (+ index 1)))
         ((= index maxval))
       body ...))))

(define (polyline points radius closed)
  (let ((x-coord (lambda (pt) (car (car pt))))
	(y-coord (lambda (pt) (cadr (car pt)))))
    (if (zero? radius)
	(let ((x1 (x-coord points))
	      (y1 (y-coord points)))
	  (move-to x1 y1)
	  (let loop ((point (cdr points)))
	    (if (not (null? point))
		(begin
		  (line-to (x-coord point) (y-coord point))
		  (loop (cdr point)))))
	  (if closed
	      (line-to x1 y1))))
    (begin
      (if closed
	  (let ((break-point (midpoint (car points) (car (last-pair points)) 0.5)))
	    (set! points `(,break-point ,@points ,break-point))))
      (move-to (x-coord points) (y-coord points))
      (dotimes (i (- (length points) 2))
	       (let ((p1 (list-ref points i))
		     (p2 (list-ref points (+ 1 i)))
		     (p3 (list-ref points (+ 2 i))))
		 (fillet p2 p1 p3 radius)))
      (line-to (x-coord (last-pair points))
	       (y-coord (last-pair points))))))

(define regular-polygon
  (case-lambda
   ((center-x center-y radius sides fillet-radius)
    (polyline
     (let ((step-angle (/ +2pi+ sides)))
       (do ((current-angle +2pi+ (+ current-angle step-angle))
	    (side 0 (+ side 1))
	    (lst '()))
	   ((> side sides) lst)
	 (set! lst (cons (list (+ center-x (* radius (cos current-angle)))
			       (+ center-y (* radius (sin current-angle))))
			 lst))))
     fillet-radius #t))
   ((center-x center-y radius sides)
    (regular-polygon center-x center-y radius sides 0))))

(define star
  (case-lambda
   ((center-x center-y ext-radius int-radius sides fillet-radius)
    (let* ((current-angle +pi/2+)
	   (step-angle (/ +2pi+ sides))
	   (half-step (/ step-angle 2.0))
	   (points '()))
      (dotimes (i sides)
	       (set! points 
		     (cons (list (+ center-x (* ext-radius (cos current-angle)))
				 (+ center-y (* ext-radius (sin current-angle))))
			   points))
	       (set! points
		     (cons (list (+ center-x (* int-radius (cos (+ current-angle half-step))))
				 (+ center-y (* int-radius (sin (+ current-angle half-step)))))
			   points))
	       (set! current-angle (+ current-angle step-angle)))
      (polyline points fillet-radius #t)))
   ((center-x center-y ext-radius int-radius sides)
    (star center-x center-y ext-radius int-radius sides 0))))


;;; Non exported functions

(define (arc-to center-x center-y radius start extent)
  ;; An arc of extent zero will generate an error at bezarc (divide by zero).
  ;; This case may be given by two aligned points in a polyline.
  ;; Better do nothing.
  (unless (zero? extent)
	  (if (<= (abs extent) (/ pi 2.0))
	      (let-values (((x1 y1 x2 y2 x3 y3)
			    (bezarc center-x center-y radius start extent)))
			  (bezier-to x1 y1 x2 y2 x3 y3))
	      (let ((half-extent (/ extent 2.0)))
		(arc-to center-x center-y radius start half-extent)
		(arc-to center-x center-y radius (+ start half-extent) half-extent)))))
  
(define (bezarc center-x center-y radius start extent)
  ;; start and extent should be in radians.
  ;; Returns first-control-point-x first-control-point-y
  ;;         second-control-point-x second-control-point-y
  ;;         end-point-x end-point-y
  (let* ((end (+ start extent))
	 (s-start (sin start)) (c-start (cos start))
	 (s-end (sin end)) (c-end (cos end))
	 (ang/2 (/ extent 2.0))
	 (kappa (* (/ 4.0 3.0)
		   (/ (- 1 (cos ang/2))
		      (sin ang/2))))
	 (x1 (- c-start (* kappa s-start)))
	 (y1 (+ s-start (* kappa c-start)))
	 (x2 (+ c-end   (* kappa s-end)))
	 (y2 (- s-end   (* kappa c-end))))
    (values (+ (* x1 radius) center-x) (+ (* y1 radius) center-y)
	    (+ (* x2 radius) center-x) (+ (* y2 radius) center-y)
	    (+ (* c-end radius) center-x) (+ (* s-end radius) center-y))))


(define (distance p1 p2)
  (sqrt (+ (expt (- (first p2)  (first p1))  2)
	   (expt (- (second p2) (second p1)) 2))))

(define (angle2 p1 p2)
  (if (zero? (distance p1 p2))
      0.0
      (atan (- (second p2) (second p1)) (- (first p2) (first p1)))))
  
;;;============================================================================;
;;;
;;; (angle-3points <point> <point> <point>)
;;;
;;; Devuelve el angulo en radianes entre tres puntos.  Se considera el punto
;;; 'pt1' como vertice del angulo.  El rango del angulo de salida es [+Pi -Pi)
;;;

(define (sgn num)
  (cond ((zero? num) num)
	((positive? num) 1.0)
	(else -1.0)))

(define (angle-3points pt1 pt2 pt3)
  (let ((ang (- (angle2 pt1 pt3) (angle2 pt1 pt2))))
    (if (or (> ang pi) (<= ang (- pi)))
	(- ang (* (sgn ang) +2pi+))
	ang)))


;;;============================================================================;
;;;
;;; (midpoint <point> <point> <real>)
;;;
;;; Devuelve un punto situado entre los dos que se dan como argumento. El
;;; factor de posici indica la relaci de las distancias entre los puntos
;;; de entrada y el de salida.
;;;

(define first car)
(define second cadr)

(define (midpoint pt1 pt2 ratio)
  (let ((x1 (first pt1))(y1 (second pt1))
	(x2 (first pt2))(y2 (second pt2)))
    (list (+ x1 (* ratio (- x2 x1)))
	  (+ y1 (* ratio (- y2 y1))))))

;; This function is the support to create rounded polylines
;;
;; p1 = corner
;; p2 = start
;; p3 = end
;; -> no usefull return value
(define (fillet p1 p2 p3 radius)
  (let* ((gamma (/ (abs (angle-3points p1 p2 p3)) 2))
	 (dist-p1-t (/ radius (tan gamma)))
	 (dist-p1-s (/ (sqrt (+ (expt radius 2) (expt dist-p1-t 2)))
		       (cos gamma)))
	 (dist-p1-p2 (distance p1 p2))
	 (dist-p1-p3 (distance p1 p3)))
    (if (or (< dist-p1-p2 dist-p1-t)
	    (< dist-p1-p3 dist-p1-t))
	;; Radius is too large, so we aren't going to draw the arc.
	(line-to (first p1) (second p1))
	;; Else, draw the arc.
	(let ((t2 (midpoint p1 p2 (/ dist-p1-t dist-p1-p2)))
	      (t3 (midpoint p1 p3 (/ dist-p1-t dist-p1-p3)))
	      (center (midpoint (midpoint p1 p2 (/ dist-p1-s dist-p1-p2))
				(midpoint p1 p3 (/ dist-p1-s dist-p1-p3))
				0.5)))
	  (line-to (first t2) (second t2))
	  (arc-to (first center) (second center) radius
		  (angle2 center t2) (angle-3points center t2 t3))))))


;; preserve current font for typesetting

(define *typeset-font-name* (make-parameter #f))
(define *typeset-font-size* (make-parameter #f))

(define (set-typeset-font font size)
  (*page-font-name* font)
  (*page-font-size* size)
  (set-font font size))


;; Japanese output utils

(define (draw-jtext str)
  (format (*page-stream*) "[ ")
  (for-each
    (lambda (x)
      (let ((c (char->integer x)))
	(cond ((>= c 256) 
	       (format (*page-stream*) "<~x> " c))
	      ((or (= c 40) (= c 41) (= c 92))
	       (format (*page-stream*) "(\\~a) " x))
	      (else
	       (format (*page-stream*) "(~a) " x)))))
    str)
  (format (*page-stream*) "] TJ~%"))

(define (draw-combo-text str f1 s1 f2 s2)
  (let ((fcontext 0))
    (for-each
     (lambda (x)
       (let ((c (char->integer x)))
	 (cond ((< c 256)
		(when (equal? fcontext 2) (format (*page-stream*) "] TJ~%"))
		(unless (equal? fcontext 1) (set-font f1 s1) (format (*page-stream*) "[ "))
		(set! fcontext 1)
		(cond ((or (= c 40) (= c 41) (= c 92))
		       (format (*page-stream*) "(\\~a) " x))
		      (else
		       (format (*page-stream*) "(~a) " x))))
	       (else
		(when (equal? fcontext 1) (format (*page-stream*) "] TJ~%"))
		(unless (equal? fcontext 2) (set-font f2 s2) (format (*page-stream*) "[ "))
		(set! fcontext 2)
		(format (*page-stream*) "<~x> " c)))))
     str)
  (format (*page-stream*) "] TJ~%")))


;; typesetting

(define (find-break w str)
  (define (iter len count wacc)
    (cond ((= len count) len)
	  ((< w wacc) (- count 1))
	  (else (iter len (+ count 1) (+ wacc (width (string-ref str count)))))))
  (iter (string-length str) 0 0))

(define (find-break-with-rule w str)
  (define (iter count)
    (let ((last-char (string-ref str (- count 1) #\null))
	  (top-char (string-ref str count #\null)))
      (cond ((<= count 1) count)
	    ((string-index "¡£¡¢¡¤¡¥¡ª¡©¡×¡Ù¡Û¡Í¡Ñ¡Ó¡Õ¡Ï" top-char)
	     (iter (- count 1)))
	    ((string-index "¡Ö¡Ø¡Ú¡Ú¡Ì¡Ð¡Ò¡Ô¡Î" last-char)
	     (iter (- count 1)))
	    ((and (char-alphabetic? last-char) (char-alphabetic? top-char))
	     (iter (- count 1)))
	    (else count))))
  (iter (find-break w str)))

(define (break-into-lines w str)
  (define (iter str vbox)
    (cond ((string-null? str) (reverse vbox))
	  (else 
	   (let ((c (find-break-with-rule w str)))
	     (iter (string-drop str c) (cons (string-take str c) vbox))))))
  (iter str '()))

(define (break-into-pages h lines)
  (slices lines 24))


(define (draw-char f s c)
  (let ((code (char->integer c)))
    (if (< code 256)
	(cond ((or (= code 40) (= code 41) (= code 92))
	       (format (*page-stream*) "~a ~a.00 Tf [ (\\~a) ] TJ " f s c) )
	      (else 
	       (format (*page-stream*) "~a ~a.00 Tf [ (~a) ] TJ " f s c) ))
	(format (*page-stream*) "~a ~a.00 Tf [ <~x> ] TJ " f s code) )))



(provide "kahua/pdf/util")
