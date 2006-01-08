;; Serialize module
;;
;; Don't use this module:-)  Experimental!
;;
;; $Id: serialize.scm,v 1.1 2006/01/08 06:48:48 tahara Exp $

(define-module kahua.serialize
  (export serialize-string deserialize-string)
  )

(select-module kahua.serialize)

;;TODO
;;defined module
;;stack
;;bytecode
;;cycle reference
;;vector

(define-module 

;; code
(define INST #\i)
(define SETSLOT #\s)
(define CONS #\c)
(define PUT  #\p)
(define APPLY #\a)
(define MARK #\#)
(define STOP #\.)

(define BOOL #\B)
(define PAIR #\P)
(define NIL  #\N)
(define SYMBOL #\Y)
(define STRING #\S)
(define NUMBER #\I)
(define CHAR #\C)

;; serializer
(define (serializer port object)
  (with-output-to-port port
    (lambda ()
      (%serializer object)
      (display "."))))

(define (%serializer o)
    (cond ((pair? o) (save-pair o))
	  ((number? o) (save-number o))
	  ((symbol? o) (save-symbol o))
	  ((string? o) (save-string o))
	  ((char? o) (save-char o))
	  ((null? o) (save-nil o))
	  ((boolean? o) (save-bool o))
	  ((equal? (ref (class-of o) 'category) 'scheme) (save-inst o))
	  (else (error "Can't serialize object:" o))))

(define (save-inst o)
  (let ((class (class-of o)))
    (print MARK)
    (print INST
	   (string-join (map x->string
			     (list (module-name (car (ref class 'defined-modules)))
				   " " (class-name class)))
			"")
     )
    (for-each (lambda (name)
		(if (slot-bound? o name)
		    (begin (%serializer name)
			   (%serializer (ref o name))
			   (print SETSLOT))))
	      (map car (class-slots class)))
    ))

(define (save-number o)
  (print NUMBER (number->string o)))

(define (save-symbol o)
  (print SYMBOL (symbol->string o)))

(define (save-string o)
  (print STRING (quote-string o)))

(define (save-char o)
  (print CHAR o))

(define (save-bool o)
  (print BOOL (if (eq? o #t) "t" "f")))

(define (save-nil o)
  (print NIL))

(define (save-pair o)
  (print MARK)
  (%serializer (car o))
  (%serializer (cdr o))
  (print CONS))


;; deserializer
(define (deserializer port)
  (let ((stack (make <stack>)))
    (define (iter)
      (let ((i (read-char port)))
	(if (equal? i STOP)
	    (pop stack)
	    (begin
	      (print i)
	      ((cond ((eq? i INST) load-inst)
		     ((eq? i SYMBOL) load-symbol)
		     ((eq? i STRING) load-string)
		     ((eq? i NUMBER) load-number)
		     ((eq? i CHAR) load-char)
		     ((eq? i BOOL) load-bool)
		     ((eq? i NIL) load-nil)
		     ((eq? i SETSLOT) load-setslot)
		     ((eq? i CONS) load-cons)
		     ((eq? i MARK) load-mark))
	       (read-line port) stack)
	      (iter)))))
    (iter)))

(define (load-inst data stack)
  (let* ((tmp (string-split data " "))
	 (module (string->symbol (car tmp)))
	 (class (string->symbol (cadr tmp))))
    (push stack (allocate-instance (eval class (find-module module)) ()))))

(define (load-symbol data stack)
  (push stack (string->symbol data)))

(define (load-string data stack)
  (push stack (unquote-string data)))

(define (load-number data stack)
  (push stack (string->number data)))

(define (load-char data stack)
  (push stack (car (string->list data))))

(define (load-bool data stack)
  (push stack (if (equal? data "t") #t #f)))

(define (load-nil data stack)
  (push stack ()))

(define (load-setslot data stack)
  (let ((top (topobject stack)))
    (define (iter)
      (let ((value (pop stack))
	    (name (pop stack))
	    (n (next stack)))
	(slot-set! top name value)
	(if (not (eq? n top))
	    (iter))))
    (iter)))

(define (load-cons data stack)
  (let ((cdr-obj (pop stack))
	(car-obj (pop stack)))
    (push stack (cons car-obj cdr-obj))))

(define (load-mark data stack)
  (set! mark *mark*))


;;frame&mark
(define-class <mark> () ())
(define *mark* (make <mark>))
(define *unmark* 'unmark)
(define mark *unmark*)
(define (frame-value frame) (cdr frame))
(define (frame-mark frame) (car frame))
(define (make-frame object)
  (let ((frame (cons mark object)))
    (set! mark *unmark*)
    frame))

;;stack
(define-class <stack> ()
  ((frames :init-value ())))
(define-method initialize ((self <stack>))
  (slot-set! self 'frames ()))
(define-method push ((self <stack>) value)
  (let ((frames (ref self 'frames)))
    (slot-set! self 'frames (cons (make-frame value) frames))
    (print "*push* " value)
    ))
(define-method pop ((self <stack>))
  (let* ((frames (ref self 'frames))
	 (f (car frames))
	 (value (frame-value f)))
    (slot-set! self 'frames (cdr frames))
    (print "*pop* " value)
    value
    ))
(define-method next ((self <stack>))
  (let ((frames (ref self 'frames)))
    (if (null? frames)
	#f
	(frame-value (car frames)))))
(define-method topobject ((self <stack>))
  (define (iter frames)
    (if (null? frames)
	#f
	(let ((f (car frames))
	      (rest (cdr frames)))
	  (if (or (eq? (frame-mark f) *mark*) (null? rest))
	      (begin
		(print "*top* " (cdr f))
		(frame-value f))
	      (iter rest)))))
  (iter (ref self 'frames)))

;;string
(define (quote-string string)
  (string-join (string-split string "\n") "\\n"))
(define (unquote-string string)
  (string-join (string-split string "\\n") "\n"))

;; public api
(define (serialize-string object)
  (let ((port (open-output-string)))
    (serializer port object)
    (get-output-string port)
    ))
(define (deserialize-string string)
  (let ((port (open-input-string string)))
    (deserializer port)))

(provide "kahua/serialize")

;; sample code
; (define-class <container> ()
;   ((name :init-keyword :name)
;    (items :init-value ())
;    ))
; (define-method items ((self <container>))
;   (map (lambda (o) (list (ref o 'name) o))
;        (ref self 'items)))
; (define-method add ((self <container>) object)
;   (let ((items (ref self 'items)))
;     (slot-set! self 'items (cons object items))))
; (define-class <page> ()
;   ((name :init-keyword :name)
;    (source :init-value "" :init-keyword :source)
;    ))
; (define folder (make <container> :name "root"))
; (add folder (make <page> :name "index.html" :source "<html>INDEX.HTML</html>"))
; (add folder (make <page> :name "main.html" :source "<html>MAIN.HTML</html>"))
; (items folder)
; (serialize-string folder)
; (define folder (deserialize-string "#\niuser <container>\nYname\nSroot\ns\nYitems\n#\n#\niuser <page>\nYname\nSmain.html\ns\nYsource\nS<html>MAIN.HTML</html>\ns\n#\n#\niuser <page>\nYname\nSindex.html\ns\nYsource\nS<html>INDEX.HTML</html>\ns\nN\nc\nc\ns\n."))
; (items folder)
