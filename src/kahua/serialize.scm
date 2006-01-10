;; Serialize module
;;
;; Please don't use this module for production:-)
;; Bytecode will change in future version.
;;
;; $Id: serialize.scm,v 1.8 2006/01/10 15:30:43 tahara Exp $

(define-module kahua.serialize
  (export serialize-string deserialize-string extension-register)
  )

(select-module kahua.serialize)

;;TODO
;;special slot accessor

;; code
(define MARK #\#)
(define STOP #\.)

(define APPLY #\a)
(define CONS #\c)
(define DEFROST #\d)
(define INST #\i)
(define PUT  #\p)
(define GET  #\g)
(define SETCDR #\r)
(define SLOTSET #\s)
(define VECTORSET #\v)

(define BOOL #\B)
(define CHAR #\C)
(define NUMBER #\I)
(define NIL  #\N)
(define PAIR #\P)
(define STRING #\S)
(define VECTOR #\V)
(define SYMBOL #\Y)


;;memorization
(define *memo* (make-hash-table))
(define (memo-initialize) (set! *memo* (make-hash-table)))
(define (memo-append object)
  (let ((length (hash-table-num-entries *memo*)))
    (hash-table-put! *memo* object length)
    length))
(define (memo-exists? object)
  (hash-table-exists? *memo* object))
(define (memo-index object)
  (hash-table-get *memo* object))
(define (memo-put index object)
  (hash-table-put! *memo* index object))
(define (memo-get index)
  (hash-table-get *memo* index))
  

;;extention dispatcher
(define *extension-table* (make-hash-table))
(define (extension-register class dumper loader . args)
  (let-optionals* args ((builder #f))
		  (hash-table-put! *extension-table*
				   class (list dumper builder loader))))
(define (extension-get class) (hash-table-get *extension-table* class))
(define (extension-dumper ext) (car ext))
(define (extension-builder ext) (cadr ext))
(define (extension-loader ext) (caddr ext))
(define (serializable? obj)
  (call/cc (lambda (c)
	     (for-each (lambda (class)
			 (if (is-a? obj class)
			     (c #t)))
		       (hash-table-keys *extension-table*))
	     #f)))

;; serializer
(define (serialize port object)
  (memo-initialize)
  (with-output-to-port port
    (lambda ()
      (%serialize object)
      (display STOP))))

(define (%serialize o)
    (cond ((pair? o) (save-pair o))
	  ((number? o) (save-number o))
	  ((symbol? o) (save-symbol o))
	  ((string? o) (save-string o))
	  ((char? o) (save-char o))
	  ((null? o) (save-nil o))
	  ((boolean? o) (save-bool o))
	  ((vector? o) (save-vector o))
	  ((equal? (ref (class-of o) 'category) 'scheme) (save-inst o))
	  ((serializable? o) (save-extension o))
	  (else (error "Can't serialize object:" o))))

(define (binding-name class)
  (let* ((defined-modules (ref class 'defined-modules))
	 (modname (if (pair? defined-modules)
		      (module-name (car defined-modules))
		      (find-binding-module-name class))))
    (string-join (list (symbol->string modname)  " "
		       (symbol->string (class-name class)))
		 "")))

(define gauche.internal (find-module 'gauche.internal))
(define find-binding (eval 'find-binding gauche.internal))
(define gloc-ref (eval 'gloc-ref gauche.internal))
(define (find-binding-module-name class)
  (let ((name (class-name class)))
    (call/cc
     (lambda (c)
       (for-each (lambda (module)
                   (let ((gloc (find-binding module name #f)))
                     (if (and gloc (eq? (gloc-ref gloc) class))
                         (c (module-name module)))))
                 (all-modules))
       #f))))


(define (save-inst o)
  (if (memo-exists? o)
      (print GET (memo-index o))
      (begin
	(let ((class (class-of o)))
	  (print MARK)
	  (print INST (binding-name class))
	  (print PUT (memo-append o))
	  (for-each (lambda (name)
		      (if (slot-bound? o name)
			  (begin (%serialize name)
				 (%serialize (ref o name))
				 (print SLOTSET))))
		    (map car (class-slots class)))
	  ))
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
  (if (memo-exists? o)
      (print GET (memo-index o))
      (let ((cdr-obj (cdr o)))
	(set-cdr! o ())
	(print MARK)
	(%serialize (car o))
	(print NIL)
	(print CONS)
	(print PUT (memo-append o))
	(%serialize cdr-obj)
	(print SETCDR)
	(set-cdr! o cdr-obj)
	)))

(define (save-vector o)
  (if (memo-exists? o)
      (print GET (memo-index o))
      (begin
	(let ((length (vector-length o)))
	  (define (iter n)
	    (if (< n length)
		(begin
		  (%serialize (vector-ref o n))
		  (print VECTORSET n)
		  (iter (+ n 1)))))
	  (print MARK)
	  (print VECTOR length)
	  (print PUT (memo-append o))
	  (iter 0)
	  ))
	))

(define (save-extension o)
  (if (memo-exists? o)
      (print GET (memo-index o))
      (begin
	(let* ((class (class-of o))
	       (ext (extension-get class))
	       (name (binding-name class)))
	  (print DEFROST name)
	  (print PUT (memo-append o))
	  (save-pair ((extension-dumper ext) o))
	  (if (extension-builder ext)
	      (print CONS))
	  (print APPLY)
	))
      ))


;; deserializer
(define (deserialize port)
  (memo-initialize)
  (let ((stack (make <stack>)))
    (define (iter)
      (let ((i (read-char port)))
	(if (equal? i STOP)
	    (pop stack)
	    (begin
	      ((cond ((eq? i INST) load-inst)
		     ((eq? i SYMBOL) load-symbol)
		     ((eq? i STRING) load-string)
		     ((eq? i NUMBER) load-number)
		     ((eq? i CHAR) load-char)
		     ((eq? i BOOL) load-bool)
		     ((eq? i NIL) load-nil)
		     ((eq? i SLOTSET) load-slotset)
		     ((eq? i CONS) load-cons)
		     ((eq? i SETCDR) load-setcdr)
		     ((eq? i VECTOR) load-vector)
		     ((eq? i VECTORSET) load-vectorset)
		     ((eq? i MARK) load-mark)
		     ((eq? i DEFROST) load-defrost)
		     ((eq? i APPLY) load-apply)
		     ((eq? i PUT) load-put)
		     ((eq? i GET) load-get)
		     (else (error "Unknown code" i)))
	       (read-line port) stack)
	      (iter)))))
    (iter)))

(define (binding-name-object data)
  (let* ((tmp (string-split data " "))
	 (module (string->symbol (car tmp)))
	 (class (string->symbol (cadr tmp))))
    (eval class (find-module module))))

(define (load-inst data stack)
    (push stack (allocate-instance (binding-name-object data) ())))

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

(define (load-slotset data stack)
  (let ((value (pop stack))
	(name (pop stack))
	(top (topobject stack)))
      (slot-set! top name value)))

(define (load-cons data stack)
  (let ((cdr-obj (pop stack))
	(car-obj (pop stack)))
    (push stack (cons car-obj cdr-obj))))

(define (load-setcdr data stack)
  (let ((cdr-obj (pop stack))
	(c (pop stack)))
    (set-cdr! c cdr-obj)
    (push stack c)))

(define (load-vector data stack)
  (push stack (make-vector (string->number data))))

(define (load-vectorset data stack)
  (let ((index (string->number data))
	(value (pop stack))
	(top (topobject stack)))
    (vector-set! top index value)))

(define (load-mark data stack)
  (set! mark *mark*))

(define (load-defrost data stack)
  (let* ((class (binding-name-object data))
	 (ext (extension-get class))
	 (loader (extension-loader ext))
	 (builder (extension-builder ext)))
    (push stack loader)
    (if builder
	(push stack (builder)))))

(define (load-apply data stack)
  (let ((args (pop stack))
	(proc (pop stack)))
    (push stack (apply proc args))))

(define (load-put data stack)
  (memo-put (string->number data) (frame-value (last stack))))

(define (load-get data stack)
  (push stack (memo-get (string->number data))))


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
    ))
(define-method pop ((self <stack>))
  (let* ((frames (ref self 'frames))
	 (f (car frames))
	 (value (frame-value f)))
    (slot-set! self 'frames (cdr frames))
    value
    ))
(define-method next-mark? ((self <stack>))
  (let ((frames (ref self 'frames)))
    (if (null? frames)
	#f
	(eq? *mark* (frame-mark (car frames))))))
(define-method topobject ((self <stack>))
  (define (iter frames)
    (if (null? frames)
	#f
	(let ((f (car frames))
	      (rest (cdr frames)))
	  (if (or (eq? (frame-mark f) *mark*) (null? rest))
	      (begin
		(frame-value f))
	      (iter rest)))))
  (iter (ref self 'frames)))
(define-method last ((self <stack>))
  (car (ref self 'frames)))

;;string
(define (quote-string string)
  (string-join (string-split string "\n") "\\n"))
(define (unquote-string string)
  (string-join (string-split string "\\n") "\n"))

;; public api
(define (serialize-string object)
  (let ((port (open-output-string)))
    (serialize port object)
    (get-output-string port)
    ))
(define (deserialize-string string)
  (let ((port (open-input-string string)))
    (deserialize port)))

;;extension regexp
(define (regexp-dump regexp)
  (list (regexp->string regexp) (regexp-case-fold? regexp)))
(define (regexp-load string case-fold)
  (string->regexp string :case-fold case-fold))
(extension-register <regexp> regexp-dump regexp-load)

;;extension hash-table
(define (hash-table-dump ht)
  (list (hash-table-type ht)
	(hash-table-map ht (lambda (k v) (cons k v)))))
(define (hash-table-load self type items)
  (for-each (lambda (item)
	      (hash-table-put! self (car item) (cdr item)))
	    items)
  self)
(extension-register <hash-table> hash-table-dump hash-table-load make-hash-table)

;;extension keyword
(define (keyword-dump k) (list (keyword->string k)))
(define (keyword-load k) (make-keyword k))
(extension-register <keyword> keyword-dump keyword-load)

(provide "kahua/serialize")
