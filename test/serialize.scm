;; Test kahua.serialize module
;; $Id: serialize.scm,v 1.2 2006/01/10 13:07:42 tahara Exp $

(use gauche.test)

(test-start "kahua.serialize")
(use kahua.serialize)
(test-module 'kahua.serialize)

;;---------------------------------------------------------------
(test-section "basic types")

(test* "string" "Kahua is a web application framework."
       (deserialize-string
	(serialize-string "Kahua is a web application framework.")))

(test* "integer" 12345678901234567890
       (deserialize-string (serialize-string 12345678901234567890)))

(test* "integer" -12345678901234567890
       (deserialize-string (serialize-string -12345678901234567890)))

(test* "real" 3.14159265358979323846
       (deserialize-string (serialize-string 3.14159265358979323846)))

(test* "complex" 1+0.3i
       (deserialize-string (serialize-string 1+0.3i)))

(test* "boolean" #t
       (deserialize-string (serialize-string #t)))

(test* "boolean" #f
       (deserialize-string (serialize-string #f)))

(test* "pair" '(1 2 3)
       (deserialize-string (serialize-string '(1 2 3))))

(test* "pair" '(1 (2 . 3) 4 (5 6 (7) 8.0) -9 "end")
       (deserialize-string
	(serialize-string '(1 (2 . 3) 4 (5 6 (7) 8.0) -9 "end"))))

(test* "nil" ()
       (deserialize-string (serialize-string ())))

(test* "vector" (vector 1 2 (vector 3 4) 5)
       (deserialize-string (serialize-string (vector 1 2 (vector 3 4) 5))))

(test* "symbol" 'Kahua
       (deserialize-string (serialize-string 'Kahua)))

(test* "char" #\A
       (deserialize-string (serialize-string #\A)))

;;---------------------------------------------------------------
(test-section "unsupported types (part of all)")

(test* "syntax" *test-error*
       (serialize-string define))

(test* "class" *test-error*
       (serialize-string <class>))

(test* "closure" *test-error*
       (serialize-string (lambda ())))

(test* "generic" *test-error*
       (serialize-string make))

(test* "subr" *test-error*
       (serialize-string cons))

(test* "module" *test-error*
       (serialize-string (find-module 'gauche)))

;;---------------------------------------------------------------
(test-section "user defined class instance")

(define-class <person> ()
  ((name :init-keyword :name)
   (age :init-keyword :age)
   (brother)))

(define tarou (make <person> :name "tarou" :age 10))
(define jirou (make <person> :name "jirou" :age 9))
(slot-set! tarou 'brother jirou)

(test* "class" <person>
       (class-of (deserialize-string (serialize-string tarou))))

(test* "slot name" "tarou"
       (ref (deserialize-string (serialize-string tarou)) 'name))

(test* "slot age" 10
       (ref (deserialize-string (serialize-string tarou)) 'age))

(test* "slot brother name" "jirou"
       (ref
	(ref (deserialize-string (serialize-string tarou)) 'brother)
	'name))

;;---------------------------------------------------------------
(test-section "reference")

(define pair1 (list 1 2 3))
(define pair2 (list pair1 pair1 2 3 4 pair1))
(define pair3 (cons 1 2))
(set-cdr! pair3 (cons pair3 pair3))

(test* "pair" #t
       (let ((p (deserialize-string (serialize-string pair2))))
	 (eq? (car p) (cadr p))))

(test* "pair" #t
       (let ((p (deserialize-string (serialize-string pair3))))
	 (eq? p (cadr p))))

(define vector1 (vector 1 2 3))
(define vector2 (vector vector1 vector1))

(test* "vector" #t
       (let ((v (deserialize-string (serialize-string vector2))))
	 (eq? (vector-ref v 0) (vector-ref v 1))))

;;---------------------------------------------------------------
(test-section "cyclic reference")

(define cpair (cons 1 2))
(set-cdr! cpair cpair)

(test* "pair" #t
       (let ((p (deserialize-string (serialize-string cpair))))
	 (eq? p (cdr p))))

(define cvector (vector "1" 2 3 4))
(vector-set! cvector 0 cvector)

(test* "vector" #t
       (let ((v (deserialize-string (serialize-string cvector))))
	 (eq? v (vector-ref v 0))))

;;---------------------------------------------------------------
(test-section "regexp")

(define reg1 (string->regexp "[0-9]" :case-fold #t))
(define reg2 (string->regexp "[a-z]" :case-fold #f))

(test* "<regexp>" <regexp>
       (class-of (deserialize-string (serialize-string reg1))))

(test* "regexp string" "[0-9]"
       (regexp->string (deserialize-string (serialize-string reg1))))

(test* "case-fold is true" #t
       (regexp-case-fold? (deserialize-string (serialize-string reg1))))

(test* "case-fold is false" #f
       (regexp-case-fold? (deserialize-string (serialize-string reg2))))

;;---------------------------------------------------------------
(test-section "hash-table")

(define ht (make-hash-table))
(hash-table-put! ht 1 (list 1 2 3))
(hash-table-put! ht 2 "string1")
(hash-table-put! ht 3 ht)
(hash-table-put! ht 4 tarou)

(test* "<hash-table>" <hash-table>
       (class-of (deserialize-string (serialize-string ht))))

(test* "key check" #t
       (let ((object (deserialize-string (serialize-string ht))))
	 (equal? (sort (hash-table-keys ht))
		 (sort (hash-table-keys object)))))

(test* "value check" (hash-table-get ht 2)
       (let ((object (deserialize-string (serialize-string ht))))
	 (hash-table-get object 2)))

(test* "value check" #t
       (let ((object (deserialize-string (serialize-string ht))))
	 (eq? object (hash-table-get object 3))))

(test* "value check" <person>
       (let ((object (deserialize-string (serialize-string ht))))
	 (class-of (hash-table-get object 4))))

;;---------------------------------------------------------------
(test-section "keyword")

(test* "<keyword>" <keyword>
       (class-of (deserialize-string (serialize-string :kahua))))

(test* "value" :keyword
       (deserialize-string (serialize-string :keyword)))

(test-end)
