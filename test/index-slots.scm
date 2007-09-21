;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots
;;
;; $Id: index-slots.scm,v 1.4.2.2 2007/06/19 05:36:31 bizenn Exp $

(use gauche.test)
(use gauche.collection)
(use srfi-1)

(debug-print-width #f)

(test-start (format "index slots (~a)" ((#/^(\w+):/ *dbname*) 1)))

(use kahua.persistence)
(test-module 'kahua.persistence)

(test-section "Prologue")

(define-class <index-test> (<kahua-persistent-base>)
  ((a :init-keyword :a :init-value "a" :allocation :persistent :index :unique)
   (b :init-keyword :b :init-value "b" :allocation :persistent :index :any)
   (c :init-keyword :c :init-value "c" :allocation :persistent)))

(test-section "Create instances")

;; from creating instances to committing them
(with-db (_ *dbname*)
  (test* "Initial index cache status" '() (dump-index-cache <index-test>) eq?)
  (test-section "Collect them before committing")
  (test* "No initialize arguments" <index-test> (class-of (make <index-test>)) eq?)
  (for-each (lambda (a)
	      (test* (format "initialiize :a ~s :b \"b0\"" a)
		     <index-test> (class-of (make <index-test> :a a :b "b0")) eq?))
	    '("a0" "a1" "a2" "a3" "a4"))
  (for-each (lambda (a)
	      (test* (format "find-kahua-instance for unique index ~s" a)
		     a (slot-ref (find-kahua-instance <index-test> 'a a) 'a) equal?))
	    '("a" "a0" "a1" "a2" "a3" "a4"))
  (test* "find-kahua-instance for instance that doesn't exist"
	 #f (find-kahua-instance <index-test> 'a "aa") eq?)
  (test* "make-kahua-collection for any index \"b\""
	 '("a") (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b"))) equal?)
  (test* "make-kahua-collection for any index \"b0\""
	 '("a0" "a1" "a2" "a3" "a4")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	 equal?)
  (test* "make-kahua-collection for instance that doesn't exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(b . "bb"))))
  (test-section "Modify index value before committing")
  (let ((o (find-kahua-instance <index-test> 'a "a"))
	(o1 (find-kahua-instance <index-test> 'a "a3"))
	(o2 (find-kahua-instance <index-test> 'a "a4")))
    (set! (ref o 'a) "a5")
    (set! (ref o1 'b) "b1")
    (set! (ref o2 'b) "b1")
    (test* "find-kahua-instance by old value w/ unique index"
	   #f (find-kahua-instance <index-test> 'a "a") eq?)
    (test* "find-kahua-instance by new value w/ unique index"
	   "a5" (slot-ref (find-kahua-instance <index-test> 'a "a5") 'a) equal?)
    (test* "make-kahua-collection by \"b0\" w/ any index"
	   '("a0" "a1" "a2")
	   (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	   equal?)
    (test* "make-kahua-collection by \"b1\" w/ any index"
	   '("a3" "a4")
	   (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b1"))))
	   equal?)
    (test-section "Remove instance before committing")
    (remove-kahua-instance o)
    (test* "find-kahua-instance for removed instance"
	   #f (find-kahua-instance <index-test> 'a "a5") eq?)
    (test* "make-kahua-collection for removed instance"
	   '() (map identity (make-kahua-collection <index-test> :index '(b . "b"))) eq?)
    (test* "find-kahua-instance for removed instance w/ :include-removed-object? #t"
	   "a5" (slot-ref (find-kahua-instance <index-test> 'a "a5" #t) 'a) equal?)
    (test* "make-kahua-collection for removed instance w/ :include-removed-object? #t"
	   '("a5") (map (cut slot-ref <> 'a)
			(make-kahua-collection <index-test> :index '(b . "b")
					       :include-removed-object? #t)) equal?)
    ))

;; access to saved instances
(with-db (_ *dbname*)
  (test-section "Maybe on cache")
  (test-section "Collect instances by unique index")
  (for-each (lambda (a)
	      (test* (format "find-kahua-instance w/ a ~s on unique index" a)
		     a (slot-ref (find-kahua-instance <index-test> 'a a) 'a) equal?))
	    '("a0" "a1" "a2" "a3" "a4"))
  (test* "find-kahua-instance for removed instance"
	 #f (find-kahua-instance <index-test> 'a "a5") eq?)
  (test* "find-kahua-instance for removed instance w/ :include-removed-object? #t"
	 "a5" (slot-ref (find-kahua-instance <index-test> 'a "a5" #t) 'a) equal?)
  (test* "find-kahua-instance for instance doesn't exist"
	 #f (find-kahua-instance <index-test> 'a "a6") eq?)
  (test-section "Collect instances by any index")
  (test* "make-kahua-collection by (b . \"b0\") w/ any index"
	 '("a0" "a1" "a2")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	 equal?)
  (test* "make-kahua-collection by (b . \"b1\" w/ any index"
	 '("a3" "a4")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b1"))))
	 equal?)
  (test* "make-kahua-collection for instance doesn't exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(b . "b2"))) eq?)

  (kahua-db-purge-objs)			; Clear on-memory cache
  (test-section "Maybe on disk")
  (test-section "Collect instances by unique index")
  (for-each (lambda (a)
	      (test* (format "find-kahua-instance w/ a ~s on unique index" a)
		     a (slot-ref (find-kahua-instance <index-test> 'a a) 'a) equal?))
	    '("a0" "a1" "a2" "a3" "a4"))

  (test* "find-kahua-instance for removed instance"
	 #f (find-kahua-instance <index-test> 'a "a5") eq?)
  (test* "find-kahua-instance for removed instance w/ :include-removed-object? #t"
	 "a5" (slot-ref (find-kahua-instance <index-test> 'a "a5" #t) 'a) equal?)
  (test* "find-kahua-instance for instance doesn't exist"
	 #f (find-kahua-instance <index-test> 'a "a6") eq?)
  (test-section "Collect instances by any index")
  (test* "make-kahua-collection by (b . \"b0\") w/ any index"
	 '("a0" "a1" "a2")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	 equal?)
  (test* "make-kahua-collection by (b . \"b1\" w/ any index"
	 '("a3" "a4")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b1"))))
	 equal?)
  (test* "make-kahua-collection for instance doesn't exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(b . "b2"))) eq?)

  (let ((o (find-kahua-instance <index-test> 'a "a0"))
	(o1 (find-kahua-instance <index-test> 'a "a1"))
	(o2 (find-kahua-instance <index-test> 'a "a2")))
    (set! (ref o 'a) "a5")
    (set! (ref o1 'a) "a6")
    (set! (ref o2 'a) "a7")))

(with-db (_ *dbname*)
  (kahua-db-purge-objs)			; Clear on-memory cache
  (test-section "Maybe on disk again")
  (for-each (lambda (a result)
	      (test* (format "find-kahua-instance w/ a ~s on unique index" a)
		     result (and-let* ((obj (find-kahua-instance <index-test> 'a a)))
			      (slot-ref obj 'a))
		     equal?))
	    '("a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8")
	    '(#f   #f   #f   "a3" "a4" "a5" "a6" "a7" #f))
  (for-each (lambda (b result)
	      (test* (format "make-kahua-collection by (b . ~s) w/ any index" b)
		     result (sort! (map (cut slot-ref <> 'a)
					(make-kahua-collection <index-test> :index `(b . ,b)))) equal?))
	    '("b0" "b1")
	    '(("a5" "a6" "a7") ("a3" "a4"))))

(define-class <index-test> (<kahua-persistent-base>)
  ((a :init-keyword :a :init-value "a" :allocation :persistent :index :any)
   (b :init-keyword :b :init-value "b" :allocation :persistent :index :any)
   (c :init-keyword :c :init-value "c" :allocation :persistent)))

(test-section "Class Redifinition (index change)")
(with-db (_ *dbname*)
  (test-section "Maybe on disk again")
  (for-each (lambda (a result)
	      (test* (format "make-kahua-collection by (a . ~s) w/ any index" a)
		     result (map (cut slot-ref <> 'a)
				 (make-kahua-collection <index-test> :index `(a . ,a))) equal?))
	    '("a0" "a1" "a2"  "a3"   "a4"   "a5"   "a6"   "a7" "a8")
	    '(()   ()   ()   ("a3") ("a4") ("a5") ("a6") ("a7") ()))
  (for-each (lambda (b result)
	      (test* (format "make-kahua-collection by (b . ~s) w/ any index" b)
		     result (sort! (map (cut slot-ref <> 'a)
					(make-kahua-collection <index-test> :index `(b . ,b)))) equal?))
	    '("b0" "b1")
	    '(("a5" "a6" "a7") ("a3" "a4"))))

(define-class <index-test> (<kahua-persistent-base>)
  ((a :init-keyword :a :init-value "a" :allocation :persistent)
   (b :init-keyword :b :init-value "b" :allocation :persistent)
   (c :init-keyword :c :init-value "c" :allocation :persistent :index :any)))

(with-db (_ *dbname*)
  (test* "make-kahua-collection by (c . \"c\") w/ any index"
	 '("a3" "a4" "a5" "a6" "a7")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(c . "c")))) equal?))

(define-class <index-test> (<kahua-persistent-base>)
  ((a :init-keyword :a :init-value "a" :allocation :persistent :index :unique)
   (b :init-keyword :b :init-value "b" :allocation :persistent)
   (c :init-keyword :c :init-value "c" :allocation :persistent)))

(with-db (_ *dbname*)
  (for-each (lambda (a result)
	      (test* (format "find-kahua-instance w/ a ~s on unique index" a)
		     result (and-let* ((obj (find-kahua-instance <index-test> 'a a)))
			      (slot-ref obj 'a))
		     equal?))
	    '("a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8")
	    '(#f   #f   #f   "a3" "a4" "a5" "a6" "a7" #f))
  )

(define-class <index-test> (<kahua-persistent-base>)
  ((a :init-keyword :a :allocation :persistent :index :unique)
   (b :init-keyword :b :allocation :persistent :index :any)
   (c :init-keyword :c :allocation :persistent)))

(with-db (_ *dbname*)
  (make <index-test> :a "a0" :b #f :c "boolean")
  (make <index-test> :a "a1" :b 1  :c "integer")
  (make <index-test> :a "a2" :b "b" :c "string")
  (let1 obj (find-kahua-instance <index-test> 'a "a3")
    (slot-set! obj 'b 'b)
    (slot-set! obj 'c "symbol"))
  (let1 obj (find-kahua-instance <index-test> 'a "a4")
    (slot-set! obj 'b :b)
    (slot-set! obj 'c "keyword"))
  (let1 obj (find-kahua-instance <index-test> 'a "a5")
    (slot-set! obj 'b obj)
    (slot-set! obj 'c "self"))
  (let1 obj (find-kahua-instance <index-test> 'a "a6")
    (slot-set! obj 'b '("a" "b" "c"))
    (slot-set! obj 'c "list"))
  (let1 obj (find-kahua-instance <index-test> 'a "a7")
    (slot-set! obj 'b '#("a" "b" "c"))
    (slot-set! obj 'c "vector"))
  )

(with-db (_ *dbname*)
  (for-each (lambda (b result)
	      (test* "index value" result
		     (map (cut slot-ref <> 'a)
			  (make-kahua-collection <index-test> :index `(b . ,b)))
		     equal?))
	    '(#f 1 "b" b :b ("a" "b" "c") #("a" "b" "c"))
	    '(("a0") ("a1") ("a2") ("a3") ("a4") ("a6") ("a7")))
  (let1 obj (find-kahua-instance <index-test> 'a "a5")
    (test* "self as index value" (list obj)
	   (map identity (make-kahua-collection <index-test> :index `(b . ,obj)))
	   equal?))
  )

(test-end)
