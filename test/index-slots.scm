;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots
;;
;; $Id: index-slots.scm,v 1.2 2006/11/27 07:18:37 bizenn Exp $

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

(test* "Initial index cache status" '() (dump-index-cache <index-test>) eq?)

(test-section "Create instances")

;; from creating instances to committing them
(with-db (_ *dbname*)
  (test-section "Collect them before committing")
  (test* "No initialize arguments" <index-test> (class-of (make <index-test>)) eq?)
  (for-each (lambda (a)
	      (test* (format "initialiize :a ~s :b \"b0\"" a)
		     <index-test> (class-of (make <index-test> :a a :b "b0")) eq?))
	    '("a0" "a1" "a2" "a3" "a4"))
  (for-each (lambda (a)
	      (test* (format "make-kahua-collection for unique index ~s" a)
		     `(,a) (map (cut slot-ref <> 'a)
				(make-kahua-collection <index-test> :index `(a . ,a)))
		     equal?))
	    '("a" "a0" "a1" "a2" "a3" "a4"))
  (test* "make-kahua-collection for instance that doesn't exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(a . "aa"))) eq?)
  (test* "make-kahua-collection for any index \"b\""
	 '("a") (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b"))) equal?)
  (test* "make-kahua-collection for any index \"b0\""
	 '("a0" "a1" "a2" "a3" "a4")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	 equal?)
  (test* "make-kahua-collection for instance that doesn't exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(b . "bb"))))
  (test-section "Modify index value before committing")
  (let ((o (car (map identity (make-kahua-collection <index-test> :index '(a . "a")))))
	(o1 (car (map identity (make-kahua-collection <index-test> :index '(a . "a3")))))
	(o2 (car (map identity (make-kahua-collection <index-test> :index '(a . "a4"))))))
    (set! (ref o 'a) "a5")
    (set! (ref o1 'b) "b1")
    (set! (ref o2 'b) "b1")
    (test* "make-kahua-collection by old value w/ unique index"
	   '() (map identity (make-kahua-collection <index-test> :index '(a . "a"))) eq?)
    (test* "make-kahua-collection by new value w/ unique index"
	   '("a5") (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(a . "a5"))) equal?)
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
    (test* "make-kahua-collection for removed instance"
	   '() (map identity (make-kahua-collection <index-test> :index '(a . "a5"))) eq?)
    (test* "make-kahua-collection for removed instance"
	   '() (map identity (make-kahua-collection <index-test> :index '(b . "b"))) eq?)
    (test* "make-kahua-collection for removed instance w/ :include-removed-object? #t"
	   '("a5") (map (cut slot-ref <> 'a)
			(make-kahua-collection <index-test> :index '(a . "a5")
					       :include-removed-object? #t)) equal?)))

;; access to saved instances
(with-db (_ *dbname*)
  (test-section "Maybe on cache")
  (test-section "Collect instances by unique index")
  (for-each (lambda (a)
	      (test* (format "make-kahua-collection by (a . ~s) w/ unique index" a)
		     `(,a) (map (cut slot-ref <> 'a)
				(make-kahua-collection <index-test> :index `(a . ,a))) equal?))
	    '("a0" "a1" "a2" "a3" "a4"))
  (test* "make-kahua-collection for removed instance"
	 '() (map identity (make-kahua-collection <index-test> :index '(a . "a5"))) eq?)
  (test* "make-kahua-collection for removed instance w/ :include-removed-object? #t"
	 '("a5") (map (cut slot-ref <> 'a)
		      (make-kahua-collection <index-test> :index '(a . "a5")
					     :include-removed-object? #t)) equal?)
  (test* "make-kahua-collection for instance does'nt exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(a . "a6"))) eq?)
  (test-section "Collect instances by any index")
  (test* "make-kahua-collection by (b . \"b0\") w/ any index"
	 '("a0" "a1" "a2")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	 equal?)
  (test* "make-kahua-collection by (b . \"b1\" w/ any index"
	 '("a3" "a4")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b1"))))
	 equal?)
  (test* "make-kahua-collection for instance does'nt exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(b . "b2"))) eq?)

  (kahua-db-purge-objs)			; Clear on-memory cache
  (test-section "Maybe on disk")
  (test-section "Collect instances by unique index")
  (for-each (lambda (a)
	      (test* (format "make-kahua-collection by (a . ~s) w/ unique index" a)
		     `(,a) (map (cut slot-ref <> 'a)
				(make-kahua-collection <index-test> :index `(a . ,a))) equal?))
	    '("a0" "a1" "a2" "a3" "a4"))

  (test* "make-kahua-collection for removed instance"
	 '() (map identity (make-kahua-collection <index-test> :index '(a . "a5"))) eq?)
  (test* "make-kahua-collection for removed instance w/ :include-removed-object? #t"
	 '("a5") (map (cut slot-ref <> 'a)
		      (make-kahua-collection <index-test> :index '(a . "a5")
					     :include-removed-object? #t)) equal?)
  (test* "make-kahua-collection for instance does'nt exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(a . "a6"))) eq?)
  (test-section "Collect instances by any index")
  (test* "make-kahua-collection by (b . \"b0\") w/ any index"
	 '("a0" "a1" "a2")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b0"))))
	 equal?)
  (test* "make-kahua-collection by (b . \"b1\" w/ any index"
	 '("a3" "a4")
	 (sort! (map (cut slot-ref <> 'a) (make-kahua-collection <index-test> :index '(b . "b1"))))
	 equal?)
  (test* "make-kahua-collection for instance does'nt exist"
	 '() (map identity (make-kahua-collection <index-test> :index '(b . "b2"))) eq?)

  (let ((o (car (map identity (make-kahua-collection <index-test> :index '(a . "a0")))))
	(o1 (car (map identity (make-kahua-collection <index-test> :index '(a . "a1")))))
	(o2 (car (map identity (make-kahua-collection <index-test> :index '(a . "a2"))))))
    (set! (ref o 'a) "a5")
    (set! (ref o1 'a) "a6")
    (set! (ref o2 'a) "a7")))

(with-db (_ *dbname*)
  (kahua-db-purge-objs)			; Clear on-memory cache
  (test-section "Maybe on disk again")
  (for-each (lambda (a result)
	      (test* (format "make-kahua-collection by (a . ~s) w/ unique index" a)
		     result (map (cut slot-ref <> 'a)
				 (make-kahua-collection <index-test> :index `(a . ,a))) equal?))
	    '("a0" "a1" "a2"  "a3"   "a4"   "a5"   "a6"   "a7")
	    '(()   ()   ()   ("a3") ("a4") ("a5") ("a6") ("a7")))
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
  (kahua-db-purge-objs)			; Clear on-memory cache
  (test-section "Maybe on disk again")
  (for-each (lambda (a result)
	      (test* (format "make-kahua-collection by (a . ~s) w/ unique index" a)
		     result (map (cut slot-ref <> 'a)
				 (make-kahua-collection <index-test> :index `(a . ,a))) equal?))
	    '("a0" "a1" "a2"  "a3"   "a4"   "a5"   "a6"   "a7")
	    '(()   ()   ()   ("a3") ("a4") ("a5") ("a6") ("a7")))
  (for-each (lambda (b result)
	      (test* (format "make-kahua-collection by (b . ~s) w/ any index" b)
		     result (sort! (map (cut slot-ref <> 'a)
					(make-kahua-collection <index-test> :index `(b . ,b)))) equal?))
	    '("b0" "b1")
	    '(("a5" "a6" "a7") ("a3" "a4"))))


(test-end)
