;; -*- coding: utf-8 ; mode: scheme -*-
;;; Copyright (c) 2004 Hisazumi, Kenji. All rights reserved.

(use gauche.test)
(use gauche.collection)
(use file.util)
(use util.list)
(use srfi-1)
(use srfi-2)
(use kahua.plugin)
(use kahua.config)

(test-start "query plugin")

(define *site* "_site")

(test-section "initialization")
(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(copy-file "../plugins/query.scm" #`",|*site*|/plugins/query.scm")
(kahua-common-init *site* #f)

(initialize-plugins (kahua-plugin-directory))
(use-plugin query)
(use kahua.persistence)

(define *dbname* (kahua-dbpath "efs:db"))

(define-class <address> (<kahua-persistent-base>)
  ((address :allocation :persistent :init-keyword :address)
   (name    :allocation :persistent :init-keyword :name)
   ))

(define-class <numnum> (<kahua-persistent-base>)
  ((num :allocation :persistent :init-keyword :num)))

(define-class <group> (<kahua-persistent-base>)
  ((name :allocation :persistent :init-keyword :name)))

(define-class <person> (<kahua-persistent-base>)
  ((name  :allocation :persistent :init-keyword :name)
   (group :allocation :persistent :init-keyword :group)))

(define-method list-slots ((self <kahua-persistent-base>))
  (map (lambda (slot)
         (let1 o (ref self slot)
           (if (is-a? o <kahua-persistent-base>)
               (list-slots o)
               o)))
       (sort (filter-map (lambda (slotdef)
                           (if (eq? (slot-definition-allocation slotdef)
                                    :persistent)
                               (car slotdef)
                               #f))
                         (class-slots (class-of self)))
             (lambda (a b)
               (string<? (symbol->string a)
                         (symbol->string b))))))

(define addr #f)
(with-db (db *dbname*)
  (set! addr (make <address>
               :name "kenji hisazumi"
               :address "hoge"))

  (dotimes (i 100)
    (make <numnum> :num (- 99 i))))

(test-section "utlities")

(test* "list-slots" '("hoge" "kenji hisazumi")
       (list-slots addr))

(test* "ref:" "kenji hisazumi"
       ((ref: <address> 'name) addr))

(define predlist #f)
(test "=: (string:#t)" #t
      (lambda ()
        (set! predlist (=: "a" "a"))
        (predlist #f)))

(test "=: (string:#t)" #f
      (lambda ()
        (set! predlist (=: "b" "a"))
        (predlist #f)))

(test* "where:" predlist
       (cadr (WHERE: predlist)))

(test* "orderby:" #f
       ((cadr (ORDERBY: 'DESC (lambda (o) o))) 1 2))

(test* "orderby:" #t
       ((cadr (ORDERBY: 'DESC (lambda (o) o))) 2 1))

(test-section "queries")

(define-syntax testq
  (syntax-rules ()
    ((_ com val query) (test com val (lambda () (map list-slots query))))))

(with-db (db *dbname*)
  (testq "simple query" '(("hoge" "kenji hisazumi"))
         (QUERY (FROM: <address>)))

  (testq "where: hit" '(("hoge" "kenji hisazumi"))
         (QUERY (FROM:  <address>)
                (WHERE: (=: (ref: <address> 'address) "hoge"))))

  (testq "where: miss" '()
         (QUERY (FROM:  <address>)
                (WHERE: (=: (ref: <address> 'address) "hage"))))

  (testq "pred: =" '((10))
         (QUERY (FROM: <numnum>)
                (WHERE: (=: (ref: <numnum> 'num) 10))))

  (test* "pred: <" '(0 1 2 3 4 5 6 7 8 9)
         (sort (map (cut ref <> 'num)
                    (QUERY (FROM: <numnum>)
                           (WHERE: (<: (ref: <numnum> 'num) 10))))))

  (test* "pred: >" '(0 1 2 3 4 5 6 7 8 9)
         (sort (map (cut ref <> 'num)
                    (QUERY (FROM: <numnum>)
                           (WHERE: (>: 10 (ref: <numnum> 'num)))))))

  (test* "pred: >" '(98 99)
         (sort (map (cut ref <> 'num)
                    (QUERY (FROM: <numnum>)
                           (WHERE: (>: (ref: <numnum> 'num) 97))))))

  (test* "pred: <" '(98 99)
         (sort (map (cut ref <> 'num)
                    (QUERY (FROM: <numnum>)
                           (WHERE: (<: 97 (ref: <numnum> 'num)))))))

  (testq "pred: %%" '(("hoge" "kenji hisazumi"))
         (QUERY (FROM:  <address>)
                (WHERE: (%%: (ref: 'name) "kenji"))))

  (testq "and: hit" '(("hoge" "kenji hisazumi"))
         (QUERY (FROM:  <address>)
                (WHERE: (and: (=: (ref: <address> 'address) "hoge")
                              (=: (ref: <address> 'name) "kenji hisazumi")))))

  (testq "and: miss" '()
         (QUERY (FROM:  <address>)
                (WHERE: (and: (=: (ref: <address> 'address) "hoge")
                              (=: (ref: <address> 'name) "kenji")))))

  (testq "or: hit" '(("hoge" "kenji hisazumi"))
         (QUERY (FROM:  <address>)
                (WHERE: (or: (=: "hoge" (ref: <address> 'address))
                             (=: "kenji" (ref: <address> 'name))))))

  (testq "or: miss" '()
         (QUERY (FROM:  <address>)
                (WHERE: (or: (=: "hogee" (ref: <address> 'address))
                             (=: "kenji" (ref: <address> 'name))))))

  (test* "projection" '(0 1 2 3 4 5 6 7 8 9)
         (sort (QUERY (PRJ: (ref: 'num))
                      (FROM: <numnum>)
                      (WHERE: (<: (ref: 'num) 10)))))

  (test* "order by" '(0 1 2 3 4 5 6 7 8 9)
         (QUERY (PRJ: (ref: 'num))
                (FROM: <numnum>)
                (WHERE: (<: (ref: <numnum> 'num) 10))
                (ORDERBY: 'INC (ref: <numnum> 'num))))

  (test* "order by (desc)" '(9 8 7 6 5 4 3 2 1 0)
         (QUERY (PRJ: (ref: 'num))
                (FROM: <numnum>)
                (WHERE: (<: (ref: <numnum> 'num) 10))
                (ORDERBY: 'DESC (ref: <numnum> 'num))))
  )

(with-db (db *dbname*)
  (make <address>
    :name #f
    :address "hoge")
  (make <address>
    :name '()
    :address "booo"))


(with-db (db *dbname*)
  (testq "null/#f value is regarded as a #f" '(("hoge" "kenji hisazumi"))
         (QUERY (FROM: <address>)
                (WHERE: (%%: (ref: 'name) "kenji"))))
  )


(test-section "object references")

(define sake #f)
(define anime #f)

(with-db (db *dbname*)
  (let ((sake (make <group> :name "sake freaks"))
        (anime (make <group> :name "anime freaks")))
    (make <person> :name "Kenji" :group sake)
    (make <person> :name "Akane" :group anime)))

(with-db (db *dbname*)
   (testq "reference" `((("sake freaks") "Kenji"))
          (QUERY (FROM: <person>)
                 (WHERE: (=: (ref: (ref: <person> 'group) 'name)
                             "sake freaks"))))
   )

(test-section "update")

;; UPDATE commandは欲しいですかね...
(with-db (db *dbname*)
  (for-each (lambda (o)
              (slot-set! o 'name "Hogee"))
            (QUERY (FROM: <person>) (WHERE: (=: (ref: 'name) "Kenji")))))

(with-db (db *dbname*)
  (testq "db update" '((("sake freaks") "Hogee"))
         (QUERY (FROM: <person>) (WHERE: (=: (ref: 'name) "Hogee"))))
  )

(test-end)
