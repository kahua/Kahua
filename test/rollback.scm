;; -*- coding: utf-8 ; mode: scheme -*-
;; test worker scripts.
;; this test isn't for modules, but the actual scripts.

(use srfi-2)
(use gauche.test)
(use gauche.process)
(use gauche.net)
(use rfc.uri)
(use util.list)
(use text.tree)
(use file.util)

(use kahua.test.xml)
(use kahua.test.worker)
(use kahua.persistence)
(use kahua.user)
(use kahua.config)

(test-start "rollback")

(define *site* "_site")
(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(copy-file "../plugins/allow-module.scm"  #`",|*site*|/plugins/allow-module.scm")

(define-syntax kahua-eval
  (syntax-rules ()
    ((_  body)
     '(eval 'body kahua-app-server))))
              


(kahua-common-init *site* #f)

;;------------------------------------------------------------
;; Run rollback

(test-section "kahua-server rollback.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./rollback.kahua"))

 (test* "run rollback.kahua" #t (worker-running? w))

 
 (test* "make object"
        '(*TOP*
          (html
           "aaaa"))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-worker" "rollback")
           ("x-kahua-cgsid" "make-obj")
           ("x-kahua-path-info"
            ("rollback" "make-obj")))
         '())
        (make-match&pick w))

 (test* "abort transaction"
        '(*TOP*
          (html
           (head
            (title "Kahua error"))
           ?*))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-worker" "rollback")
           ("x-kahua-cgsid" "raise-error")
           ("x-kahua-path-info"
            ("rollback" "raise-error")))
         '())
        (make-match&pick w))

 (test* "check rollback"
        '(*TOP*
          (html
           "hoge"))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-worker" "rollback")
           ("x-kahua-cgsid" "read-obj")
           ("x-kahua-path-info"
            ("rollback" "read-obj")))
         '())
        (make-match&pick w))

 (test* "x-kahua-eval 1"
        '((("x-kahua-status" "OK"))
          ("" "" "obj1"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (define obj1 (make <ooo> :a 'a-of-obj1)))
                     list))

 (test* "x-kahua-eval check"
        '((("x-kahua-status" "OK"))
          ("" "" "a-of-obj1"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (ref obj1 'a))
                     list))

 (test* "x-kahua-eval modify"
        '((("x-kahua-status" "OK"))
          ("" "" "a-of-obj1-mod"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (begin (set! (ref obj1 'a) 'a-of-obj1-mod)
                                        (ref obj1 'a)))
                     list))

 (test* "x-kahua-eval error"
        '(("x-kahua-status" "ERROR"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (begin (set! (ref obj1 'a) 'a-of-obj1-mod2)
                                        (error "a")))
                     (lambda (h b) h)))

 (test* "x-kahua-eval rollback"
        '((("x-kahua-status" "OK"))
          ("" "" "a-of-obj1-mod"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (ref obj1 'a))
                     list))

 (test* "x-kahua-eval new-obj"
        '(("x-kahua-status" "ERROR"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (let1 obj (make <k> :k "new-k")
                                   (error "k")
                                   (key-of obj)))
                     (lambda (h b) h)
                     ))

 (test* "x-kahua-eval rollback"
        '((("x-kahua-status" "OK"))
          ("" "" "#f"))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (find-kahua-instance <k> "new-k"))
                     list))



 (test* "x-kahua-eval rollback"
        '((("x-kahua-status" "OK"))
          ("" "" "\"new-k\""))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (let1 obj (make <k> :k "new-k")
                                   (key-of obj)
                                   ))
                     list))

 (test* "x-kahua-eval rollback"
        '((("x-kahua-status" "OK"))
          ("" "" "\"new-k\""))
        (call-worker w
                     '(("x-kahua-eval" "#t"))
                     (kahua-eval (let1 obj (find-kahua-instance <k> "new-k")
                                   (key-of obj)))
                     list))

 )


(test-end)
