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

(test-start "worker scripts")

(define *site* "_site")

(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(copy-file "../plugins/allow-module.scm"  #`",|*site*|/plugins/allow-module.scm")

(kahua-common-init *site* #f)

;;------------------------------------------------------------
;; Run hello-world

(test-section "kahua-server hello-world.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./hello-world.kahua"))

 (test* "run hello-world.kahua" #t (worker-running? w))

 (test* "start hello-world.kahua"
       '(*TOP* (html (head ?*)
                     (body ?@
                           (h1 ?*)
                           (h2 ?*)
                           (p ?*))))
       (call-worker/gsid->sxml w '() '())
       (make-match&pick w))

 (test* "eval" '((("x-kahua-status" "OK"))
                 ("" "" "kahua-server"))
       (call-worker w
                    '(("x-kahua-eval" "#t"))
                    '(module-name (current-module))
                    list))
 )

;; Higher-order version
(test-section "kahua-server hello-world-st.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./hello-world-st.kahua"))

 (test* "run hello-world-st.kahua" #t (worker-running? w))

 (test* "start hello-world-st.kahua"
       '(*TOP* (html (head ?*)
                     (body ?@
                           (h1 ?*)
                           (h2 ?*)
                           (p ?*))))
       (call-worker/gsid->sxml w '() '())
       (make-match&pick w))

 (test* "eval" '((("x-kahua-status" "OK"))
                 ("" "" "kahua-server"))
       (call-worker w
                    '(("x-kahua-eval" "#t"))
                    '(module-name (current-module))
                    list))
 )

;;------------------------------------------------------------
;; Run greeting

(test-section "kahua-server greeting.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./greeting.kahua"))

 (test* "run greeting.kahua" #t (worker-running? w))

 (test* "start greeting.kahua"
        '(*TOP* (html (head ?*)
                      (body ?@
                            (h1 ?*)
                            (form (@ (action ?&) ?*)
                                  (input ?@))
                            )))
       (call-worker/gsid->sxml w '() '())
       (make-match&pick w))

 (test* "next greeting.kahua"
        '(*TOP* (html (head (title "Welcome"))
                      (body ?@
                            (h1 "Welcome")
                            "Hello, nobsun! How are you?")))
        (call-worker/gsid->sxml w '() '(("name" "nobsun")))
        (make-match&pick w))
 )

;; Higher-order version
(test-section "kahua-server greeting-st.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./greeting-st.kahua"))

 (test* "run greeting-st.kahua" #t (worker-running? w))

 (test* "start greeting-st.kahua"
        '(*TOP* (html (head ?*)
                      (body ?@
                            (h1 ?*)
                            (form (@ (action ?&) ?*)
                                  (input ?@))
                            )))
       (call-worker/gsid->sxml w '() '())
       (make-match&pick w))

 (test* "next greeting-st.kahua"
        '(*TOP* (html (head (title "Welcome"))
                      (body ?@
                            (h1 "Welcome")
                            "Hello, nobsun! How are you?")))
        (call-worker/gsid->sxml w '() '(("name" "nobsun")))
        (make-match&pick w))
 )

;;------------------------------------------------------------
;; Run lister

(test-section "kahua-server lister.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./lister.kahua"))

 (test* "run lister.kahua" #t (worker-running? w))

 (test* "start lister.kahua"
        '(*TOP*
          (html (head (title ?*))
                (body ?@
                      (h1 ?*)
                      (ul (li "0")
                          (li "1")
                          (li "2")
                          (li "3")
                          (li "4")
                          (li "5")
                          (li "6")
                          (li "7")
                          (li "8")
                          (li "9")
                          (li (a (@ (href ?&)) "Next"))))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test* "next lister.kahua"
        '(*TOP*
          (html (head (title ?*))
                (body ?@
                      (h1 ?*)
                      (ul (li "10")
                          (li "11")
                          (li "12")
                          (li "13")
                          (li "14")))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))
 )

;; Higher-order version
(test-section "kahua-server lister-st.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./lister-st.kahua"))

 (test* "run lister-st.kahua" #t (worker-running? w))

 (test* "start lister-st.kahua"
        '(*TOP*
          (html (head (title ?*))
                (body ?@
                      (h1 ?*)
                      (ul (li "0")
                          (li "1")
                          (li "2")
                          (li "3")
                          (li "4")
                          (li "5")
                          (li "6")
                          (li "7")
                          (li "8")
                          (li "9")
                          (li (a (@ (href ?&)) "Next"))))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test* "next lister-st.kahua"
        '(*TOP*
          (html (head (title ?*))
                (body ?@
                      (h1 ?*)
                      (ul (li "10")
                          (li "11")
                          (li "12")
                          (li "13")
                          (li "14")))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))
 )

;;------------------------------------------------------------
;; Run a/cont test
(test-section "kahua-server acont.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./acont.kahua"))

 (test* "run acont.kahua" #t (worker-running? w))

 (test* "start acont.kahua"
        '(*TOP* (html (body ?@
                            (a (@ (href ?&)) "next"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test* "next acont.kahua"
        '(*TOP* (html (body ?@
                            (a (@ (href ?&)) "top"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))
 )

;; Higher-order version
(test-section "kahua-server acont-st.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./acont-st.kahua"))

 (test* "run acont-st.kahua" #t (worker-running? w))

 (test* "start acont-st.kahua"
        '(*TOP* (html (body ?@
                            (a (@ (href ?&)) "next"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test* "next acont-st.kahua"
        '(*TOP* (html (body ?@
                            (a (@ (href ?&)) "top"))))
        (call-worker/gsid->sxml w '() '() '(html))
        (make-match&pick w))
 )

(test-end)
