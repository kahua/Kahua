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

(test-start "kahua-call-with-current-context")

(define *site* "_site")

(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(copy-file "../plugins/allow-module.scm"  #`",|*site*|/plugins/allow-module.scm")

(kahua-common-init *site* #f)

;;------------------------------------------------------------
;; Run a/cont test
(test-section "kahua-server kahua-call-with-current-context.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./kahua-call-with-current-context.kahua"))

 (test* "run kahua-call-with-current-context.kahua" #t (worker-running? w))

 (test* "caller with counter and query"
        '(*TOP* (!permute (h1 "1") (h2 "123") (a (@ (href ?&)) "call")))
        (call-worker/gsid->sxml
         w
         '()
         '(("query" "123")
           )
         '(// (or@ h1 h2 a)))
        (make-match&pick w))

 (test* "callee"
        '(*TOP* (h1 "callee") (a (@ (href ?&)) "return caller"))
        (call-worker/gsid->sxml
         w
         '()
         '()
         '(// (or@ h1 h2 a)))
        (make-match&pick w))

 (test* "caller with incremented counter and query not changed"
        '(*TOP* (!permute (h1 "2") (h2 "123") (a (@ (href ?&)) "call")))
        (call-worker/gsid->sxml
         w
         '()
         '()
         '(// (or@ h1 h2 a)))
        (make-match&pick w))

 )

(test-end)
