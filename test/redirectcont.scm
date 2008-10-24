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

(test-start "redirect/cont")

(define *site* "_site")

(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(copy-file "../plugins/allow-module.scm"  #`",|*site*|/plugins/allow-module.scm")

(kahua-common-init *site* #f)

;;------------------------------------------------------------
;; Run a/cont test
(test-section "kahua-server redirectcont.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-S" ,*site* "./redirectcont.kahua"))

 (test* "run redirectcont.kahua" #t (worker-running? w))

 (test* "header->sxml"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '(("arg" "aaaa")) header->sxml)
        (make-match&pick w))

 (test* "next acont.kahua"
        '(*TOP* (html (body "aaaa" (a (@ (href ?&)) "top"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test/send&pick "test/send&pick" w '())

 (test* "next acont.kahua"
        '(*TOP* (html (body ?@ (a (@ (href ?&)) "top"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test* "header->sxml"
	'(*TOP* (!contain (Status "303 See Other") (Location ?&)))
	(call-worker/gsid w '(("x-kahua-cgsid" "redirectcont/second"))
			  '(("arg" "bbbb")) header->sxml)
	(make-match&pick w))

 (test* "next acont.kahua"
	'(*TOP* (html (body "bbbb" (a (@ (href ?&)) "top"))))
	(call-worker/gsid->sxml w '() '())
	(make-match&pick w))
 )

(test-end)