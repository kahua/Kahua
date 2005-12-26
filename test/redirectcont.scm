;; -*- coding: euc-jp ; mode: scheme -*-
;; test worker scripts.
;; this test isn't for modules, but the actual scripts.
;; $Id: redirectcont.scm,v 1.1 2005/12/26 14:27:36 shibata Exp $

(use srfi-2)
(use gauche.test)
(use gauche.process)
(use gauche.net)
(use rfc.uri)
(use util.list)
(use text.tree)
(use kahua)
(use kahua.test.xml)
(use kahua.test.worker)

(use kahua.persistence)
(use kahua.user)
(use file.util)

(test-start "redirect/cont")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(kahua-init *config*)

;;------------------------------------------------------------
;; Run a/cont test
(test-section "kahua-server redirectcont.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-c" ,*config* "./redirectcont.kahua"))

 (test* "run redirectcont.kahua" #t (worker-running? w))

 (test* "header->sxml"
        '(*TOP* (!contain (Status "302 Moved")
                          (Location ?&)))
        (call-worker/gsid
         w
         '()
         '(("arg" "aaaa")
           )
         header->sxml)
        (make-match&pick w))

 (test* "next acont.kahua"
        '(*TOP* (html (body "aaaa"
                            (a (@ (href ?&)) "top"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test/send&pick "test/send&pick" w '())

 (test* "next acont.kahua"
        '(*TOP* (html (body ?@
                            (a (@ (href ?&)) "top"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))
 )

(test-end)