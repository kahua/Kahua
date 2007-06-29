;; -*- coding: euc-jp ; mode: scheme -*-
;; test wiki-iki script.
;; this test isn't for modules, but the actual scripts.
;; $Id: wiki-iki.scm,v 1.3.10.1 2007/06/29 08:03:33 bizenn Exp $

(use srfi-2)
(use srfi-11)
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

(test-start "wiki-iki test scripts")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(kahua-init *config*)

;;------------------------------------------------------------
;; Run wiki-iki
(test-section "kahua-server wiki-iki.kahua")

(with-worker
 (w `("gosh" "-I../src" "-I../examples" "../src/kahua-server.scm"
      "-c" ,*config* "../examples/wiki-iki/wiki-iki.kahua"))

 (test* "run wiki-iki.kahua" #t (worker-running? w))

 (test* "top page"
        '(*TOP* (a (@ (href ?&)) "[Edit]")
                (a ?@ "Kahua"))
        (call-worker/gsid->sxml w '() '() '(// (td 1) div a))
        (make-match&pick w))

 (test* "edit top page"
        '(*TOP* (form (@ (action ?&) ?*) ?*))
        (call-worker/gsid->sxml w '() '() '(// form))
        (make-match&pick w))

 ;; NB: we should be able to select the A element in recent changes list
 ;; by sxpath (// "td[@id='recentchanges']" a), but it doesn't work on
 ;; Gauche 0.7.4.1 due to the bug in sxpath-ext.scm.  
 (test* "commit - recentchanges"
        '(*TOP*
          (a ?@ "[Login]")
          (a (@ (href ?&)) "Welcome to Wiki Iki"))
        (call-worker/gsid->sxml w '()
                                '(("content" "[[test page]]")
                                  ("commit" "commit"))
                                '(// (td 2) // a))
        (make-match&pick w))

 (test* "access via recentchanges link"
        '(*TOP* (a (@ (href ?&)) "test page"))
        (call-worker/gsid->sxml w '() '() '(// p a))
        (make-match&pick w))

 (test* "try to access test page"
        '(*TOP* (a (@ (href ?&)) "Edit test page"))
        (call-worker/gsid->sxml w '() '() '(// p a))
        (make-match&pick w))

 (test* "start editing test page"
        '(*TOP* (form (@ (action ?&) ?*) ?*))
        (call-worker/gsid->sxml w '() '() '(// form))
        (make-match&pick w))

 (test* "commit test page"
        '(*TOP* (p "OK\n"))
        (call-worker/gsid->sxml w '()
                                '(("content" "OK")
                                  ("commit" "コミット"))
                                '(// (td 1) // p))
        (make-match&pick w))
 )

(test-end)
