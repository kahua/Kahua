;; -*- coding: utf-8 ; mode: scheme -*-
;; test of foldlist.scm

(use srfi-2)
(use srfi-11)
(use gauche.test)
(use gauche.process)
(use gauche.net)
(use rfc.uri)
(use util.list)
(use text.tree)
(use sxml.ssax)
(use sxml.sxpath)
(use kahua)
(use kahua.test.xml)
(use kahua.test.worker)

(use kahua.persistence)
(use kahua.user)
(use file.util)

(test-start "foldlist test scripts")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(kahua-init *config*)


;;------------------------------------------------------------
;; Run nqueen

(test-section "kahua-server foldlist.kahua")

(with-worker
 (w `("gosh" "-I../src" "-I../examples" "../src/kahua-server"
      "-c" ,*config* "../examples/foldlist/foldlist.kahua"))
 
 (test* "run foldlist" #t (worker-running? w))

 (test* "initial screen"
        '(*TOP* (form (@ (method "POST") (action ?&)) ?*))
        (call-worker/gsid->sxml w '() '() '(// form))
        (make-match&pick w))

 (test* "open tree screen 1"
	'(*TOP* (a (@ (href ?&)) "[open] "))
	(call-worker/gsid->sxml w '() '(("content" "(a (b (c)))"))
				'(// li a))
	(make-match&pick w))
 
 (test* "open tree screen 2"
	'(*TOP* (a ?@ "[close] ")
		(a (@ (href ?&)) "[open] "))
	(call-worker/gsid->sxml w '() '() '(// li a))
	(make-match&pick w))

 (test* "open tree screen 3"
	'(*TOP* (a ?@ "[close] ")
		(a ?@ "[close] ")
		(a (@ (href ?&)) "[open] "))
	(call-worker/gsid->sxml w '() '() '(// li a))
	(make-match&pick w))

 (test* "open tree screen 4"
	'(*TOP* (a ?@ "[close] ")
		(a ?@ "[close] ")
		(a (@ (href ?&)) "[close] "))
	(call-worker/gsid->sxml w '() '() '(// li a))
	(make-match&pick w))

 )

;;
(test-end)



