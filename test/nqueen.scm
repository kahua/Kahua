;; -*- coding: euc-jp ; mode: scheme -*-
;; test nqueen script.
;; this test isn't for modules, but the actual scripts.
;; $Id: nqueen.scm,v 1.2.2.1 2005/12/04 02:19:48 nobsun Exp $

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

(test-start "nqueen test scripts")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(kahua-init *config*)

;;------------------------------------------------------------
;; Run nqueen
(test-section "kahua-server nqueen.kahua")

(with-worker
 (w `("gosh" "-I../src" "-I../examples" "../src/kahua-server"
      "-c" ,*config* "../examples/nqueen/nqueen.kahua"))

 (test* "run nqueen.kahua" #t (worker-running? w))

 (test* "initial screen"
        '(*TOP* (a (@ (href ?&)) "8 queens")
                (a ?@ "12 queens"))
        (call-worker/gsid->sxml w '() '() '(// a))
        (make-match&pick w))

 (test* "8 queen (1)"
        '(*TOP*
          (li "(3 0 4 7 5 2 6 1)")
          (li "(2 5 3 0 7 4 6 1)")
          (li "(4 6 3 0 2 7 5 1)")
          (li "(4 2 7 3 6 0 5 1)")
          (li "(2 5 7 0 3 6 4 1)")
          (li "(3 5 7 2 0 6 4 1)")
          (li "(4 6 0 2 7 5 3 1)")
          (li "(2 4 1 7 5 3 6 0)")
          (li "(2 5 3 1 7 4 6 0)")
          (li "(4 1 3 6 2 7 5 0)")
          (li "(3 1 6 2 5 7 4 0)")
          (a (@ (href ?&)) "next items")
          (a ?@ "restart"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
        (make-match&pick w))

  (test* "8 queen (2)"
        '(*TOP*
          (li "(4 7 3 0 6 1 5 2)")
          (li "(3 7 0 4 6 1 5 2)")
          (li "(3 6 0 7 4 1 5 2)")
          (li "(0 6 4 7 1 3 5 2)")
          (li "(1 6 4 7 0 3 5 2)")
          (li "(5 1 6 0 3 7 4 2)")
          (li "(5 7 1 3 0 6 4 2)")
          (li "(5 3 6 0 7 1 4 2)")
          (li "(0 6 3 5 7 1 4 2)")
          (li "(5 3 1 7 4 6 0 2)")
          (li "(3 6 4 2 0 5 7 1)")
          (a (@ (href ?&)) "next items")
          (a ?@ "restart"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
        (make-match&pick w))
 )

; (test-end)

;;------------------------------------------------------------
;; Run nqueen
(test-section "kahua-server lazy-nqueen.kahua")

(with-worker
 (w `("gosh" "-I../src" "-I../examples" "../src/kahua-server"
      "-c" ,*config* "../examples/lazy-nqueen/lazy-nqueen.kahua"))

 (test* "run lazy-nqueen.kahua" #t (worker-running? w))

 (test* "initial screen"
        '(html (head (title ?*))
	       (body ?@
		     (h1 ?*)
		     (a ?@ "6-Queens")
		     ?*
		     (a ?@ "7-Queens")
		     ?*
		     (a (@ (href ?&)) "8-Queens")
		     ?*
		     (a ?@ "9-Queens")
		     ?*
		     (a ?@ "10-Queens")
		     ?*
		     (a ?@ "11-Queens")
		     ?*
		     (a ?@ "12-Queens")))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "8 queen (1)"
        '(html (head (title ?*))
	       (body ?@
		     (h1 ?*)
		     (p ?*
			(table ?@
			 (tr (td "○")(td "○")(td "○")(td "●") 
			     (td "○")(td "○")(td "○")(td "○"))
			 (tr (td "○")(td "●")(td "○")(td "○")
			     (td "○")(td "○")(td "○")(td "○"))
			 (tr (td "○")(td "○")(td "○")(td "○")
			     (td "○")(td "○")(td "●")(td "○"))
			 (tr (td "○")(td "○")(td "●")(td "○")
			     (td "○")(td "○")(td "○")(td "○"))
			 (tr (td "○")(td "○")(td "○")(td "○")
			     (td "○")(td "●")(td "○")(td "○"))
			 (tr (td "○")(td "○")(td "○")(td "○")
			     (td "○")(td "○")(td "○")(td "●"))
			 (tr (td "○")(td "○")(td "○")(td "○")
			     (td "●")(td "○")(td "○")(td "○"))
			 (tr (td "●")(td "○")(td "○")(td "○")
			     (td "○")(td "○")(td "○")(td "○"))
			 )
			(a (@ (href ?&)) "Next")
			?*)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "8 queen (1)"
        '(html (head (title ?*))
	       (body ?@
		     (h1 ?*)
		     (p "2個目の解"
 			(table ?@
 			 (tr (td "○")(td "○")(td "○")(td "○")
 			     (td "●")(td "○")(td "○")(td "○"))
 			 (tr (td "○")(td "●")(td "○")(td "○")
 			     (td "○")(td "○")(td "○")(td "○"))
 			 (tr (td "○")(td "○")(td "○")(td "●") 
 			     (td "○")(td "○")(td "○")(td "○"))
 			 (tr (td "○")(td "○")(td "○")(td "○")
 			     (td "○")(td "○")(td "●")(td "○"))
 			 (tr (td "○")(td "○")(td "●")(td "○")
 			     (td "○")(td "○")(td "○")(td "○"))
 			 (tr (td "○")(td "○")(td "○")(td "○")
 			     (td "○")(td "○")(td "○")(td "●"))
 			 (tr (td "○")(td "○")(td "○")(td "○")
 			     (td "○")(td "●")(td "○")(td "○"))
 			 (tr (td "●")(td "○")(td "○")(td "○")
 			     (td "○")(td "○")(td "○")(td "○"))
 			 )
 			(a (@ (href ?&)) "Next")
 			?*)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))
 )

(test-end)