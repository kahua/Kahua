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
(use kahua)
(use kahua.test.xml)
(use kahua.test.worker)

(use kahua.persistence)
(use kahua.user)
(use file.util)

(test-start "multi-paging and partial form")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(kahua-init *config*)

;;------------------------------------------------------------
;; Run a/cont test
(test-section "kahua-server multi-paging.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-c" ,*config* "./multi-paging.kahua"))

 (test* "run multi-paging.kahua" #t (worker-running? w))

 (test* "initial page"
        '(*TOP* (a ?@ "<<")
		(a (@ (href ?&)) ">>")
		(a ?@ "<<")
		(a ?@ ">>")
		(li "1") (li "2") (li "3") (li "1") (li "2") (li "3"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (test* "header->sxml page 1"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "p2 at list-1 & p1 at list-2"
        '(*TOP* (a ?@ "<<")
		(a (@ (href ?&)) ">>")
		(a ?@ "<<")
		(a ?@ ">>")
		(li "4") (li "5") (li "6") (li "1") (li "2") (li "3"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (test* "header->sxml page 2"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "p3 at list-1 & p1 at list-2"
        '(*TOP* (a ?@ "<<")
		(a (@ (href ?&save1)) ">>")
		(a ?@ "<<")
		(a (@ (href ?&)) ">>")
		(li "7") (li "8") (li "9") (li "1") (li "2") (li "3"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (test* "header->sxml page 3"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "p3 at list-1 & p2 at list-2"
        '(*TOP* (a (@ (href ?&)) "<<")
		(a ?@ ">>")
		(a ?@ "<<")
		(a ?@ ">>")
		(li "7") (li "8") (li "9") (li "4") (li "5") (li "6"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (test* "header->sxml page 4"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "p2 at list-1 & p2 at list-2"
        '(*TOP* (a ?@ "<<")
		(a ?@ ">>")
		(a ?@ "<<")
		(a (@ (href ?&)) ">>")
		(li "4") (li "5") (li "6") (li "4") (li "5") (li "6"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (test* "header->sxml page 5"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "p2 at list-1 & p2 at list-2"
        '(*TOP* (a ?@ "<<")
		(a ?@ ">>")
		(a ?@ "<<")
		(a (@ (href ?&)) ">>")
		(li "4") (li "5") (li "6") (li "7") (li "8") (li "9"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (set-gsid w 'save1)

 (test* "header->sxml page 6"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "brawser's back button attack - p4 at list-1 & p1 at list-2"
        '(*TOP* (a ?@ "<<")
		(a ?@ ">>")
		(a ?@ "<<")
		(a (@ (href ?&)) ">>")
		(li "10") (li "11") (li "12") (li "1") (li "2") (li "3"))
        (call-worker/gsid->sxml w '() '() '(// (or@ a li)))
	(make-match&pick w))

 (test* "header->sxml page 7"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '() header->sxml)
	(make-match&pick w))

 (test* "p4 at list-1 & p2 at list-2"
        '(*TOP*
	  (form (@ (method "POST")
		   (id "dd1")
		   (action ?&))
		(input (@ (type "text") (name "name")))
		(input (@ (type "text") (name "age")))
		(input (@ (value "Add") (type "submit")))
		(dl))
	  (li "10") (li "11") (li "12") (li "4") (li "5") (li "6"))
        (call-worker/gsid->sxml w '() '() '(// (or@ li form)))
	(make-match&pick w))

 (test* "header->sxml page 8"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '(("name" "cut-sea") ("age" "37")) header->sxml)
	(make-match&pick w))

 (test* "data insert1 & keep paging list-1&2"
        '(*TOP*
	  (form (@ (method "POST")
		   (id "dd1")
		   (action ?&))
		(input (@ (type "text") (name "name")))
		(input (@ (type "text") (name "age")))
		(input (@ (value "Add") (type "submit")))
		(dl (dt "cut-sea" (dd "37"))))
	  (li "10") (li "11") (li "12") (li "4") (li "5") (li "6"))
        (call-worker/gsid->sxml w '() '() '(// (or@ li form)))
	(make-match&pick w))

 (test* "header->sxml page 8"
        '(*TOP* (!contain (Status "302 Found") (Location ?&)))
        (call-worker/gsid w '() '(("name" "shiro") ("age" "38")) header->sxml)
	(make-match&pick w))

 (test* "data insert2 & keep paging list-1&2"
        '(*TOP*
	  (div (@ (id "pg1"))
	       (a (@ (href ?&)) "<<")
	       (a ?@ ">>")
	       (ul (li "10") (li "11") (li "12")))
	  (div (@ (id "pg2"))
	       (a ?@ "<<")
	       (a ?@ ">>")
	       (ul (li "4") (li "5") (li "6")))
	  (form ?@
		(input (@ (type "text") (name "name")))
		(input (@ (type "text") (name "age")))
		(input (@ (value "Add") (type "submit")))
		(dl (dt "shiro" (dd "38"))
		    (dt "cut-sea" (dd "37")))))
        (call-worker/gsid->sxml w '() '() '(// (or@ div form)))
	(make-match&pick w))

 )

(test-end)
