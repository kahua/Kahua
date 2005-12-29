;; -*- coding: euc-jp ; mode: scheme -*-
;; test worker scripts.
;; this test isn't for modules, but the actual scripts.
;; $Id: entry-method.scm,v 1.4 2005/12/29 08:39:23 shibata Exp $

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

(test-start "entry-method")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(define-syntax kahua-eval
  (syntax-rules ()
    ((_  body)
     '(eval 'body kahua-app-server))))

(kahua-init *config*)

;;------------------------------------------------------------
;; Run rollback

(test-section "kahua-server entry-method.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-c" ,*config* "./entry-method.kahua"))

 (test* "run entry-method.kahua" #t (worker-running? w))

 (test* "make objects"
        '(*TOP*
          (html
           (body
            (span "Kahua"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "make-objects")
           ("x-kahua-path-info"
            ("rollback" "make-objects")))
         '())
        (make-match&pick w))

 (test* "dispatch shwo entry()"
        '(*TOP*
          (html
           (body
            (h1 "show"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show"))
           )
         '())
        (make-match&pick w))

 (test* "dispatch shwo entry(:keyword k)"
        '(*TOP*
          (html
           (body
            (h1 "show")
            (h2 "keyword"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show")))
         '(("k" "keyword")))
        (make-match&pick w))

  (test* "dispatch shwo entry(path1)"
        '(*TOP*
          (html
           (body
            (span "path1"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "path1"))
           )
         '())
        (make-match&pick w))

 (test* "dispatch shwo entry(path1 path2)"
        '(*TOP*
          (html
           (body
            (h1 "path1")
            (h2 "path2"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "path1" "path2"))
           )
         '())
        (make-match&pick w)
        )

 (test* "dispatch shwo entry(<folder>)"
        '(*TOP*
          (html
           (body
            (h1 "here is Kahua")
            (ul (li (a ?@ "VERSION"))
                (li (a (@ (href ?&) ?*) "README"))
                (li (a ?@ "Kahua2"))
                (li (a ?@ "tmp*link"))))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "*folder*Kahua"))
           )
         '())
        (make-match&pick w)
        )

 (test* "dispatch shwo entry(<file>)"
        '(*TOP*
          (html
           (body
            (h1 "README")
            (pre "see INSTALL"))))
        (call-worker/gsid->sxml
         w
         '()
         '())
        (make-match&pick w)
        )

 (test* "dispatch shwo entry(<link>)"
        '(*TOP*
          (html
           (body
            (h1 "tmp*link")
            (pre "tmp link"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "*(lin%2ak)*tmp%2alink")))
         '())
        (make-match&pick w)
        )
 )


(test-end)
