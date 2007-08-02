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
;; Run entry-method
(test-section "kahua-server entry-method.kahua")

(with-worker
 (w `("gosh" "-I../src" "../src/kahua-server"
      "-c" ,*config* "./entry-method.kahua"))

 (test* "run entry-method.kahua" #t (worker-running? w))

 (test* "make objects"
        '(*TOP*
          (html
           (body
            (h1 "here is Kahua")
            (ul (li (a ?@ "VERSION"))
                (li (a ?@ "README"))
                (li (a ?@ "Kahua2"))
                (li (a ?@ "tmp*link"))))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "make-objects")
           ("x-kahua-path-info"
            ("entry-method" "make-objects")))
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

 (test* "dispatch shwo entry(arg1 :rest rargs)"
        '(*TOP*
          (html
           (body
            "(path1 path2 path3)")))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "path1" "path2" "path3")))
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
                (li (a (@ (href ?&folder) ?*) "Kahua2"))
                (li (a (@ (href ?&link) ?*) "tmp*link"))))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "<folder>\tKahua"))
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
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (set-gsid w "link")

 (test* "dispatch shwo entry(<link>)"
        '(*TOP*
          (html
           (body
            (h1 "tmp*link")
            (pre "tmp link"))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (set-gsid w "folder")

 (test* "dispatch shwo entry(<folder>)"
        '(*TOP*
          (html
           (body
            (h1 "here is Kahua2")
            (ul))))
        (call-worker/gsid->sxml w '() '())
        (make-match&pick w))

 (test-section "check entry method rules")

 (test* "check rule ((file <file>) \"size\")"
        '(*TOP*
          (html
           (body
            (h1 "size of README")
            (pre "11"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "<file>\tREADME" "size"))
           )
         '())
        (make-match&pick w))

 (test* "check rule ((file <file>) \"desc\")"
        '(*TOP*
          (html
           (body
            (h1 "description of README")
            (pre "class : <file>"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "<file>\tREADME" "desc"))
           )
         '())
        (make-match&pick w))

 (test* "check rule ((file <file>) unknown)"
        '(*TOP*
          (html
           (body
            (h1 "unknown info for README : hogehoge"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "<file>\tREADME" "hogehoge"))
           )
         '())
        (make-match&pick w))

 (test* "check rule (\"flat\" (folder <folder>))"
        '(*TOP*
          (html
           (body
            (span "VERSION")
            (span "README")
            (span "Kahua2")
            (span "tmp*link"))))
        (call-worker/gsid->sxml
         w
         '(("x-kahua-cgsid" "show")
           ("x-kahua-path-info"
            ("entry-method" "show" "flat" "<folder>\tKahua"))
           )
         '())
        (make-match&pick w))
 )

(test-end)
