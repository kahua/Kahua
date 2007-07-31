;; -*- coding: utf-8 ; mode: scheme -*-
;; test for kahua.test.xml
;; $Id$

(use gauche.test)

(test-start "kahua.test.xml")

(use kahua.test.xml)
(test-module 'kahua.test.xml)

;;-----------------------------------------------------------------
(test-section "elements")

(test* "atom" #t (test-sxml-match? 'a 'a))
(test* "atom" #f (test-sxml-match? 'a 'b))
(test* "atom" #t (test-sxml-match? "1" "1"))
(test* "atom" #f (test-sxml-match? "1" "2"))
(test* "atom" #t (test-sxml-match? '() '()))
(test* "atom" #f (test-sxml-match? '() '(a)))

(test* "extra-check" '() (test-sxml-match? 'a 'a values))
(test* "extra-check" #f  (test-sxml-match? 'a 'b values))

(test* "patvar" '((?a . a))   (test-sxml-match? '?a 'a values))
(test* "patvar" '((?a . "a")) (test-sxml-match? '?a '"a" values))
(test* "patvar" '((?a b c))   (test-sxml-match? '?a '(b c) values))
(test* "patvar" '((?a))       (test-sxml-match? '?a '() values))

(test* "node"   #t (test-sxml-match? '(p "foo" "bar")
                                     '(p "foo" "bar")))
(test* "node"   #f (test-sxml-match? '(p "foo" "bar")
                                     '(p "bar" "foo")))
(test* "node"   #t (test-sxml-match? '(ul (li (p "foo" "bar"))
                                          (li (p "FOO" "BAR")))
                                     '(ul (li (p "foo" "bar"))
                                          (li (p "FOO" "BAR")))))
(test* "node"   #f (test-sxml-match? '(ul (li (p "foo" "bar"))
                                          (li (p "FOO" "BAR")))
                                     '(ul (li (p "FOO" "BAR"))
                                          (li (p "foo" "bar")))))

(test* "node+patvar" '((?a . (p "foo" "bar")))
       (test-sxml-match? '?a '(p "foo" "bar") values))
(test* "node+patvar" '((?a . "foo"))
       (test-sxml-match? '(p ?a "bar") '(p "foo" "bar") values))
(test* "node+patvar" '((?b . "bar") (?a . "foo"))
       (test-sxml-match? '(p ?a ?b) '(p "foo" "bar") values))
(test* "node+patvar" #f
       (test-sxml-match? '(p ?a ?b ?c) '(p "foo" "bar") values))
(test* "node+patvar" #f
       (test-sxml-match? '(p ?a) '(p "foo" "bar") values))
(test* "node+patvar" '((?b . (p "FOO" "BAR")) (?a . (p "foo" "bar")))
       (test-sxml-match? '(ul (li ?a) (li ?b))
                         '(ul (li (p "foo" "bar")) (li (p "FOO" "BAR")))
                         values))
(test* "node+patvar" #f
       (test-sxml-match? '(ul (li ?a) (li ?b))
                         '(ul (li (p "foo" "bar")))
                         values))
(test* "node+patvar" #f
       (test-sxml-match? '(ul (li ?a) (li ?b))
                         '(ul (li (p "foo" "bar"))
                              (li (p "foo" "bar"))
                              (li (p "foo" "bar")))
                         values))
(test* "node+patvar(attr)" '((?@ @ ("href" "http://w3.org") ("name" "zz")))
       (test-sxml-match? '(a ?@ "foo")
                         '(a (@ ("href" "http://w3.org") ("name" "zz")) "foo")
                         values))
(test* "node+patvar(attr)" ()
       (test-sxml-match? '(a ?@ "foo")
                         '(a "foo")
                         values))
(test* "node+patvar(attr)" ()
       (test-sxml-match? '(a ?@)
                         '(a)
                         values))
(test* "node+patvar(attr)" #f
       (test-sxml-match? '(a ?@ "foo")
                         '(a)
                         values))

;;-----------------------------------------------------------------
(test-section "pattern directives")

(test* "seq" #t
       (test-sxml-match? '(p (!seq "a" "b" "c"))
                         '(p "a" "b" "c")))
(test* "seq" #f
       (test-sxml-match? '(p (!seq "a" "b" "c"))
                         '(p ("a" "b" "c"))))
(test* "seq" #f
       (test-sxml-match? '(p (!seq "a" "b" "c"))
                         '(p "a" "b")))
(test* "seq" #f
       (test-sxml-match? '(p (!seq "a" "b" "c"))
                         '(p "a" "b" "d")))
(test* "seq" #f
       (test-sxml-match? '(p (!seq "a" "b" "c"))
                         '(p "a" "b" "c" "d")))
(test* "seq" #t
       (test-sxml-match? '(p "A" (!seq "a" "b" "c"))
                         '(p "A" "a" "b" "c")))
(test* "seq" #f
       (test-sxml-match? '(p "A" (!seq "a" "b" "c"))
                         '(p "A" ("a" "b" "c"))))
(test* "seq" #f
       (test-sxml-match? '(p "A" (!seq "a" "b" "c"))
                         '(p "A" "a" "b")))
(test* "seq" #f
       (test-sxml-match? '(p "A" (!seq "a" "b" "c"))
                         '(p "A" "a" "b" "c" "d")))
(test* "seq" #t
       (test-sxml-match? '(p "A" (!seq "a" "b" "c") "Z")
                         '(p "A" "a" "b" "c" "Z")))
(test* "seq" #t
       (test-sxml-match? '(p "A" (!seq "a" "b") (!seq "c" "Z"))
                         '(p "A" "a" "b" "c" "Z")))
(test* "seq" #f
       (test-sxml-match? '(p "A" (!seq "a" "b") (!seq "c" "Z"))
                         '(p "A" "a" "c" "Z")))
(test* "seq" #f
       (test-sxml-match? '(p "A" (!seq "a" "b") (!seq "c" "Z"))
                         '(p "A" "a" "b" "c")))
(test* "seq" #t
       (test-sxml-match? '(p "A" (!seq "a" (!seq "b" "c") "Z") "ZZ")
                         '(p "A" "a" "b" "c" "Z" "ZZ")))
(test* "seq" #t
       (test-sxml-match? '(p (!seq) "A" (!seq "a" (!seq) (!seq (!seq "b") "c") "Z") "ZZ")
                         '(p "A" "a" "b" "c" "Z" "ZZ")))
(test* "seq" #t
       (test-sxml-match? '(ul (!seq (li "a" "b") (li "c" "d")))
                         '(ul (li "a" "b") (li "c" "d"))))
(test* "seq" #f
       (test-sxml-match? '(ul (!seq (li "a" "b") (li "c" "d")))
                         '(ul (li "a" "b"))))
(test* "seq" #f
       (test-sxml-match? '(ul (!seq (li "a" "b") (li "c" "d")))
                         '(ul (li "a" "b") (li "c" "d") (li "e" "bhf"))))
(test* "seq" #t
       (test-sxml-match? '(ul (!seq (li (!seq "a" "b")) (li "c" "d")))
                         '(ul (li "a" "b") (li "c" "d"))))


(test* "seq+patvar" '((?c . "ZZ") (?b . "b") (?a . "A"))
       (test-sxml-match? '(p (!seq) ?a (!seq "a" (!seq) (!seq (!seq ?b) "c") "Z") ?c)
                         '(p "A" "a" "b" "c" "Z" "ZZ")
                         values))

(test* "perm" #t
       (test-sxml-match? '(p (!permute "a" "b" "c"))
                         '(p "c" "a" "b")))
(test* "perm" #t
       (test-sxml-match? '(p (!permute "a" "b" "c"))
                         '(p "b" "c" "a")))
(test* "perm" #f
       (test-sxml-match? '(p (!permute "a" "b" "c"))
                         '(p "b" "c" "a" "a")))
(test* "perm" #f
       (test-sxml-match? '(p (!permute "a" "b" "c"))
                         '(p "a" "b" "c" "c")))
(test* "perm" #f
       (test-sxml-match? '(p (!permute "a" "b" "c"))
                         '(p "a" "b" "c" "c")))

(test* "perm" #t
       (test-sxml-match? '(p "a" (!permute "a" "b" "c") "c")
                         '(p "a" "c" "a" "b" "c")))
(test* "perm" #f
       (test-sxml-match? '(p "a" (!permute "a" "b" "c") "c")
                         '(p "a" "b" "c" "c")))
(test* "perm" #t
       (test-sxml-match? '(p "a" (!permute "a" "b" "c") "c")
                         '(p "a" "c" "a" "b" "c")))
(test* "perm" #f
       (test-sxml-match? '(p "a" (!permute "a" "b" "c") "c")
                         '(p "a" "b" "c" "c")))
(test* "perm" #t
       (test-sxml-match? '(ul (li "a") (!permute (li "b") (li "c")) (li "d"))
                         '(ul (li "a") (li "c") (li "b") (li "d"))))
(test* "perm" #f
       (test-sxml-match? '(ul (li "a") (!permute (li "b") (li "c")) (li "d"))
                         '(ul (li "a") (li "b") (li "d") (li "c"))))
(test* "perm" #t
       (test-sxml-match? '(ul (li "a")
                              (!permute (li "b")
                                        (!seq (li "c") (li "d"))))
                         '(ul (li "a") (li "c") (li "d") (li "b"))))
(test* "perm" #f
       (test-sxml-match? '(ul (li "a")
                              (!permute (li "b")
                                        (!seq (li "c") (li "d"))))
                         '(ul (li "a") (li "c") (li "b") (li "d"))))

(test* "perm" #t
       (test-sxml-match? '(ul (!seq (!permute (li "a") (li "b"))
                                    (!permute (li "c") (li "d"))))
                         '(ul (li "b") (li "a") (li "c") (li "d"))))
(test* "perm" #f
       (test-sxml-match? '(ul (!seq (!permute (li "a") (li "b"))
                                    (!permute (li "c") (li "d"))))
                         '(ul (li "b") (li "c") (li "a") (li "d"))))

(test* "perm+patvar" '((?a . (li "var")))
       (test-sxml-match? '(ul (!permute (li "fix") ?a))
                         '(ul (li "var") (li "fix"))
                         values))

(test* "or" #t
       (test-sxml-match? '(p (!or "a" "b" "c"))
                         '(p "b")))
(test* "or" #f
       (test-sxml-match? '(p (!or "a" "b" "c"))
                         '(p "d")))
(test* "or" #t
       (test-sxml-match? '(p (!or (!seq "a" "b") "c"))
                         '(p "a" "b")))
(test* "or" #t
       (test-sxml-match? '(p (!or (!seq "a" "b") "c"))
                         '(p "c")))
(test* "or" #t
       (test-sxml-match? '(p (!or "c" (!seq "a" "b")))
                         '(p "a" "b")))
(test* "or" #f
       (test-sxml-match? '(p (!or (!seq "a" "b") "c"))
                         '(p "a")))
(test* "or" #t
       (test-sxml-match? '(p (!or (!seq "a" "b") (!seq "a" "c")))
                         '(p "a" "c")))
(test* "or" #t
       (test-sxml-match? '(p (!or (!permute "a" "b") (!permute "a" "c")))
                         '(p "c" "a")))
(test* "or" #f
       (test-sxml-match? '(p (!or (!permute "a" "b") (!permute "a" "c")))
                         '(p "c" "b")))
(test* "or" #t
       (test-sxml-match? '(ul (!seq (!or (li "a") (li "b"))
                                    (!or (li "c") (li "d"))))
                         '(ul (li "b") (li "c"))))
(test* "or" #t
       (test-sxml-match? '(ul (!permute (!or (li "a") (li "b"))
                                        (!or (li "c") (li "d"))))
                         '(ul (li "c") (li "b"))))

(test* "repeat" #t
       (test-sxml-match? '(p (!repeat "a" "b"))
                         '(p)))
(test* "repeat" #t
       (test-sxml-match? '(p (!repeat "a" "b"))
                         '(p "a" "b")))
(test* "repeat" #t
       (test-sxml-match? '(p (!repeat "a" "b"))
                         '(p "a" "b" "a" "b")))
(test* "repeat" #t
       (test-sxml-match? '(p (!repeat "a" "b"))
                         '(p "a" "b" "a" "b" "a" "b")))
(test* "repeat" #f
       (test-sxml-match? '(p (!repeat "a" "b"))
                         '(p "a" "b" "a" "b" "a")))
(test* "repeat" #t
       (test-sxml-match? '(p (!repeat "a" "b") "a")
                         '(p "a" "b" "a" "b" "a")))
(test* "repeat" #f
       (test-sxml-match? '(p (!repeat "a" "b") "a")
                         '(p "a" "b" "a" "b" "a" "b")))
(test* "repeat" #t
       (test-sxml-match? '(ul (!repeat (!or (li "a") (li "b"))))
                         '(ul (li "a") (li "a") (li "b") (li "a"))))
(test* "repeat" #t
       (test-sxml-match? '(ul (!repeat (!or (li "a") (li "b"))))
                         '(ul)))
(test* "repeat" #f
       (test-sxml-match? '(ul (!repeat (!or (li "a") (li "b"))))
                         '(ul (li "c"))))
(test* "repeat" #t
       (test-sxml-match? '(ul (!repeat (!or (li "a") (!seq (li "b") (li "c")))))
                         '(ul (li "b") (li "c") (li "a")
                              (li "a") (li "b") (li "c"))))
(test* "repeat" #f
       (test-sxml-match? '(ul (!repeat (!or (li "a") (!seq (li "b") (li "c")))))
                         '(ul (li "b") (li "a") (li "c")
                              (li "a") (li "b") (li "c"))))
(test* "repeat" #t
       (test-sxml-match? '(dl (!repeat (dt ?a) (dd ?b)))
                         '(dl (dt "a") (dd "b") (dt "a") (dd "b"))))

(test* "contain(1)" #t
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "a"))))
(test* "contain(1)" #f
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "b"))))
(test* "contain(1)" #f
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul)))
(test* "contain(1)" #t
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "a") (li "b"))))
(test* "contain(1)" #t
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "a") (li "b"))))
(test* "contain(1)" #t
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "b") (li "a"))))
(test* "contain(1)" #t
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "c") (li "a") (li "b"))))
(test* "contain(1)" #t
       (test-sxml-match? '(ul (!contain (li "a")))
                         '(ul (li "d") (li "c") (li "a") (li "b") (li "e"))))

(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "a") (li "b"))))
(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "b") (li "a"))))
(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "a") (li "b") (li "c"))))
(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "a") (li "c") (li "b"))))
(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "b") (li "c") (li "a"))))
(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "c") (li "a") (li "b"))))
(test* "contain(2)" #t
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "c") (li "b") (li "a"))))
(test* "contain(2)" #f
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "c") (li "b") (li "b"))))
(test* "contain(2)" #f
       (test-sxml-match? '(ul (!contain (li "a") (li "b")))
                         '(ul (li "a") (li "a") (li "a"))))


(test* "contain(patvar)" '((?b . "b") (?a . "a"))
       (test-sxml-match? '(ul (!contain (li ?a) (li ?b)))
                         '(ul (li "a") (li "b") (li "c"))
                         values))
(test* "contain(patvar)" '((?a . "em") (?b . "span"))
       (test-sxml-match? '(p (!contain (em ?a) (span ?b)))
                         '(p "abcde" (span "span") "zyxw" (em "em") "pqrs")
                         values))


;;-----------------------------------------------------------------
(test-section "attribute node")

(test* "attr - normal" #t
       (test-sxml-match?
        '(input (@ (type "submit") (name "foo") (value "bar")))
        '(input (@ (name "foo") (value "bar") (type "submit")))))
(test* "attr - normal" #f
       (test-sxml-match?
        '(input (@ (type "submit") (name "foo") (value "bar")))
        '(input (@ (name "foo") (value "bar")))))
(test* "attr - normal" #f
       (test-sxml-match?
        '(input (@ (name "foo") (value "bar")))
        '(input (@ (type "submit") (name "foo") (value "bar")))))

;; this is for compatiblity w/ older test code
(test* "attr - includes directive" #t 
       (test-sxml-match?
        '(input (@ (!permute (type "submit") (name "foo") (value "bar"))))
        '(input (@ (name "foo") (value "bar") (type "submit")))))

(test* "attr - w/patvar" #t
       (test-sxml-match?
        '(input (@ ?_ (type "submit") (name "foo")))
        '(input (@ (name "foo") (value "bar") (type "submit")))))
(test* "attr - w/patvar" #t
       (test-sxml-match?
        '(input (@ (type "submit") ?_ (name "foo")))
        '(input (@ (name "foo") (value "bar") (type "submit")))))
(test* "attr - w/patvar" #f
       (test-sxml-match?
        '(input (@ (type "submit") ?_ ?_ (name "foo")))
        '(input (@ (name "foo") (value "bar") (type "submit")))))
(test* "attr - w/patvar" #t
       (test-sxml-match?
        '(input (@ (type "submit") ?*))
        '(input (@ (name "foo") (value "bar") (type "submit")))))
(test* "attr - w/patvar" #t
       (test-sxml-match?
        '(input (@ ?*))
        '(input (@ (name "foo") (value "bar") (type "submit")))))
(test* "attr - w/patvar" #t
       (test-sxml-match?
        '(input (@ (type "submit") (name "foo") (value "bar") ?*))
        '(input (@ (name "foo") (value "bar") (type "submit")))))

;;-----------------------------------------------------------------
(test-section "xml match")

(test* "xml match" #t
       (test-xml-match? '(dl (!repeat (dt ?a) (dd ?b)))
                        "<dl>
                           <dt>aaaa</dt><dd>bbbb</dd>
                           <dt>cccc</dt><dd>dddd</dd>
                         </dl>"))
(test* "xml match" #t
       (test-xml-match? '(a ?@ "foo")
                        "<a href=\"http://w3c.org\">foo</a>"))
(test* "xml match" #t
       (test-xml-match? '(ul ?*)
                        "<ul>
                           <li>aaaa</li>
                           <li>bbbb</li>
                         </ul>"))
(test* "xml match" #t
       (test-xml-match? '(ul ?*)
                        "<ul>
                         </ul>"))
(test* "xml match" #f
       (test-xml-match? '(ul ?_ ?*)
                        "<ul>
                         </ul>"))
(test* "xml match" #t
       (test-xml-match? '(ul ?_ ?*)
                        "<ul>
                           <li>aaaa</li>
                         </ul>"))

;;-----------------------------------------------------------------
(test-section "parameterization by sxpath")

(test* "identity (sxml)" #t
       ((test-sxml-select-matcher '(dl))
        '(dl (!repeat (dt ?a) (dd ?b)))
        '(dl (dt "a") (dd "b") (dt "a") (dd "b"))))
(test* "identity (sxml)" #f
       ((test-sxml-select-matcher '(dl))
        '(dl (!repeat (dt ?a) (dd ?b)))
        '(dl (dt "a") (dd "b") (dd "b") (dt "a"))))
(test* "identity (xml)" #t
       ((test-xml-select-matcher '(dl))
        '(dl (!repeat (dt ?a) (dd ?b)))
        "<dl><dt>a</dt><dd>b</dd><dt>a</dt><dd>b</dd></dl>"))
(test* "identity (xml)" #f
       ((test-xml-select-matcher '(dl))
        '(dl (!repeat (dt ?a) (dd ?b)))
        "<dl><dt>a</dt><dd>b</dd><dd>b</dd><dt>a</dt></dl>"))

(test* "extra-check (sxml)" '((?b . "b") (?a . "a"))
       ((test-sxml-select-matcher '(dl) identity)
        '(dl (!repeat (dt ?a) (dd ?b)))
        '(dl (dt "a") (dd "b"))))
(test* "extra-check (sxml)" #f
       ((test-sxml-select-matcher '(dl) identity)
        '(dl (!repeat (dt ?a) (dd ?b)))
        '(dl (dd "b") (dt "a"))))
(test* "extra-check (xml)" '((?b . "b") (?a . "a"))
       ((test-xml-select-matcher '(dl) identity)
        '(dl (!repeat (dt ?a) (dd ?b)))
        "<dl><dt>a</dt><dd>b</dd></dl>"))
(test* "extra-check (xml)" #f
       ((test-xml-select-matcher '(dl) identity)
        '(dl (!repeat (dt ?a) (dd ?b)))
        "<dl><dd>a</dd><dt>b</dt></dl>"))

(test* "selection (sxml)" #t
       ((test-sxml-select-matcher '(html body ul))
        '(ul (!repeat (li ?_)))
        '(html (head (title "foo"))
               (body (h1 "foo")
                     (ul (li "a") (li "b") (li "c"))))))
(test* "selection (sxml)" #f
       ((test-sxml-select-matcher '(html body ul))
        '(ul (!repeat (li ?_)))
        '(html (head (title "foo"))
               (body (h1 "foo") (p "abc")))))
(test* "selection (sxml)" '((?b . "def") (?a . "abc"))
       ((test-sxml-select-matcher '(html body p) identity)
        '(!seq (p ?a) (p ?b))
        '(html (head (title "foo"))
               (body (h1 "foo") (p "abc") (p "def")))))
(test* "selection (sxml)" #t
       ((test-sxml-select-matcher '(html body p))
        '(!seq (p ?_) (!repeat (p ?_)))
        '(html (head (title "foo"))
               (body (h1 "foo") (p "abc") (p "def")))))
(test* "selection (sxml)" #t
       ((test-sxml-select-matcher '(html body form input))
        '(!contain (input (@ (type "submit") ?*)))
        '(html (head (title "foo"))
               (body (h1 "foo")
                     (form (input (@ (type "text") (name "foo")))
                           (input (@ (type "text") (name "bar")))
                           (input (@ (type "submit") (name "submit")))
                           (input (@ (type "reset") (name "reset")))))))
       )
(test* "selection (sxml)" '((?a . "foo"))
       ((test-sxml-select-matcher '(html body h1) identity)
        '(h1 ?a)
        '(html (head (title "foo"))
               (body (h1 "foo") (p "abc") (p "def")))))

(test* "selection (xml)" #t
       ((test-xml-select-matcher '(html body ul))
        '(ul (!repeat (li ?_)))
        "<html><head><title>foo</title></head>
               <body><h1>foo</h1>
                     <ul><li>a</li><li>b</li><li>c</li></ul></body></html>"))
(test* "selection (xml)" #f
       ((test-xml-select-matcher '(html body ul))
        '(ul (!repeat (li ?_)))
        "<html><head><title>foo</title></head>
               <body><h1>foo</h1>
                     <p>abc</p></body></html>"))
(test* "selection (xml)" '((?b . "def") (?a . "abc"))
       ((test-xml-select-matcher '(html body p) identity)
        '(!seq (p ?a) (p ?b))
        "<html><head><title>foo</title></head>
               <body><h1>foo</h1>
                     <p>abc</p><p>def</p></body></html>"))

(test-end)
