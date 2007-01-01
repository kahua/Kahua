(use gauche.test)

(test-start "kahua.css")
(use text.tree)
(use kahua.css)
(test-module 'kahua.css)

(define (css-compare str1 str2)
  (define normalize
    (lambda (str)
      (regexp-replace-all
       #/^\n/
       (regexp-replace-all #/^\n|\n\n+|\n\s*/ str "\n")
       "")))
  (string=? (normalize str1)
            (normalize str2)))

(define test-selector
  (compose tree->string
           (with-module kahua.css parse-selector)))
  
(define test-ruleset
  (compose tree->string
           (with-module kahua.css parse-ruleset)))

(define test-stylesheet
  (compose tree->string
           (with-module kahua.css parse-stylesheet)))

(define test-at-rule
  (compose tree->string
           (with-module kahua.css parse-at-rule)))

(define test-statement
  (compose tree->string
           (with-module kahua.css parse-statement)))

(test-section "selector")

(test* "Universal selector" "*"
       (test-selector '*))

(test* "Type selectors" "E"
       (test-selector 'E))

(test* "Descendant selectors" "a b c"
       (test-selector '(>> a b c)))

(test* "Child selectors" "a > b > c"
       (test-selector '(> a b c)))

(test* "Pseudo-classselectors" "E:first-child"
       (test-selector 'E:first-child))

(test* "The :lang() pseudo-class" "E:lang(c)"
       (test-selector '(lang E c)))

(test* "Adjacent selectors" "E + F"
       (test-selector '(+ E F)))

(test* "Attribute selectors(attribute set)" "E[foo]"
       (test-selector '(@ E foo)))

(test* "Attribute selectors(attribute set) 2" "E[foo][baa]"
       (test-selector '(@ E foo baa)))

(test* "Attribute selectors(attribute value is exactly equal)"
       "E[foo=\"warning\"]"
       (test-selector '(@ E (= foo "warning"))))

(test* "Attribute selectors(attribute value is exactly equal) 2"
       "E[foo=\"warning\"][baa=\"notify\"][hoge]"
       (test-selector '(@ E (= foo "warning")
                            (= baa "notify")
                            hoge)))

(test* "Attribute selectors(attribute value is a list of space-separated values, one of which is exactly equal)"
       "E[foo~=\"warning\"]"
       (test-selector '(@ E (~= foo "warning"))))

(test* "Attribute selectors(attribute value is a list of hyphen-separated values, one of which is exactly equal)"
       "E[foo|=\"warning\"]"
       (test-selector '(@ E (-= foo "warning"))))

(test* "Class selectors" "DIV.warning"
       (test-selector 'DIV.warning))

(test* "ID selectors" "E#myid"
       (test-selector 'E$myid))

(test* "ID selectors" "#myid"
       (test-selector '$myid))

(test* "Grouping" "h1, h2, h3"
       (test-selector '(or h1 h2 h3)))

(test* "mix" "h1, h2 > h3[title]"
       (test-selector '(or h1 (> h2 (@ h3 title)))))

(test-section "ruleset")

(test* "ruleset" "
h1 {
    background-color:red
}
"
       (test-ruleset '(h1 (background-color red)))
       css-compare)

(test* "ruleset with !important" "
h1 {
    background-color:red !important
}
"
       (test-ruleset '(h1 (background-color red !important)))
       css-compare)

(test-section "at-rule")

(test* "The @import rule" "import \"test.css\";"
       (test-at-rule '(import "test.css")))

(test* "The @import rule 2" "import url(\"test.css\");"
       (test-at-rule '(import (url "test.css"))))

(test* "The @import rule with media" "import url(\"test.css\") projection, tv;"
       (test-at-rule '(import (url "test.css") (or projection tv))))

(test* "The @media rule"
       "
media print, screen {
    h1 {
        background-color:red
    }
}
"
       (test-at-rule '(media (or print screen)
                             (h1 (background-color red))))
       css-compare)

(test* "The @page rule" "
page  {
    margin:3cm
}
"
       (test-at-rule '(page (margin 3cm)))
       css-compare)

(test* "The @page rule" "
page :left {
    margin:3cm
}
"
       (test-at-rule '((page :left) (margin 3cm)))
       css-compare)

(test* "The @charset rule" "charset \"UTF-8\";"
       (test-at-rule '(charset "UTF-8")))

(test-section "ruleset")

(test* "Ruleset" "
h1 {
    color:red;
    width:100px
}
"
       (test-ruleset '(h1 (color red)
                          (width 100px)))
       css-compare)

(test* "Ruleset" "
h1 h2, h1 > h3 {
    backgroun-color:blue
}
"
       (test-ruleset '((or (>> h1 h2) (> h1 h3))
                       (backgroun-color blue)))
       css-compare)

(test-section "statement")

(test* "statement" "
h1 {
height:10px
}
"
       (test-statement '(h1 (height 10px)))
       css-compare)

(test* "statement" "@import url(\"a.css\");\n\n"
       (test-statement '(@ (import (url "a.css")))))


(test-section "stylesheet")

(test* "stylesheet"
       "
@import url(\"a.css\");

h1 {
    height:10px
}

@media print {
    h1 {
        height:1em
    }
}
"
       (test-stylesheet '((@ (import (url "a.css")))
                          (h1 (height 10px))
                          (@ (media print
                                    (h1 (height 1em)))
                             )))
       css-compare)

(test-section "color")

(define sample-color (make <css:color> :r 220 :g 204 :b 50))

(test* "color write-object" "#,(css:color 220 204 50)"
       (x->string sample-color))

(test* "test color" "h1 {\ncolor:rgb(220,204,50)\n}\n\n"
       (test-statement `(h1 (color ,sample-color))))

(test* "color reader-constractor" "rgb(1,2,3)"
       (css:value-of #,(css:color 1 2 3)))

(test* "color reader-constractor" "rgb(26,51,76)"
       (css:value-of #,(css:color 10% 20% 30%)))

(test* "color reader-constractor" "rgb(170,255,187)"
       (css:value-of #,(css:color "#aa" "#f" "#b")))

(test* "color reader-constractor" "rgb(255,0,0)"
       (css:value-of #,(css:color red)))

(test* "color css:darken" "rgb(93,86,22)"
       (css:value-of (css:darken sample-color 0.5)))

(test* "color css:brighten" "rgb(348,323,79)"
       (css:value-of (css:brighten sample-color 0.5)))

(test* "color css:darken css:brighten" "rgb(221,205,51)"
       (css:value-of (css:darken (css:brighten sample-color 0.5) 0.5)))

(test* "color css:darken css:brighten" "rgb(128,0,128)"
       (css:value-of (css:blend #,(css:color red) #,(css:color blue))))

(test* "color css:bright?" #t
       (css:bright? sample-color))

(test* "color css:dark?" #f
       (css:dark? sample-color))

(test* "color css:brighten" #f
       (css:bright? (css:darken sample-color 0.5)))

(test-end)
