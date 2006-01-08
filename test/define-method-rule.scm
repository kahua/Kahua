(use gauche.test)
(test-start "define-method-rule")
(use kahua.server)

(define-method-rule rule (x)
  (list "(x)" x))

(define-method-rule rule (a b)
  (list "(a b)"
        a b))

(define-method-rule rule ("a" "b")
  (list "(\"a\" \"b\")"))

(define-method-rule rule ("c" x)
  (list "(\"c\" x)"
        x))

(define-method-rule rule ("d" x)
  (list "(\"d\" x)"
        x))

(test* "(x)" '("(x)" "X")
       (rule "X"))

(test* "(a b)" '("(a b)" "A" "B")
       (rule "A" "B"))

(test* "(\"a\" \"b\")" '("(\"a\" \"b\")")
       (rule "a" "b"))

(test* "(\"c\" x)" '("(\"c\" x)" "X")
       (rule "c" "X"))

(test* "(\"d\" x)" '("(\"d\" x)" "X")
       (rule "d" "X"))

;; TODO more test

(test-end)
