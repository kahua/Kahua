;; -*- coding: utf-8 ; mode: scheme -*-
;; Test kahua.server module

;; The kahua.server in the "real situation" would be tested by
;; worker and spvr tests.  This module tests the surface API.

;; This test also accesses kahua.session.

(use srfi-1)
(use gauche.test)
(use gauche.parameter)
(use kahua.gsid)
(use kahua.session)
(use kahua.test.xml)
(use kahua.config)
(use util.list)

(test-start "kahua.server")
(use kahua.server)
(test-module 'kahua.server)

(define *site* "_site")
(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)

(kahua-common-init *site* #f)

;;---------------------------------------------------------------
(test-section "initialization")

(test* "kahua-init-server" #t
       (string? (kahua-init-server "dummy" #f)))

(test* "kahua-bridge-name" "/kahua.cgi"
       (kahua-bridge-name))

;;---------------------------------------------------------------
(test-section "utilities")

(test* "kahua-merge-headers" '(("a" "b"))
       (kahua-merge-headers '(("a" "b"))))

(test* "kahua-merge-headers" '(("e" "z") ("a" "x") ("c" "d"))
       (kahua-merge-headers '(("a" "b") ("c" "d"))
                            '(("a" "x") ("e" "z"))))

(test* "kahua-merge-headers" '(("c" "p") ("f" "q") ("e" "z") ("a" "x"))
       (kahua-merge-headers '(("a" "b") ("c" "d"))
                            '(("a" "x") ("e" "z"))
                            '(("f" "q") ("c" "p"))))

;;---------------------------------------------------------------
(test-section "dispatcher")

(define (a-cont . _) '(p "a-cont"))

(define (a-renderer body context)
  (values `(html (head (title "test")) (body ,body)) context))

(test* "kahua-default-handler"
       '(html (head (title "test"))
              (body (p "a-cont")))
       (let ((cgsid (session-cont-register a-cont)))
         ((kahua-default-handler `(("x-kahua-cgsid" ,cgsid))
				 '()
				 (lambda (h b) b)
				 identity
				 :render-proc a-renderer))))

(test* "kahua-default-handler (stale proc)"
       '(html (head (title "test"))
              (body (p "unknown session key")))
       ((kahua-default-handler `(("x-kahua-cgsid" "bongobongo"))
			       '()
			       (lambda (h b) b)
			       identity
			       :render-proc a-renderer
			       :stale-proc (lambda _
					     '(p "unknown session key")))))

(test* "kahua-default-handler (session state)"
       '(html (head (title "test"))
              (body "kahua!"))
       (let* ((b-cont (lambda ()
                        (let1 state (kahua-context-ref "session-state")
                          (set! (ref state 'a) "kahua!"))))
              (c-cont (lambda ()
                        (let1 state (kahua-context-ref "session-state")
                          (ref state 'a))))
              (cgsid1 (session-cont-register values))
              (cgsid2 (session-cont-register b-cont))
              (cgsid3 (session-cont-register c-cont))
              (sgsid  #f)
              )
         ((kahua-default-handler `(("x-kahua-cgsid" ,cgsid1))
				 '()
				 (lambda (h b)
				   (receive (state cont)
				       (get-gsid-from-header h)
				     (set! sgsid state)
				     #f))
				 identity
				 :render-proc a-renderer))
         ((kahua-default-handler `(("x-kahua-sgsid" ,sgsid)
				   ("x-kahua-cgsid" ,cgsid2))
				 '()
				 (lambda (h b) b)
				 identity
				 :render-proc a-renderer))
         ((kahua-default-handler `(("x-kahua-sgsid" ,sgsid)
				   ("x-kahua-cgsid" ,cgsid3))
				 '()
				 (lambda (h b) b)
				 identity
				 :render-proc a-renderer))
         ))


;;---------------------------------------------------------------
(test-section "entries")

(define-syntax call-entry
  (syntax-rules ()
    ((call-entry entry context)
     (parameterize ((kahua-current-context context))
       ((values-ref (session-cont-get (symbol->string entry)) 0))))))

(test* "define-entry"
       '(("usr" "var" "www" "xxX" #f "Zzzz")
         (#f #f #f "xxx" "yyy" "zzz")
         (#f #f #f #f #f #f))
       (let ()
         (eval
          '(define-entry (foo a b c :keyword x y z)
             (list a b c x y z))
          (current-module))
         (list
          (call-entry 'foo
                      '(("x-kahua-path-info" ("usr" "var" "www" "zzz"))
                        ("z" "Zzzz")
                        ("x" "xxX")))
          (call-entry 'foo
                      '(("x-kahua-path-info" ())
                        ("x" "xxx")
                        ("y" "yyy")
                        ("z" "zzz")))
          (call-entry 'foo '())
          )))

(test* "define-entry (multi-value bind parameter)"
       '(("usr" "var" "www" ("xxX" "Xxx" "xXx") () ("Zzzz"))
         (#f #f #f ("xxx") ("yyy") ("zzz"))
         (#f #f #f () () ()))
       (let ()
         (eval
          '(define-entry (bar a b c :multi-value-keyword x y z)
             (list a b c x y z))
          (current-module))
         (list
          (call-entry 'bar
                      '(("x-kahua-path-info" ("usr" "var" "www" "zzz"))
                        ("z" "Zzzz")
                        ("x" "xxX" "Xxx" "xXx")))
          (call-entry 'bar
                      '(("x-kahua-path-info" ())
                        ("x" "xxx")
                        ("y" "yyy")
                        ("z" "zzz")))
          (call-entry 'bar '())
          )))

(test* "define-entry (bind closure)"
       '(1 2 3)
       (let ()
         (eval
          '(define-entry counter
             (let1 i 0
               (entry-lambda ()
                 (inc! i))))
          (current-module))
         (list
          (call-entry 'counter ())
          (call-entry 'counter ())
          (call-entry 'counter ()))))

;; make sure 'foo' is registered globally.
(test* "define-entry & session"
       '("usr" "var" "www" "xxX" #f "Zzzz")
       (call-entry 'foo
                   '(("x-kahua-path-info" ("usr" "var" "www" "zzz"))
                     ("z" "Zzzz")
                     ("x" "xxX"))))


;; test :rest argument variations
(let ((env '(("x-kahua-path-info" ("usr" "var" "www" "zzz"))
             ("x" "xxx")
             ("y" "yyy"))))
  (test* "define-entry (:rest arg - 1)"
         '("usr" "var" ("www" "zzz"))
         (let ()
           (eval '(define-entry (foo a b :rest c) (list a b c))
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 2)"
         '("usr" "var" "www" "zzz")
         (let ()
           (eval '(define-entry (foo :rest a) a)
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 3)"
         '(("usr" "var" "www" "zzz") "xxx" "yyy")
         (let ()
           (eval '(define-entry (foo :rest a :keyword y x) (list a x y))
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 3)"
         '(("usr" "var" "www" "zzz") "xxx" "yyy")
         (let ()
           (eval '(define-entry (foo :rest a :keyword y x) (list a x y))
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 4)"
         '(("usr" "var" "www" "zzz") "xxx" "yyy")
         (let ()
           (eval '(define-entry (foo :keyword y x :rest a) (list a x y))
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 5)"
         '("usr" "var" ("www" "zzz") "xxx" "yyy")
         (let ()
           (eval '(define-entry (foo a b :keyword y x :rest c) (list a b c x y))
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 6)"
         '("usr" "var" ("www" "zzz"))
         (let ()
           (eval '(define-entry (foo a b :keyword :rest c) (list a b c))
                 (current-module))
           (call-entry 'foo env)))
  (test* "define-entry (:rest arg - 7)"
         '()
         (let ()
           (eval '(define-entry (foo :rest a) a)
                 (current-module))
           (call-entry 'foo '(("x-kahua-path-info" ())))))
  (test* "define-entry (bad :rest arg - 1)"
         *test-error*
         (eval '(define-entry (foo :rest) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 2)"
         *test-error*
         (eval '(define-entry (foo :rest a b) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 3)"
         *test-error*
         (eval '(define-entry (foo a b :rest) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 4)"
         *test-error*
         (eval '(define-entry (foo a b :rest c d) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 5)"
         *test-error*
         (eval '(define-entry (foo a b :rest :keyword x y) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 6)"
         *test-error*
         (eval '(define-entry (foo a b :rest c d :keyword x y) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 7)"
         *test-error*
         (eval '(define-entry (foo a b :keyword x y :rest) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 8)"
         *test-error*
         (eval '(define-entry (foo a b :keyword x y :rest z q) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 9)"
         *test-error*
         (eval '(define-entry (foo a b :rest :mvkeyword z) #f)
               (interaction-environment)))
  (test* "define-entry (bad :rest arg - 10)"
         *test-error*
         (eval '(define-entry (foo a b :rest :multi-value-keyword z) #f)
               (interaction-environment)))
  (test* "define-entry (bad keyword tail - 11)"
         *test-error*
         (eval '(define-entry (foo a b :rest :bad-keyword z) #f)
               (interaction-environment)))
  (test* "define-entry (bad unknown keyword - 1)"
         *test-error*
         (eval '(define-entry (foo a b :bad-keyword z) #f)
               (interaction-environment)))
  (test* "define-entry (bad keyword tail - 1)"
         *test-error*
         (eval '(define-entry (foo a b :keyword) #f)
               (interaction-environment)))
  (test* "define-entry (bad keyword tail - 2)"
         *test-error*
         (eval '(define-entry (foo a b :mvkeyword) #f)
               (interaction-environment)))
  (test* "define-entry (bad keyword tail - 3)"
         *test-error*
         (eval '(define-entry (foo a b :multi-value-keyword) #f)
               (interaction-environment)))
  )

;;---------------------------------------------------------------
(test-section "extra-header element")

(test* "extra-header" '((("foo" "bar")) ())
       ((kahua-default-handler
	 '()
	 '()
	 (lambda (h b)
	   (list (filter (lambda (e) (not (#/^x-(?:kahua|robots)-/ (car e)))) h) b))
	 (lambda ()
	   '((extra-header (@ (name "foo") (value "bar"))))))))

(test* "extra-header" '(("foo" "bar") ("voo" "doo"))
       ((kahua-default-handler
	 '()
	 '()
	 (lambda (h b)
	   (filter (lambda (e) (not (#/^x-(?:kahua|robots)-/ (car e)))) h))
	 (lambda ()
	   '((html
	      (head
	       (extra-header (@ (name "foo") (value "bar")))
	       (title "hoge"))
	      (body
	       (p
		(extra-header (@ (name "voo") (value "doo")))))))))))


;;---------------------------------------------------------------
(test-section "kahua-current-entry-name")

(test* "check entry name"
       "my-entry"
       (let ()
         (eval
          '(define-entry (my-entry)
             (kahua-current-entry-name))
          (current-module))
         (call-entry 'my-entry
                     '()
                     )))


;;---------------------------------------------------------------
(test-section "JSON")

(test* "check json string"
       "(\"str\")\n"
       (kahua-render '((json "str")) ()))

(test* "check json number"
       "(1)\n"
       (kahua-render '((json 1)) ()))

(test* "check json object"
       "({a: 1})\n"
       (kahua-render '((json #((a . 1)))) ()))

(test* "check json array"
       "([\"str\",1,{a: 1}])\n"
       (kahua-render '((json ("str"
                              1
                              #((a . 1))))) ()))

(test* "check json true"
       "(true)\n"
       (kahua-render '((json #t)) ()))

(test* "check json false"
       "(false)\n"
       (kahua-render '((json #f)) ()))


(define-class <jsonable> (<json-base>)
  ((a :json #t
      :init-keyword :a)
   (b :init-keyword :b)
   (c :json #t
      :init-keyword :c)))

(test* "check json <json-base>"
       "({a: 1,c: \"str\"})\n"
       (kahua-render `((json ,(make <jsonable>
                                :a 1
                                :b 2
                                :c "str"))) ()))

;;---------------------------------------------------------------
(test-section "Conditional Comments for Internet Explorer")

(test* "with-ie element"
       "<html><!--[if IE]><span>IE</span\n><![endif]--></html\n>"
       (kahua-render `((html (with-ie (span "IE")))) '()))

(test* "with-ie element with condition"
       "<html><!--[if gte IE 5]>hello<![endif]--></html\n>"
       (kahua-render `((html (with-ie (@ (condition "gte IE 5")) "hello"))) '()))

(test-section "No Escape strings")

(test* "entity reference"
       "<h3>&nbsp;</h3\n>"
       (kahua-render `((h3 (& "nbsp"))) '()))

(test* "entity reference with many comtents"
       "<h3>&amp;&nbsp;</h3\n>"
       (kahua-render `((h3 (& "amp" "nbsp"))) '()))


;;(test-section "No Escape strings")

;;(test* "no-escape element"
;;       "<h3>&nbsp;</h3\n>"
;;       (kahua-render `((h3 (no-escape "&nbsp;"))) '()))

;;(test* "no-escape element with many comtents"
;;       "<h3>&amp;&nbsp;</h3\n>"
;;       (kahua-render `((h3 (no-escape "&amp;" "&nbsp;"))) '()))


(test-end)

;; Local variables:
;; mode: scheme
;; end:
