;; test supervisor scripts in http mode. -*-mode: scheme-*-
;; this test isn't for modules, but for actual scripts.
;; $Id: spvr-httpd.scm,v 1.1 2004/11/01 09:37:03 shiro Exp $

(use gauche.test)
(use gauche.process)
(use rfc.http)
(use kahua.test.xml)
(use file.util)

(test-start "supervisor script (http mode)")

(define *spvr* #f)

(define *port* 27490)

;;-----------------------------------------------------------
(test-section "start kahua-spvr with httpd")

(test* "start" #t
       (let* ((p (run-process "../src/kahua-spvr" "--test"
                              "--httpd" (x->string *port*) "-l" "/dev/null")))
         (set! *spvr* p)
         #t))

(sys-sleep 3) ;; give time for kahua-spvr to start

;;-----------------------------------------------------------
(test-section "access to the page")

(test* "httpd get" '("200" #t)
       (receive (status headers body)
           (http-get #`"localhost:,*port*" "/")
         (list status
               (test-xml-match? 
                '(html (head (title "Lambda books") ?*)
                       (body ?*))
                body))))

(test* "httpd get (nonexistent)" "404"
       (receive (status headers body)
           (http-get #`"localhost:,*port*" "/zzzz")
         status))

(test* "httpd get (static path)" `("200" ,(file-size "../tmp/checkout/lambdabooks/images/lambda-books-logo.png"))
       (receive (status headers body)
           (http-get #`"localhost:,*port*" "/doc/lambdabooks/images/lambda-books-logo.png")
         (list status
               (string-size body))))

;; Shutdown
(process-send-signal *spvr* SIGTERM)

(test-end)

                              
