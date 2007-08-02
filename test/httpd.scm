;; -*- coding: utf-8 ; mode: scheme -*-
;; test supervisor scripts in http mode.
;; this test isn't for modules, but for actual scripts.

(use gauche.test)
(use gauche.process)
(use rfc.http)
(use kahua.test.xml)
(use file.util)

(test-start "kahua-httpd script")

(define *spvr* #f)

(define *port* 27490)

(define (http-get/retry host path count)
  (let/cc ret
    (dotimes (i 10)
      (guard (e (else #f))
	(receive (status headers body)
	    (http-get host path)
	  (ret status headers body)))
      (sys-sleep 1))
    (error "httpd-get: retry error")))

;;-----------------------------------------------------------
(test-section "start kahua-spvr with kahua-httpd")

(test* "start" #t
       (let* ((p (run-process "../src/kahua-spvr" "--test"
                              "--httpd" (x->string *port*) "> /dev/null")))
         (set! *spvr* p)
         #t))

(sys-sleep 3) ;; give time for kahua-spvr to start

;;-----------------------------------------------------------
(test-section "access to the page")

(test* "httpd get" '("200" #t)
       (receive (status headers body)
           (http-get/retry #`"localhost:,*port*" "/lambdabooks" 10)
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

                              
