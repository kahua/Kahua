;;; -*- mode: scheme; coding: utf-8 -*-
(define-module kahua.protocol.http
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use rfc.cookie)
  (use rfc.uri)
  (use util.list)
  (use text.html-lite)
  (use text.tree)
  (use gauche.charconv)
  (use gauche.sequence)
  (use kahua.util)
  (use kahua.protocol.worker)
  (export *default-content-type*
	  http-date
	  path->path-info
	  path-info->abs-path
	  normalize-path
	  rel-path-info
	  unsupported-method?
	  abs-uri
	  kahua-header->http-header
	  send-http-header
	  send-http-body
	  default-error-page
	  http-status-string
	  print-status-line
	  ))
(select-module kahua.protocol.http)

(define-constant *default-content-type*
  (let1 encoding (gauche-character-encoding)
    (if (eq? 'none encoding)
	"text/html"
        (format "text/html; charset=~s" encoding))))

(define (http-date t)
  (time->rfc1123-string t))

(define (path->path-info path)
  (define (simplify-path-info path-info)
    (reverse!
     (fold (lambda (comp res)
	     (cond ((string-null? comp) res)
		   ((string=? "." comp) res)
		   ((string=? ".." comp)
		    (if (null? res)
			res
			(cdr res)))
		   (else (cons comp res))))
	   '()
	   path-info)))
  (simplify-path-info (string-split path #[/])))

(define (path-info->abs-path path-info)
  (if (null? path-info)
      "/"
      (string-join path-info "/" 'prefix)))

(define normalize-path (compose path-info->abs-path path->path-info))

(define (rel-path-info path-info base-info)
  (let loop ((p path-info)
	     (b base-info))
    (cond ((null? b) p)
	  ((null? p) #f)
	  ((string=? (car p) (car b)) (loop (cdr p) (cdr b)))
	  (else #f))))

;; Now support method GET, HEAD, POST only.
(define (unsupported-method? method)
  (case method
    ((PUT DELETE OPTIONS TRACE CONNECT) #t)
    (else                               #f)))

(define (default-error-page out status msg)
  (display "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\r\n" out)
  (display "<html><head>\r\n" out)
  (let1 status-line (http-status-string status #f)
    (format out "<title>~a</title>\r\n" status-line)
    (format out "</head><body>\r\n<h1>~a</h1>\r\n<p>~a</p>\r\n</body></html>\r\n"
	    status-line (html-escape-string msg))))

(define (abs-uri uri base)
  (receive (scheme spec) (uri-scheme&specific uri)
    (if scheme
	uri
	(string-append base spec))))

(define (kahua-header->http-header kheader . maybe-path)
  (let1 path (get-optional maybe-path "/")
    (filter-map (lambda (e)
		  (rxmatch-case (car e)
		    (#/^x-kahua-sgsid$/ (#f)
		       (cons "set-cookie" (construct-cookie-string
					   `(("x-kahua-sgsid" ,(cadr e) :path ,path)))))
		    (#/^x-kahua-/ (#f) #f)
		    (#/(?i:^location$)/ (h)
		       (list h (abs-uri (cadr e) (assoc-ref-car kheader "x-kahua-server-uri"))))
		    (else e)))
		kheader)))

(define (send-http-header out header)
  (define (display-titlecase name)
    (fold (lambda (c prev)
	    (write-char ((if (char=? #\- prev)
			     char-upcase
			     char-downcase)
			 c))
	    c)
	  #\-
	  name))
  (with-output-to-port out
    (lambda ()
      (for-each (lambda (f)
		  (display-titlecase (car f))
		  (display ": ")
		  (write-tree (cdr f))
		  (display "\r\n"))
		header))))

(define (send-http-body out encoding body)
  (unless (null? body)
    (case (car body)
      ((file) (call-with-input-file (cadr body) (cut copy-port <> out)))
      (else
       (if (ces-equivalent? encoding (gauche-character-encoding))
	   (write-tree body out)
	   (with-output-conversion out (cut write-tree body) :encoding encoding))))))

;; HTTP Status code and message.
;; From RFC2616.
(define-constant *STATUS-TABLE*
  (let ()
    (define (status-entry status message)
      (cons status (format "~d ~a" status message)))
    (list (status-entry 100 "Continue")
	  (status-entry 101 "Switching Protocols")
	  (status-entry 200 "OK")
	  (status-entry 201 "Created")
	  (status-entry 202 "Accepted")
	  (status-entry 203 "Non-Authoritative Information")
	  (status-entry 204 "No Content")
	  (status-entry 205 "Reset Content")
	  (status-entry 206 "Partial Content")
	  (status-entry 300 "Multiple Choices")
	  (status-entry 301 "Moved Permanently")
	  (status-entry 302 "Found")
	  (status-entry 303 "See Other")
	  (status-entry 304 "Not Modified")
	  (status-entry 305 "Use Proxy")
	  (status-entry 307 "Temporary Redirect")
	  (status-entry 400 "Bad Request")
	  (status-entry 401 "Unauthorized")
	  (status-entry 402 "Payment Required")
	  (status-entry 403 "Forbidden")
	  (status-entry 404 "Not Found")
	  (status-entry 405 "Method Not Allowed")
	  (status-entry 406 "Not Acceptable")
	  (status-entry 407 "Proxy Authentication Required")
	  (status-entry 408 "Request Time-out")
	  (status-entry 409 "Conflict")
	  (status-entry 410 "Gone")
	  (status-entry 411 "Length Required")
	  (status-entry 412 "Precondition Failed")
	  (status-entry 413 "Request Entity Too Large")
	  (status-entry 414 "Request-URI Too Large")
	  (status-entry 415 "Unsupported Media Type")
	  (status-entry 416 "Requested range not satisfiable")
	  (status-entry 417 "Expectation Failed")
	  (status-entry 500 "Internal Server Error")
	  (status-entry 501 "Not Implemented")
	  (status-entry 502 "Bad Gateway")
	  (status-entry 503 "Service Unavailable")
	  (status-entry 504 "Gateway Time-out")
	  (status-entry 505 "HTTP Version not supported")
	  )))

(define (http-status-string status version)
  (assq-ref *STATUS-TABLE* status))

(define (print-status-line out status version)
  (format out "~a ~a\r\n" (or version 'HTTP/1.0) (http-status-string status version)))

(provide "kahua/protocol/http")
