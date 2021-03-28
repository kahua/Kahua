;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Provides common HTTP handlers.
;;
;;  Copyright (c) 2006-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2006-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(define-module kahua.protocol.http
  (use srfi-1)
  (use rfc.cookie)
  (use rfc.uri)
  (use util.list)
  (use util.match)
  (use text.html-lite)
  (use text.tree)
  (use file.util)
  (use gauche.charconv)
  (use gauche.sequence)
  (use kahua.util)
  (use kahua.config)
  (use kahua.protocol.worker)
  (export *default-content-type*
          http-date
          parse-path-info
          unsupported-method?
          abs-uri
          kahua-header->http-header
          send-http-header
          send-http-body
          ;;default-error-page
          current-error-page
          output-error-page
          default-redirect-page
          output-redirect-page
          http-status-string
          print-status-line
          kahua-error->status
          ))
(select-module kahua.protocol.http)

(define-constant *default-content-type* (make-content-type "text/html"))

(define (http-date t)
  (time->rfc1123-string t))

;; path-info list -> scheme host port worker worker-uri args
;;
;; Elements of path-info must be decoded.
;;
;; e.g.
;;   () => #f #f #f #f #f ()
;;   ("worker") => #f #f #f "worker" #f ()
;;   ("worker" "arg1" ...) => #f #f #f "worker" #f ("arg1" ...)
;;   ("worker" "--vh--http:www.kahua.org:80" "worker-url" ... "--" "arg1" ...)
;;     => http "www.kahua.org" 80 "worker" ("worker-url" ...) ("arg1" ...)
;;   ("worker" "--vh--https:karetta.jp:443 "worker-url" ... "--" "arg1" ...)
;;     => https "karetta.jp" 443 "worker" ("worker-url" ...) ("arg1" ...)
;;
(define (parse-path-info path-info)
  (define (vhosting str)
    (and-let* ((m (#/^--vh--/ str))) (rxmatch-after m)))
  (define (parse-vhost vhost fail-cont)
    (and-let* ((m (#/^(https?):/ vhost))
               (scheme (string->symbol (m 1)))
               (host (rxmatch-after m)))
      (rxmatch-case host
        (#/^(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}):(\d+)$/ ; IPv4 address w/ port
            (#f h p) (values scheme h p))
        (#/^\[([0-9A-Fa-f:]+)\]:(\d+)$/                  ; IPv6 address w/ port
              (#f h p) (values scheme h p))
        (#/^([-a-zA-Z0-9.]+):(\d+)$/                     ; Host name
            (#f h p) (values scheme h p))
        (else (fail-cont)))))
  (define (split-by ls pred?)
    (let loop ((head '())
               (tail ls))
      (if (null? tail)
          (list (reverse! head) tail)
          (let1 e (car tail)
            (if (pred? e)
                (list (reverse! head) (cdr tail))
                (loop (cons e head) (cdr tail)))))))

  (guard (e (else (if (null? path-info)
                      (values #f #f #f #f '() '())
                      (let1 w (car path-info)
                        (values #f #f #f w (list w) (cdr path-info))))))
    (match path-info
      ((worker (= vhosting (? identity vhost)) . args) (=> next)
       (receive (scheme host port) (parse-vhost vhost next)
         (apply values scheme host port worker (split-by args (pa$ equal? "--")))))
      ((worker . args) (values #f #f #f worker #f args))
      (()              (values #f #f #f #f #f '())))))

;; Now support method GET, HEAD, POST only.
(define (unsupported-method? method)
  (case method
    ((GET HEAD POST) #f)
    (else            #t)))

(define (default-error-page status msg)
  (let1 status-msg (http-status-string status #f)
  `(,(html-doctype)
    ,(html:html
      (html:head (html:title status-msg))
      (html:body
       (html:h1 status-msg)
       (html:p (html-escape-string msg)))))))

(define (current-error-page status message)
  (or (and-let* ((path (kahua-error-document status)))
        (guard (e (else #f))
          (file->string-list path)))
      (default-error-page status message)))

(define (output-error-page out status msg)
  (write-tree (current-error-page status msg) out))

(define (default-redirect-page dest)
  `(,(html-doctype)
    ,(html:html
      (html:head (html:title "Now redirecting..."))
      (html:body (html:h1 "Redirecting to " (html:a :href dest "here") "...")))))

(define (output-redirect-page out dest)
  (write-tree (default-redirect-page dest) out))

(define (abs-uri uri base)
  (receive (scheme spec) (uri-scheme&specific uri)
    (if scheme
        uri
        (string-append base spec))))

(define (kahua-header->http-header kheader . maybe-path)
  (define (make-cookie e)
    (define (make-karg k v)
      (if v (list k (x->string v)) '()))
    (cons "set-cookie"
          (construct-cookie-string
           (or (and-let* ((domain (assoc-ref kheader "x-kahua-session-domain")))
                 (receive (scheme host port path) (apply values domain)
                   `(("x-kahua-sgsid" ,(cadr e)
                      ,@(make-karg :domain host)
                      ,@(make-karg :path path)
                      ,@(make-karg :port port)
                      ,@(make-karg :secure (equal? "https" scheme))
                      :discard #t
                      :version 1
                      ))))
               `(("x-kahua-sgsid" ,(cadr e) :discard #t :version 1))))))

  (filter-map (lambda (e)
                (rxmatch-case (car e)
                  (#/^x-kahua-sgsid$/ (#f) (make-cookie e))
                  (#/^x-kahua-/ (#f) #f)
                  (#/(?i:^location$)/ (h)
                     (list h (abs-uri (cadr e) (assoc-ref-car kheader "x-kahua-server-uri"))))
                  (else e)))
              kheader))

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

(define (kahua-error->status error)
  (cond ((<kahua-spvr-not-respond> error)     503)
        ((<kahua-worker-not-found> error)     404)
;;	((<kahua-entry-not-found> error)      404)
        ((<kahua-worker-not-respond> error)   503)
        ((<kahua-spvr-session-expired> error) 410)
        (else                                 500)))

(provide "kahua/protocol/http")
