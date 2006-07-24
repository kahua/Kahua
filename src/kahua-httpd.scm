;;; -*- mode: scheme; coding: utf-8-unix -*-
;; HTTPd for kahua
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-httpd.scm,v 1.1.2.1 2006/07/24 15:55:08 bizenn Exp $

(use srfi-1)
(use srfi-11)
(use srfi-13)
(use rfc.822)
(use rfc.uri)
(use rfc.cookie)
(use rfc.mime)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use util.list)
(use util.match)
(use file.util)
(use gauche.net)
(use gauche.logger)
(use gauche.selector)
(use gauche.parseopt)
(use gauche.collection)
(use gauche.parameter)
(use gauche.charconv)
(use kahua.gsid)
(use kahua.util)
(use kahua.config)
(use kahua.thread-pool)

(define *default-sigmask* #f)
(define-constant *TERMINATION-SIGNALS*
  (sys-sigset-add! (make <sys-sigset>) SIGTERM SIGINT SIGHUP))

(define-constant *default-content-type*
  (let1 encoding (gauche-character-encoding)
    (if (eq? 'none encoding)
	"text/html"
        (format "text/html; charset=~s" encoding))))

(define (log-prefix drain)
  (format "[~s] " (current-thread)))

(define-constant *GATEWAY-INTERFACE* "CGI/1.1")

(define-constant *DEFAULT-ENCODING*
  (case (gauche-character-encoding)
    ((utf-8)  'UTF-8)
    ((euc-jp) 'EUC-JP)
    ((sjis)   'Shift_JIS)
    (else     #f)))

(define-condition-type <kahua-error> <error> #f)
(define-condition-type <kahua-worker-not-found> <kahua-error> #f (uri uri-of))
(define-condition-type <kahua-worker-error> <kahua-error> #f)
(define-condition-type <http-bad-request> <kahua-error> #f (request request-of))

(define (default-error-page out status msg)
  (display "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\r\n" out)
  (display "<html><head>\r\n" out)
  (let1 status-line (assq-ref *STATUS-TABLE* status)
    (format out "<title>~a</title>\r\n" status-line)
    (format out "</head><body>\r\n<h1>~a</h1>\r\n<p>~a</p>\r\n</body></html>\r\n"
	    status-line (html-escape-string msg))))

(define (kahua-tmpbase)
  (build-path (ref (kahua-config) 'working-directory)
	      "tmp" "kahua-"))

;; Method dispatching.
(define-values (do-get
		do-head)
  (let1 %do-get (lambda (out uri ver with-body?)
		  (or (and-let* ((path (static-document-path uri (kahua-static-document-url))))
			(process-static-document out uri path ver #t))
		      (reply-not-found out uri ver #t)))
    (values (lambda (method in out uri ver header cont) (%do-get out uri ver #t))
	    (lambda (method in out uri ver header cont) (%do-get out uri ver #f)))))
;; Unknown method handler
(define (do-unknown method in out uri ver header cont)
  (reply-not-implemented out method uri ver #t))
(define do-post do-unknown)
(define do-put do-unknown)
(define do-delete do-unknown)
(define do-options do-unknown)
(define do-trace do-unknown)
(define do-connect do-unknown)
(define-constant *METHOD-TABLE*
  `((GET     . ,do-get)
    (HEAD    . ,do-head)
    (POST    . ,do-post)
    (PUT     . ,do-put)
    (DELETE  . ,do-delete)
    (OPTIONS . ,do-options)
    (TRACE   . ,do-trace)
    (CONNECT . ,do-connect)))
(define (process-method method . args)
  (let* ((method (if (symbol? method)
		     method
		     (string->symbol method)))
	 (proc (assq-ref *METHOD-TABLE* method do-unknown)))
    (apply proc method args)))

;; MIME Type.
(define-constant *MIME-TYPES* '(("jpg"  . "image/jpeg")
				("jpeg" . "image/jpeg")
				("png"  . "image/png")
				("gif"  . "image/gif")
				("pdf"  . "application/pdf")
				("ps"   . "application/postscript")
				("eps"  . "application/postscript")
				("doc"  . "application/msword")
				("xls"  . "application/ms-excel")
				("ppt"  . "application/ms-powerpoint")
				("rtf"  . "application/rtf")
				("swf"  . "application/x-shockwave-flash")
				("html" . "text/html")
				("htm"  . "text/html")
				("css"  . "text/css")
				("js"   . "application/x-javascript")
				("xml"  . "text/xml")
				("txt"  . "text/plain")))
(define (mime-type ext)
  (assoc-ref *MIME-TYPES* ext "application/octet-stream"))

;; HTTP Status code and message.
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

(define (static-document-path path base)
  (static-document-path-info (path->path-info path) base))

(define (static-document-path-info path-info base)
  (and-let* ((result (rel-path-info path-info (path->path-info base))))
    (apply kahua-static-document-path result)))

(define (reply out status version header body-cont)
  (with-port-locking out
    (lambda ()
      (format out "~a ~a\r\n" (or version 'HTTP/1.0) (assq-ref *STATUS-TABLE* status))
      (send-header out header)
      (display "\r\n" out)
      (and body-cont (body-cont out)))))

(define (parse-request-line l)
  (if (eof-object? l)
      (values #f #f #f)
      (match (string-split l #[\s])
	((method uri (? #/HTTP\/1\.[01]/ version))
	 (values (string->symbol method) uri (string->symbol version)))
	(else (values #f #f #f)))))

(define (parse-header in)
  (rfc822-header->list in))

(define (parse-uri uri)
  (cond ((string-null? uri) (values #f #f #f #f "/" #f #f))
	((string=? "*" uri) (values #f #f #f #f #f #f #f))
	(else
	 (receive (scheme user host port path query frag) (uri-parse uri)
	   (values scheme user host port
		   (and path (uri-decode-string path))
		   (and query (uri-decode-string query :cgi-decode #t))
		   (and frag (uri-decode-string frag)))))))

(define (add-kahua-header! header . args)
  (let loop ((h header)
	     (pairs args))
    (if (null? pairs)
	h
	(let ((key (car pairs))
	      (value (cadr pairs)))
	  (if value
	      (loop (assoc-set! h key (list value)) (cddr pairs))
	      (loop h (cddr pairs)))))))

(define (server-software)
  (format "Kahua-HTTPd/~a" (kahua-version)))

(define (basic-header ct)
  `(("date" ,(http-date (current-time)))
    ("server" ,(format "Kahua-HTTPd/~a" (kahua-version)))
    ("content-type" ,ct)))

(define (send-header out header)
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

(define (process-static-document out uri path ver with-body?)
  (if (file-exists? path)
      (cond ((and (file-is-readable? path) (file-is-regular? path))
	     (reply-static-document out path ver with-body?))
	    (else (reply-forbidden out uri ver with-body?)))
      (reply-not-found out uri ver with-body?)))

(define (reply-static-document out path ver with-body?)
  (receive (dir base ext) (decompose-path path)
    (reply out 200 ver (basic-header (mime-type ext))
	   (and with-body?
		(lambda (out)
		  (call-with-input-file path
		    (cut copy-port <> out)))))))

;; 400 Bad Request
(define (reply-bad-request out ver with-body?)
  (reply out 400 ver (basic-header "text/html")
	 (and with-body?
	      (cut default-error-page <> 400
		   "Your browser sent a request that this server could not understand."))))

;; 403 Forbidden
(define (reply-forbidden out uri ver with-body?)
  (reply out 403 ver (basic-header "text/html")
	 (and with-body?
	      (cut default-error-page <> 403
		   (format "You don't have permission to access ~a on this server." uri)))))

;; 404 Not Found
(define (reply-not-found out uri ver with-body?)
  (reply out 404 ver (basic-header "text/html")
	 (and with-body?
	      (cut default-error-page <> 404
		   (format "The requested URL ~a was not found on this server." uri)))))

(define (reply-method-not-allowed out method uri ver with-body?)
  (reply out 405 ver (basic-header "text/html")
	 (and with-body?
	      (cut default-error-page <> 405
		   (format "The requested method ~a is not allowed for the URL ~a." method uri)))))

;; 500 Internal Server Error
(define (reply-internal-server-error out ver with-body?)
  (reply out 500 ver (basic-header "text/html")
	 (and with-body?
	      (cut default-error-page <> 500 "Internal Server Error occured."))))

;; 501 Not Implemented
(define (reply-not-implemented out method uri ver with-body?)
  (reply out 501 ver (basic-header "text/html")
	 (and with-body?
	      (cut default-error-page <> 501
		   (format "~a to ~a not supported." method uri)))))

(define (prepare-dispatch-request cs in)
  (define (http-host->server-name host)
    (and-let* ((m (#/\[?([^\]]+)\]?(?::(\d+))$/ host)))
      (values (m 1))))

  (define (sockaddr->ipaddr sa)
    (http-host->server-name (sockaddr-name sa)))

  (define (kahua-worker-header worker path-info . args)
    (let-keywords* args ((server-uri #f)
			 (metavariables #f)
			 (sgsid #f)
			 (cgsid #f)
			 (remote-addr #f)
			 (bridge #f))
      (reverse!
       (add-kahua-header! '()
			  "x-kahua-worker"        worker
			  "x-kahua-path-info"     path-info
			  "x-kahua-sgsid"         sgsid
			  "x-kahua-cgsid"         cgsid
			  "x-kahua-server-uri"    server-uri
			  "x-kahua-bridge"        bridge
			  "x-kahua-remote-addr"   remote-addr
			  "x-kahua-metavariables" metavariables
			  ))))

  (define (http-header->kahua-metavariables header)
    (define (name-conv name)
      (map-to <string> (lambda (c)
			 (case c
			   ((#\-) #\_)
			   (else (char-upcase c))))
	      name))
    (map (lambda (e)
	   (cons (let1 field-name (car e)
		   (cond ((string=? field-name "content-type")   "CONTENT_TYPE")
			 ((string=? field-name "content-length") "CONTENT_LENGTH")
			 (else (string-append "HTTP_" (name-conv field-name)))))
		 (cdr e)))
	 header))

  (define (kahua-metavariables http-header . args)
    (let-keywords* args ((gateway-interface #f)
			 (remote-addr #f)
			 (server-protocol #f)
			 (server-software #f)
			 (server-port #f)
			 (request-method #f)
			 (script-name #f)
			 (server-name #f)
			 (path-info #f)
			 (path-translated #f)
			 (query-string #f))
      (append! (reverse!
		(add-kahua-header! '()
				   "GATEWAY_INTERFACE" gateway-interface
				   "REMOTE_ADDR"       remote-addr
				   "SERVER_PROTOCOL"   server-protocol
				   "SERVER_SOFTWARE"   server-software
				   "SERVER_PORT"       server-port
				   "REQUEST_METHOD"    request-method
				   "SCRIPT_NAME"       script-name
				   "SERVER_NAME"       server-name
				   "PATH_INFO"         path-info
				   "PATH_TRANSLATED"   path-translated
				   "QUERY_STRING"      query-string))
	       (http-header->kahua-metavariables http-header))))

  (let*-values (((line) (read-line in))
		((method request-uri version) (parse-request-line line)))
    (unless method (raise (make-condition <http-bad-request> 'request line)))
    (let*-values (((scheme user host port path query frag) (parse-uri request-uri))
		  ((http-header) (parse-header in))
		  ((path-info) (path->path-info path))
		  ((abs-path) (path-info->abs-path path-info))
		  ((path-translated) (static-document-path-info path-info (kahua-static-document-url))))
      (if path-translated
	  (values method request-uri version path-translated #f #f #f)
	  (let* ((local-port (x->string (sockaddr-port (socket-getsockname cs))))
		 (remote-ipaddr (sockaddr->ipaddr (socket-getpeername cs)))
		 (http-host (rfc822-header-ref http-header "host"))
		 (server-uri #`",(or scheme \"http\")://,|http-host|")
		 (script-name "") ; DUMMY
		 (metavars (kahua-metavariables http-header
						:gateway-interface *GATEWAY-INTERFACE*
						:remote-addr remote-ipaddr
						:server-protocol (and version (symbol->string version))
						:server-software (server-software)
						:server-port local-port
						:request-method (and method (symbol->string method))
						:script-name script-name
						:server-name (or host (http-host->server-name http-host))
						:path-info abs-path
						:path-translated path-translated
						:query-string query))
		 (params (parameterize ((cgi-metavariables metavars))
			   (with-input-from-port in
			     (lambda ()
			       (cgi-parse-parameters :merge-cookies #t
						     :part-handlers `((#t file+name ,(kahua-tmpbase))))))))
		 (state-gsid (cgi-get-parameter "x-kahua-sgsid" params))
		 (cont-gsid (or (cgi-get-parameter "x-kahua-cgsid" params)
				(if (and path-info (pair? (cdr path-info)))
				    (cadr path-info)
				    #f)))
		 (worker-id (and cont-gsid (gsid->worker-id cont-gsid)))
		 (worker-sockaddr (worker-id->sockaddr worker-id (kahua-sockbase)))
		 (kahua-header (kahua-worker-header
				(car path-info) path-info
				:server-uri server-uri
				:metavariables metavars
				:remote-addr remote-ipaddr
				:bridge script-name
				:sgsid state-gsid :cgsid cont-gsid)))
	    (for-each (lambda (f) (and (file-exists? f) (sys-chmod f #o660))) (cgi-temporary-files))
	    (values method request-uri version path-translated worker-sockaddr kahua-header params))))))

(define (check-kahua-status kheader kbody)
  (or (and-let* ((kahua-status (assoc-ref-car kheader "x-kahua-status")))
	(case (string->symbol kahua-status)
	  ((OK)         #t)
	  ((SPVR-ERROR) (raise (make-condition <kahua-worker-not-found> 'uri (car kbody))))
	  (else         (raise (make-condition <kahua-worker-error>)))))
      #t))

;; Return HTTP status, encoding of body HTTP header.
(define (interp-kahua-header kheader)
  (define (abs-uri uri base)
    (receive (scheme spec) (uri-scheme&specific uri)
      (if scheme
	  uri
	  (string-append base spec))))
  (define (kahua-header->http-header kheader)
    (filter-map (lambda (e)
		  (rxmatch-case (car e)
		    (#/^x-kahua-sgsid$/ (#f)
		     (cons "set-cookie" (construct-cookie-string
					 `(("x-kahua-sgsid" ,(cadr e) :path "/")))))
		    (#/^x-kahua-/ (#f) #f)
		    (#/(?i:^location$)/ (h)
		     (list h (abs-uri (cadr e) (assoc-ref-car kheader "x-kahua-server-uri"))))
		    (else e)))
		kheader))
  (let* ((header (kahua-header->http-header kheader))
	 (content-type (or (assoc-ref-car header "content-type") *default-content-type*))
	 (encoding (and-let* ((m (#/\; *charset=([\w\-]+)/ content-type))) (m 1))))
    (values (or (and-let* ((status (ref-car string-ci=? kheader "status"))
			   (m (#/^\d{3}/ status)))
		  (string->number (m 0)))
		200)
	    encoding
	    (fold (lambda (e r)
		    (assoc-set! r (car e) (cadr e)))
		  header
		   (reverse (basic-header content-type))))))

(define (serve-static-document out method uri ver static-path)
  (case method
    ((GET)  (process-static-document out uri static-path ver #t))
    ((HEAD) (process-static-document out uri static-path ver #f))
    (else   (reply-method-not-allowed out method uri ver #t))))

(define (send-http-body out encoding body)
  (unless (null? body)
    (case (car body)
      ((file) (call-with-input-file (cadr body) (cut copy-port <> out)))
      (else
       (if (ces-equivalent? encoding (gauche-character-encoding))
	   (write-tree body out)
	   (with-output-conversion out (cut write-tree body) :encoding encoding))))))

(define (serve-via-worker out method uri ver worker-sa header params)
  (call-with-client-socket (make-client-socket worker-sa)
    (lambda (w-in w-out)
      (log-format "C->W header: ~s" header)
      (log-format "C->W params: ~s" params)
      (write header w-out)
      (write params w-out)
      (flush w-out)
      (let* ((w-header (read w-in))
	     (w-body   (read w-in)))
	(log-format "C<-W header: ~s" w-header)
	(check-kahua-status w-header w-body)
	(receive (status encoding http-header) (interp-kahua-header w-header)
	  (reply out status ver http-header (cut send-http-body <> encoding w-body))))
      )))

(define (handle-request cs)
  (call-with-client-socket cs
    (lambda (in out)
      (guard (e ((<http-bad-request> e) (reply-bad-request out 'HTTP/1.0 #t))
		((<kahua-worker-not-found> e) (reply-not-found out (uri-of e) #f #t))
		((<kahua-worker-error> e) (reply-internal-server-error out #f #t))
		(else (format (current-error-port) "Error: ~a\n" (ref e 'message))))
	(log-format "Request start: ~s" cs)
	(receive (method uri ver static-path worker-sockaddr header params) (prepare-dispatch-request cs in)
	  (if static-path
	      (serve-static-document out method uri ver static-path)
	      (serve-via-worker out method uri ver worker-sockaddr header params)))
	(log-format "Request finish: ~s" cs))
      (flush out)))
  (for-each sys-unlink (cgi-temporary-files)))

(define (run-server tpool socks)
  (let1 selector (make <selector>)
    (for-each (lambda (ss)
		(log-format "~s ready" ss)
		(selector-add! selector (socket-fd ss)
			       (lambda (fd flag)
				 (let1 sock (socket-accept ss)
				   (add tpool (cut handle-request sock))))
			       '(r)))
	      socks)
    (do () (#f)
      (sys-sigmask SIG_SETMASK *default-sigmask*)
      (selector-select selector 10e6))
    ))

(define (parse-host-spec host port)
  (call-with-values
    (lambda ()
      (if host
	  (rxmatch-case host
	    (#/^(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})(?::(\d+))?$/ ; IPv4 address
		(#f h p) (values h p))
	    (#/^\[([0-9A-Fa-f:]+)\](?::(\d+))?$/                  ; IPv6 address w/ port
		(#f h p) (values h p))
	    (#/^[0-9A-Fa-f:]+$/		                          ; IPv6 address w/o port
		(h) (values h #f))
	    (#/^([-a-zA-Z0-9.]+)(?::(\d+))?$/                     ; Host name
		(#f h p) (values h p))
	    (#/^$/ (#f) (values #f #f))	                          ; Empty
	    (else (error "Host spec must be hostname:portnumber or ipaddress:portnumber but got" host)))
	  (values #f #f)))
    (lambda (h p)
      (cons h (if p (x->number p) port)))))

(define (kahua-make-server-sockets hosts)
  (define (%avoid-mapped-addr sock addr)
    (when (eq? (sockaddr-family addr) 'inet6)
      (socket-setsockopt sock |IPPROTO_IPV6| |IPV6_V6ONLY| 1)))
  (apply append! (map (lambda (h)
			(make-server-sockets (car h) (cdr h)
					     :reuse-addr? #t :backlog SOMAXCONN
					     :sock-init %avoid-mapped-addr))
		      hosts)))

(define (main args)
  (define (usage)
    (display "Usage: kahua-httpd [options ...]
Options:
      --user=user         User-custom setting
      --runas=user:group  Run under the specified privilege
  -p, --port=number       Alternative port number to listen
  -c, --conf-file=file    Alternative location of kahua.conf
  -l, --logfile=file      Alternative log file (default for stderr)
  -t, --threads=number    Number of pooling threads
" (current-error-port))
    (exit 1))
  (let-args (cdr args)
      ((user      "user=s")
       (gosh      "gosh=s")		; DUMMY, not used.
       (runas     "runas=s")
       (conf-file "c|conf-file=s")
       (logfile   "l|logfile=s" #t)
       (port      "p|port=i" 80)
       (thnum     "t:threads=i" 10)
       (help      "h|help" => usage)
       (else _ (error "Unknown option.  Try --help for the usage."))
       . hosts)
    (kahua-init conf-file :user user)
    (log-open logfile :prefix log-prefix)
    (log-format "Start with ~d threads" thnum)
    (for-each (pa$ log-format  "listen: ~s") hosts)
    (let* ((tpool (make-thread-pool thnum))
	   (hosts (map (cut parse-host-spec <> port) (if (null? hosts) '(#f) hosts)))
	   (socks (kahua-make-server-sockets hosts)))
      (setuidgid! runas)
      (let1 ret
	  (call/cc
	   (lambda (bye)
	     (set! *default-sigmask* (sys-sigmask 0 #f))
	     (set-signal-handler! *TERMINATION-SIGNALS* (lambda _ (bye 0)))
	     (set-signal-handler! SIGPIPE #f)
	     (guard (e (else (bye e)))
	       (run-server tpool socks))
	     (bye 0)))
	(for-each socket-close socks)
	(wait-all tpool)
	(log-format "Finish")
	(when (condition? ret) (raise ret))
	ret)
      )))
