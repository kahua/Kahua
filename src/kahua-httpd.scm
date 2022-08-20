;;; -*- mode: scheme; coding: utf-8 -*-
;; HTTPd for kahua
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(use srfi-1)
(use srfi-11)
(use srfi-13)
(use rfc.822)
(use rfc.uri)
(use rfc.cookie)
(use rfc.mime)
(use text.tree)
(use www.cgi)
(use util.list)
(use util.match)
(use file.util)
(use control.thread-pool)
(use gauche.net)
(use gauche.selector)
(use gauche.parseopt)
(use gauche.collection)
(use gauche.parameter)
(use gauche.charconv)
(use kahua.gsid)
(use kahua.util)
(use kahua.config)
(use kahua.protocol.http)
(use kahua.protocol.worker)

(define *default-sigmask* #f)
(define-constant *TERMINATION-SIGNALS*
  (sys-sigset-add! (make <sys-sigset>) SIGTERM SIGINT SIGHUP))

(define (log-prefix drain)
  (format "[~s] " (current-thread)))

(define request-line (make-parameter #f))
(define request-method (make-parameter #f))
(define request-uri (make-parameter #f))
(define request-version (make-parameter #f))

(define (with-body?)
  (not (eq? (request-method) 'HEAD)))

(define-constant *INDEX* '("index.html"))

(define-constant *GATEWAY-INTERFACE* "CGI/1.1")

(define-constant *DEFAULT-ENCODING*
  (case (gauche-character-encoding)
    ((utf-8)  'UTF-8)
    ((euc-jp) 'EUC-JP)
    ((sjis)   'Shift_JIS)
    (else     #f)))

(define-condition-type <kahua-http-error> <kahua-error> http-error?)

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
                                ("xml"  . "application/xml")
                                ("txt"  . "text/plain")))
(define (mime-type ext)
  (assoc-ref *MIME-TYPES* ext "application/octet-stream"))

(define (static-document-path path base)
  (static-document-path-info (path->path-info path) base))

(define (static-document-path-info path-info base)
  (and-let* ((result (rel-path-info path-info (path->path-info base))))
    (apply kahua-static-document-path result)))

(define (reply out status version header body-cont)
  (with-port-locking out
    (lambda ()
      (print-status-line out status version)
      (send-http-header out header)
      (display "\r\n" out)
      (when body-cont (body-cont out)))))

(define (parse-request-line l)
  (if (eof-object? l)
      (values #f #f #f)
      (match (string-split l #[\s])
        ((method uri (? #/HTTP\/1\.[01]/ version))
         (values (string->symbol method) uri (string->symbol version)))
        (else (values #f #f #f)))))

(define http-header-parse rfc822-header->list)
(define http-header-ref rfc822-header-ref)

(define (parse-uri uri)
  (cond ((string-null? uri) (values #f #f #f #f "/" #f #f))
        ((string=? "*" uri) (values #f #f #f #f #f #f #f))
        (else
         (receive (scheme user host port path query frag) (uri-parse uri)
           (values scheme user host port
                   (and path (uri-decode-string path)) ; for compatibility with Apache HTTPd
                   query	                       ; decode in cgi-parse-parameters
                   (and frag (uri-decode-string frag)))))))

(define (server-software)
  (format "Kahua-HTTPd/~a" (kahua-version)))

(define (basic-header ct . args)
  (let-keywords* args ((content-length #f))
    (cond-list (#t `("date" ,(http-date (current-time))))
               (#t `("server" ,(format "Kahua-HTTPd/~a" (kahua-version))))
               (#t `("content-type" ,ct))
               (content-length `("content-length" ,content-length))
               (#t '("connection" "close")))))		; Now not support keep-alive connection yet.

(define (serve-static-document out path)
  (define (reply-static-document out path)
    (let*-values (((dir base ext) (decompose-path path))
                  ((size) (ref (sys-stat path) 'size)))
      (reply out 200 (request-version) (basic-header (mime-type ext) :content-length size)
             (and (with-body?)
                  (lambda (out)
                    (call-with-input-file path
                      (cut copy-port <> out)))))))
  (define (directory->index-file path)
    (and (file-is-directory? path)
         (any (lambda (idx)
                (let1 f #`",|path|/,|idx|"
                  (and (file-exists? f) f)))
                     *INDEX*)))
  (cond ((file-exists? path)
         (let1 path (or (directory->index-file path) path)
           (or (and (file-is-regular? path)
                    (guard (e (else #f))
                      (reply-static-document out path)))
               (raise (make-condition <http-forbidden>)))))
        (else
         (kahua:log-format "Not found file: ~s" path)
         (raise (make-condition <http-not-found>)))))

;; 400 Bad Request
(define (reply-bad-request out ver with-body?)
  (reply out 400 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 400
                   "Your browser sent a request that this server could not understand."))))
(define-condition-type <http-bad-request> <kahua-http-error> #f)
(define-method reply-error ((http-error <http-bad-request>) out)
  (reply-bad-request out #f #t))

;; 403 Forbidden
(define (reply-forbidden out uri ver with-body?)
  (reply out 403 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 403
                   (format "You don't have permission to access ~a on this server." uri)))))
(define-condition-type <http-forbidden> <kahua-http-error> #f)
(define-method reply-error ((e <http-forbidden>) out)
  (reply-forbidden out (request-uri) (request-version) (with-body?)))

;; 404 Not Found
(define (reply-not-found out uri ver with-body?)
  (reply out 404 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 404
                   (format "The requested URL ~a was not found on this server." uri)))))
(define-condition-type <http-not-found> <kahua-http-error> #f)
(define-method reply-error ((e <http-not-found>) out)
  (reply-not-found out (request-uri) (request-version) (with-body?)))
(define-method reply-error ((e <kahua-worker-not-found>) out)
  (reply-not-found out (request-uri) (request-version) (with-body?)))

;; 405 Method Not Allowed
(define (reply-method-not-allowed out method uri ver with-body?)
  (reply out 405 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 405
                   (format "The requested method ~a is not allowed for the URL ~a." method uri)))))
(define-condition-type <http-method-not-allowed> <kahua-http-error> #f)
(define-method reply-error ((e <http-method-not-allowed>) out)
  (reply-method-not-allowed out (request-method) (request-uri) (request-version) (with-body?)))

;; 410 Gone
(define (reply-gone out method uri ver with-body?)
  (reply out 410 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 410
                   (format "Session key expired of ~a." uri) ))))
(define-method reply-error ((e <kahua-spvr-session-expired>) out)
  (reply-gone out (request-method) (request-uri) (request-version) (with-body?)))

;; 500 Internal Server Error
(define (reply-internal-server-error out ver with-body?)
  (reply out 500 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 500 "Internal Server Error occured."))))
(define-method reply-error ((e <kahua-error>) out)
  (reply-internal-server-error out (request-version) (with-body?)))

;; 501 Not Implemented
(define (reply-not-implemented out method uri ver with-body?)
  (reply out 501 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 501
                   (format "~a to ~a not supported." method uri)))))
(define-condition-type <http-not-implemented> <kahua-http-error> #f)
(define-method reply-error ((e <http-not-implemented>) out)
  (reply-not-implemented out (request-method) (request-uri) (request-version) #t))

;; 503 Service Unavailable
(define (reply-service-unavailable out ver with-body?)
  (reply out 503 ver (basic-header "text/html")
         (and with-body?
              (cut output-error-page <> 503 "Service Unavailable."))))
(define-method reply-error ((e <kahua-worker-not-respond>) out)
  (reply-service-unavailable out (request-version) #t))
(define-method reply-error ((e <kahua-spvr-not-respond>) out)
  (reply-service-unavailable out (request-wersion) #t))

(define (prepare-dispatch-request cs in)
  (define (http-host->server-name host)
    (and-let* ((m (#/\[?([^\]]+)\]?(?::(\d+))$/ host)))
      (values (m 1))))

  (define (path-info->worker-name path-info)
    (cond ((not path-info) #f)
          ((null? path-info) "")
          (else (car path-info))))

  (define (path-info->cont-gsid path-info)
    (if (> (length path-info) 1)
        (cadr path-info)
        #f))

  (define (sockaddr->ipaddr sa)
    (http-host->server-name (sockaddr-name sa)))

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

  (let*-values (((method request-uri version) (values (request-method) (request-uri) (request-version)))
                ((scheme user host port path query frag) (parse-uri request-uri))
                ((http-header) (http-header-parse in))
                ((path-info) (path->path-info path))
                ((abs-path) (path-info->abs-path path-info))
                ((path-translated) (static-document-path-info path-info (kahua-static-document-url))))
    (if path-translated
        (values path-translated #f #f #f)
        (let* ((local-port (x->string (sockaddr-port (socket-getsockname cs))))
               (remote-ipaddr (sockaddr->ipaddr (socket-getpeername cs)))
               (client-ipaddr (or (and-let* ((xff (http-header-ref http-header "x-forwarded-for"))
                                             (hops (string-split xff #/\, */)))
                                    (and (pair? hops) (car hops)))
                                  remote-ipaddr))
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
                              (path-info->cont-gsid path-info)))
               (kahua-header (kahua-worker-header
                              (path-info->worker-name path-info) path-info
                              :server-uri server-uri
                              :metavariables metavars
                              :remote-addr client-ipaddr
                              :bridge script-name
                              :sgsid state-gsid :cgsid cont-gsid)))
          (for-each (lambda (f) (and (file-exists? f) (sys-chmod f #o660))) (cgi-temporary-files))
          (values path-translated cont-gsid kahua-header params)))))

;; Return HTTP status, encoding of body HTTP header.
(define (interp-kahua-header kheader)
  (let* ((header (remove! (lambda (e) (string-ci=? (car e) "status"))
                          (kahua-header->http-header kheader)))
         (content-type (or (ref-car string-ci=? header "content-type") *default-content-type*))
         (encoding (and-let* ((m (#/\; *charset=([\w\-]+)/ content-type))) (m 1))))
    (values (or (and-let* ((status (ref-car string-ci=? kheader "status"))
                           (m (#/^\d\d\d/ status)))
                  (string->number (m 0)))
                200)
            encoding
            (fold (lambda (e r)
                    (assoc-set! r (car e) (cdr e)))
                  header
                   (reverse (basic-header content-type))))))

(define (serve-via-worker out cgsid header params)
  (let*-values (((w-header w-body) (talk-to-worker cgsid header params))
                ((status encoding http-header) (interp-kahua-header w-header)))
    (reply out status (request-version) http-header
           (and (with-body?) (cut send-http-body <> encoding w-body)))))

(define (handle-request cs)
  (guard (e (else (kahua:log-format "Request aborted: ~s" cs)))
    (call-with-client-socket cs
      (lambda (in out)
        (guard (e ((kahua-error? e)
                   (kahua:log-format "Error: ~s" e)
                   (reply-error e out))
                  (else
                   (kahua:log-format "Unexpected error: ~s" (kahua-error-string e))
                   (reply-internal-server-error out #f #t)))
          (kahua:log-format "Request start: ~s" cs)
          (let*-values (((l) (read-line in))
                        ((m u v) (parse-request-line l)))
            (request-line l)
            (request-method m)
            (request-uri u)
            (request-version v)
            (cond ((not m) (raise (make-condition <http-bad-request>)))
                  ((unsupported-method? m) (raise (make-condition <http-not-implemented>)))
                  (else
                   (receive (static-path cgsid header params)
                       (prepare-dispatch-request cs in)
                     (if static-path
                         (serve-static-document out static-path)
                         (serve-via-worker out cgsid header params)))))))
        (flush out)))
    (kahua:log-format "Request finished: ~s" cs))
  (for-each sys-unlink (cgi-temporary-files)))

(define (run-server tpool socks)
  (let1 selector (make <selector>)
    (for-each (lambda (ss)
                (kahua:log-format "~s ready" ss)
                (selector-add! selector (socket-fd ss)
                               (lambda (fd flag)
                                 (let1 sock (socket-accept ss)
                                   (add-job! tpool (cut handle-request sock))))
                               '(r)))
              socks)
    (selector-add! selector (current-input-port)
                   (lambda (in flag)
                     (when (eof-object? (read-byte in))
                       (display "httpd: parent(maybe spvr) has gone, so me too!!\n" (current-error-port))
                       (selector-delete! selector in #f #f)
                       (sys-kill (sys-getpid) SIGTERM)))
                   '(r))
    (do () (#f) (selector-select selector 10e6))))

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
  ;; Avoid to use IPv4 mapped address.
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
      --runas=user:group  Run under the specified privilege
  -p, --port=number       Alternative port number to listen
  -c, --conf-file=file    Alternative location of kahua.conf
  -l, --logfile=file      Alternative log file (default for stderr)
  -t, --threads=number    Number of pooling threads
" (current-error-port))
    (exit 1))
  (let-args (cdr args)
      ((gosh      "gosh=s")		; DUMMY, not used.
       (runas     "runas=s")
       (site      "S|site=s")
       (conf-file "c|conf-file=s")
       (logfile   "l|logfile=s" #t)
       (port      "p|port=i" 80)
       (thnum     "t:threads=i" #f)
       (help      "h|help" => usage)
       (else _ (error "Unknown option.  Try --help for the usage."))
       . hosts)
    (kahua-common-init site conf-file)
    (kahua:log-open logfile :prefix log-prefix)
    (kahua:log-format "Start with ~d threads" thnum)
    (for-each (pa$ kahua:log-format  "listen: ~s") hosts)
    (let* ((tpool (make-thread-pool (or thnum (kahua-httpd-concurrency))))
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
        (kahua:log-format "Finish")
        (when (condition? ret) (raise ret))
        ret)
      )))
