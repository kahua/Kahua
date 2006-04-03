;; "Supervisor" or super server for kahua
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-spvr.scm,v 1.14 2006/04/03 05:13:54 bizenn Exp $

;; For clients, this server works as a receptionist of kahua system.
;; It opens a socket where initial clients will connect.
;; This server doesn't know about internals of kahua server, but
;; just dispatches it to the worker servers.
;; Eventually, this server will manage multiple worker servers for
;; load balancing or hot restarting.
;; For now, we have only one worker server, so this is just an outline
;; of what we will ultimately do.

(use gauche.charconv)
(use gauche.net)
(use gauche.threads)
(use gauche.process)
(use gauche.logger)
(use gauche.selector)
(use gauche.listener)
(use gauche.parseopt)
(use gauche.parameter)
(use gauche.mop.singleton)
(use srfi-1)
(use srfi-2)
(use srfi-11)
(use srfi-13)
(use rfc.822)
(use rfc.uri)
(use rfc.cookie)
(use rfc.mime)
(use file.util)
(use text.tree)
(use text.html-lite)
(use util.queue)
(use util.list)
(use util.match)
(use www.cgi)
(use kahua.config)
(use kahua.gsid)
(use kahua.developer)
(use kahua.util)

;; Eventually this should be configurable by some conf file
(define *default-worker-type* 'dummy)

(define *spvr* #f) ;; bound to supervisor object for convenience

(define *default-sigmask* #f)

(define *worker-types* '()) ;; for multi-thread version

(define-constant *TERMINATION-SIGNALS* (sys-sigset-add! (make <sys-sigset>) SIGTERM SIGINT SIGHUP))

;; Supervisor protocol
;;
;; [Session initiation]
;;
;;  A client first connect to a well known socket of the supervisor.
;;  This first request is called session-initiaing request.
;;  At this point, a client wouldn't have complete GSID.  It may have
;;  state ID from the last session, but it certainly doesn't have
;;  continuation ID.
;;
;;  When the supervisor observes it, and the request is not for direct
;;  administrative request for the supervisor itself, it assigns a
;;  worker and forwards the session-initiating request to the worker.
;;  The worker will return a reply, usually accompanied by GSID.
;;  The supervisor forwards the reply to the client.
;;
;;  Afterwards, the client can figure out the worker ID encoded in GSID,
;;  and directly connects to the worker.
;;
;;  Client can optionally pass the worker type it wants to talk to,
;;  using "x-kahua-worker" header.   If such header is absent, the
;;  supervisor selects the default worker.
;;
;;  As a special case, "spvr" is given to "x-kahua-worker", the body
;;  is interpreted by the spvr process as a command.  See "supervisor
;;  commands" below.
;;
;; [Message format]
;;
;;  Request and reply both consist of two S-expressions, a header and
;;  a body.  A header is a list of two-element lists, resembles to
;;  what rfc822-header->list returns.   A body can be any valid sexpr.
;;  It is arguable whether this format is adequat or not.  Let's see.
;;
;;  In a request header, "x-kahua-sgsid" elemnt carries state GSID,
;;  and "x-kahua-cgsid" carries continuation GSID.  Other header can
;;  be freely used by the worker.  A client may send additional information
;;  in the header, and the worker should ignore the header element that
;;  it doesn't understand.
;;
;;  In a reply header, "x-kahua-sgsid" and "x-kahua-cgsid" are also
;;  used to carry GSID.  It also contains "x-kahua-status", whose value
;;  is either "OK", "ERROR", or "SPVR-ERROR".   "ERROR" indicates
;;  an error occurred in the worker, and "SPVR-ERROR" indicates an
;;  error occurred in the supervisor.  The message body of error replies
;;  contains a list of a error message string (for now).
;;

;;;=================================================================
;;; Global structure
;;;

(define-class <kahua-spvr> ()
  ((sockbase   :init-form (kahua-sockbase))
   (workers    :init-form (make-queue) :getter workers-of)
   (selector   :init-form (make <selector>) :getter selector-of)
   (keyserv    :init-value #f) ; keyserver process
   (gosh-path  :init-keyword :gosh-path) ; Absolute path of gosh, passed
                                         ; by wrapper script.
   (lib-path   :init-keyword :lib-path)  ; Path where kahua library files
                                         ; are installed.
   (httpd-name :init-keyword :httpd-name ; when used as a standalone httpd,
               :init-value #f)           ; this holds "server:port"
   ))

(define-class <kahua-worker> ()
  ((spvr   :init-keyword :spvr)               ;; back ptr to spvr
   (worker-type :init-keyword :worker-type
                :getter worker-type-of)       ;; worker type (symbol)
   (worker-id :getter worker-id-of)           ;; worker id (string)
   (worker-count :getter worker-count-of)     ;; an integer count for worker
   (worker-process :getter worker-process-of) ;; worker process
   (start-time :getter start-time-of          ;; timestamp
               :init-form (sys-time))
   (worker-zombee :getter zombee?
		  :init-form #f)

   (ping-last-time  :getter ping-last-time-of
	       :init-form (sys-time))
   (ping-deactivator :getter ping-deactivator-of
		     :init-form (lambda () (error (e) "not initialized")))
   (pinger :getter pinger-of
	   :init-form (lambda () (error (e) "not initialized")))
   (ping-responded :getter ping-responded?
		   :init-form #f)

   ;; internal
   (next-worker-count :allocation :class :init-value 0)
   ))

(define-class <kahua-keyserv> ()
  ((process :init-keyword :process) ;; <process>
   (id      :init-keyword :id)      ;; keyserver id
   ))

;; worker type entry - will be overridden by configuration file
; (define worker-types
;   (make-parameter
;    '()
;    ))

;;;=================================================================
;;; Error handling
;;;

;; Spvr should handle all "expected" exceptional cases gracefully,
;; which is indicated by throwing <spvr-exception> object.
;; If an object other than <spvr-exception> is thrown, it should be
;; a program bug.

(define-class <spvr-exception> (<error>) ())

;; A convenience function to raise <spvr-exception> or its subclasses
(define (spvr-errorf class fmt . args)
  (raise (make class :message (apply format fmt args))))

;; This exception occurs when the URI given from the client doesn't
;; correspond to any known worker type.    "404 Not found" may be
;; an appropriate response to the http client.
(define-class <spvr-unknown-worker-type> (<spvr-exception>) ())

;; This exception occurs when the URI given form the client
;; specifies a worker that is known, but is not running.
;; "503 Service unavailable" may be an appropriate response
;; to the http client.
(define-class <spvr-worker-not-running> (<spvr-exception>) ())

;; This exception occurs when the given session id is invalid
;; or may be expired.  The httpd can return "200 OK" with
;; an appropriate message.
(define-class <spvr-expired-session> (<spvr-exception>) ())

;; If errors occur before spvr service starts, we should terminate
;; spvr with appropriate error message.
(define (app-error msg . args)
  (apply format #t msg args)
  (newline)
  (exit 1))

;;;=================================================================
;;; Miscellaneous utilities
;;;

(define (send-message out header body)
  (write header out) (newline out)
  (write body out)   (newline out)
  (flush out)
  )

(define (receive-message in)
  (let* ((header (read in))
         (body   (read in)))
    (values header body)))

(define (log-worker-action action worker)
  (log-format "[work] ~A: ~A(~A - ~A)" action 
	      (worker-type-of worker) (worker-count-of worker)
	      (worker-id-of worker)))

(define (get-worker-type header)
  (cond ((assoc "x-kahua-worker" header)
         => (lambda (p) (string->symbol (cadr p))))
        (else #f)))

;; returns a list suitable to pass run-process.  option-list
;; is appended first.
(define (script-command spvr script-name option-list)
  (list* (ref spvr 'gosh-path) "-I" (ref spvr 'lib-path)
         (build-path (ref spvr 'lib-path) script-name)
         (apply append option-list)))

(define (run-piped-cmd cmd)
  (log-format "[spvr] running ~a" cmd)
  (guard (e (else 
	     (log-format "[spvr] running ~a failed: ~a"
			 (car cmd) (kahua-error-string e #t))
	     (raise e)))
    (let1 p (apply run-process `(,@cmd :input "/dev/null" :output :pipe :sigmask ,*default-sigmask*))
      (log-format "[spvr] running ~a: pid ~a" (car cmd) (process-pid p))
      p)))

;; canonicalize string passed to --httpd option
(define (canonicalize-httpd-option value)
  (and value
       (rxmatch-case value
         (#/^\d+$/  (#f)  #`"localhost:,value")
         (#/^[\w._-]+:\d+$/ (#f) value)
         (else (app-error "Bad value for --httpd option: must be a port number or servername:port, but got ~a" value)))))

(define (httpd-port spvr)
  (and-let* ((httpd-name (ref spvr 'httpd-name))
             (m (#/:(\d+)$/ httpd-name)))
    (x->integer (m 1))))

;;;=================================================================
;;; Keyserv management
;;;

(define (start-keyserv spvr)
  (let* ((cmd (script-command spvr "kahua-keyserv.scm"
                              (cond-list
                               ((kahua-config-file)
                                => (cut list "-c" <>))
                               ((ref (kahua-config) 'user-mode)
                                => (cut list "-user" <>)))))
         (kserv (run-piped-cmd cmd))
         (kserv-id (read-line (process-output kserv))))
    (set! (ref spvr 'keyserv)
          (make <kahua-keyserv> :process kserv :id kserv-id))
    (close-input-port (process-output kserv))))

(define (stop-keyserv spvr)
  (when (ref spvr 'keyserv)
    (let1 serv (ref spvr 'keyserv)
      (set! (ref spvr 'keyserv) #f)
      (process-send-signal (ref serv 'process) SIGHUP)
      (process-wait (ref serv 'process)))))

;;;=================================================================
;;; Worker management
;;;

(define (worker-script worker-type spvr)
  (cond ((assq worker-type *worker-types*)
         => (lambda (p)
              (let ((args (get-keyword :arguments (cdr p) '()))
                    (user (ref (kahua-config) 'user-mode)))
                (script-command
                 spvr
                 "kahua-server.scm"
                 (cond-list
                  ((kahua-config-file) => (cut list "-c" <>))
                  ((ref (kahua-config) 'user-mode) => (cut list "-user" <>))
                  ((ref spvr 'keyserv) => (lambda (k) `("-k" ,(ref k 'id))))
                  (#t (cons (let1 type (symbol->string worker-type)
                              (string-append type "/" type ".kahua"))
                            args)))))))
        (else (spvr-errorf <spvr-unknown-worker-type>
                          "unknown worker type: ~a" worker-type))))

(define (load-app-servers-file)
  (let1 app-map
      (build-path (ref (instance-of <kahua-config>) 'working-directory)
                  "app-servers")
    (define (check-entries lis) ;; check vailidy of app-servers entries
      (and (list? lis)
           (every (lambda (ent)
                    (and (list? ent)
                         (symbol? (car ent))
                         (odd? (length ent))))
                  lis)))
    (cond
     ((file-exists? app-map)
      (guard (e (else
                 (log-format "[spvr] error in reading ~a" app-map)
                 #f))
        (let1 lis (call-with-input-file app-map read)
          (cond ((check-entries lis)
                 (log-format "[spvr] loaded ~a" app-map)
                 (set! *worker-types* lis)
                 #t)
                (else
                 (log-format "[spvr] malformed app-servers file: ~a" app-map)
                 #f)))))
     (else
      (log-format "app-servers file does not exist: ~a" app-map)
      #f))))

;; start workers that are specified as "run by default"
(define (run-default-workers spvr)
  (map (lambda (w)
         (let1 wtype (car w)
           (dotimes (n  (- (get-keyword :run-by-default (cdr w) 0)
                           (length (find-workers spvr wtype))))
             (run-worker spvr wtype))
           wtype))
       *worker-types*))

;; start worker specified by worker-class
(define-method run-worker ((self <kahua-spvr>) worker-type)
  (let1 worker (make <kahua-worker> :spvr self :worker-type worker-type)
    (log-worker-action "run" worker)
    (enqueue! (workers-of self) worker)
    (ping-activate self worker)
    worker))

;; returns a list of workers
(define-method list-workers ((self <kahua-spvr>))
  (queue->list (workers-of self)))

;; collect exit status of workers that has exit.
(define-method check-workers ((self <kahua-spvr>))
  ;; respond check with ping
  (let ((workers (list-workers self)))
    (restart-workers 
     self
     (filter (lambda (w) (ping-timeout? w))
	     workers)))
  
  ;; collect finish processes
  (and-let* ((wq (workers-of self))
             ((not (null? wq)))
	     (p (process-wait-any #t))
             (w (find (lambda (w) (eq? (worker-process-of w) p))
                      (queue->list wq))))
    (remove-from-queue! (cut eq? w <>) wq)
    (if (and (kahua-auto-restart)
	     (not (zombee? w))
	     (> (- (sys-time) (start-time-of w)) 60))
	;; unexpected terminated process
	(begin
	  (log-worker-action "restart unexpected terminated worker" w)
	  (restart-workers self (list w)))
	;;
	(begin
	  (log-worker-action "collect finished worker" w)
	  (finish-worker w)))
    w))

;; terminate all workers
(define-method nuke-all-workers ((self <kahua-spvr>))
  (log-format "[spvr] nuke-all-workers")
  (for-each (cut terminate <>) (list-workers self))
  (do ()
      ((queue-empty? (workers-of self)))
    (check-workers self)))

;; terminates given workers, and starts the same number of
;; the same type workers.  Returns terminated worker id.
(define-method restart-workers ((self <kahua-spvr>) workers)
  (let1 type&ids (map (lambda (w)
                       (let1 type&id (cons (worker-type-of w)
                                           (worker-id-of w))
			 (log-worker-action "restart" w)
                         (terminate w)
			 (check-workers self)
			 type&id))
                      workers)
    (for-each 
     (lambda (t&i) (run-worker self (car t&i))) type&ids)
    (map cdr type&ids)))

(define-method ping-timeout? ((worker <kahua-worker>))
  (if (zombee? worker)
      #f
      (let ((d (- (sys-time) (ping-last-time-of worker))))
	(if (and (not (ping-responded? worker))
		 (> d (kahua-ping-timeout-sec)))
	    (begin
	      (log-worker-action "ping timeout" worker)
	      #t) ; timeout!	    
	    ;; not timeout
	    (begin
	      (if (and (ping-responded? worker)
		       (>= d (kahua-ping-interval-sec)))
		  ;; next ping
		  (ping-to-worker worker))
	      #f)))))

(define-method ping-activate ((spvr <kahua-spvr>) (worker <kahua-worker>))
  (let*
      (
       (sock #f)
       (in   #f)
       (out  #f)	
       (proc (lambda (fd flag)
	       (set! (ref worker 'ping-responded) #t)
	       (set! (ref worker 'ping-last-time) (sys-time))
	       (receive-message fd)
	       (ping-deactivate worker)
	       (log-worker-action "ping respond" worker)
	       ))

       (reset-sock
	(lambda ()
	  ;(worker-type-of worker)
	  (set! sock (make-client-socket
		      (worker-id->sockaddr (worker-id-of worker)
					   (ref spvr 'sockbase))))
	  (set! in   (socket-input-port sock))
	  (set! out  (socket-output-port sock))
	  )))

    (set! (ref worker 'pinger)
	  (lambda ()
	    (guard (e (else #t)) ;; do nothing, collected by check-workers
              (reset-sock)
              (set! (ref worker 'ping-responded) #f)
              (send-message out `(("x-kahua-ping" ,(worker-id-of worker)))
                            '())
	      (proc in #f)
	      )
	    )
	  )
    (set! (ref worker 'ping-deactivator)
	  (lambda () 
	    (and sock (socket-close sock))))
    (ping-to-worker worker)
    ))

(define-method ping-deactivate ((worker <kahua-worker>))
  ((ping-deactivator-of worker)))

(define-method ping-to-worker ((worker <kahua-worker>))
  ; (log-worker-action "ping send" worker)
  ((pinger-of worker)))

;; pick one worker that has worker-id WID.  If WID is #f, pick arbitrary one.
(define-method find-worker ((self <kahua-spvr>) (wid <string>))
  (find-in-queue (lambda (w) (equal? (worker-id-of w) wid))
                 (workers-of self)))

(define-method find-worker ((self <kahua-spvr>) (wtype <symbol>))
  (find-in-queue (lambda (w) (eq? (worker-type-of w) wtype))
                 (workers-of self)))

(define-method find-worker ((self <kahua-spvr>) (wcount <integer>))
  (find-in-queue (lambda (w) (eq? (worker-count-of w) wcount))
                 (workers-of self)))

(define-method find-worker ((self <kahua-spvr>) _)
  ;; Fallback case, where the session-initiating request doesn't specify
  ;; the worker.  For now, we just dispatch the request to the first
  ;; worker in the queue.  Eventually we need some scheduling strategy
  (if (queue-empty? (workers-of self))
    (spvr-errorf <spvr-worker-not-running> "no worker available")
    (queue-front (workers-of self))))

;; returns group of workers
(define-method find-workers ((self <kahua-spvr>) (wtype <symbol>))
  (if (eq? wtype '*)
    (queue->list (workers-of self))
    (filter (lambda (w) (eq? (worker-type-of w) wtype))
            (queue->list (workers-of self)))))

(define-method find-workers ((self <kahua-spvr>) (wid <string>))
  (cond ((find-worker self wid) => list) (else '())))

(define-method find-workers ((self <kahua-spvr>) (wcount <integer>))
  (cond ((find-worker self wcount) => list) (else '())))

;;;=================================================================
;;; <kahua-worker> implementation
;;;

(define-method initialize ((self <kahua-worker>) initargs)
  (next-method)
  (let* ((cmd  (worker-script (worker-type-of self) (ref self 'spvr)))
         (p    (run-piped-cmd cmd))
         (id   (read-line (process-output p)))
         (count (ref self 'next-worker-count)))
    (slot-set! self 'worker-id id)
    (slot-set! self 'worker-count count)
    (slot-set! self 'worker-process p)
    (inc! (ref self 'next-worker-count))
    ))

(define-method terminate ((self <kahua-worker>))
  (if (zombee? self)
      #f
      (begin
	(set! (ref self 'worker-zombee) #t)
	(log-worker-action "terminate" self)
	(ping-deactivate self)
	(process-send-signal (worker-process-of self) SIGTERM))))

;; dummy method to do something when a worker ends unexpected
(define-method unexpected-end ((self <kahua-worker>))
  (log-worker-action "unexpected finish" self))

(define-method finish-worker ((self <kahua-worker>))
  (if (not (zombee? self))
      (unexpected-end self))
  (close-input-port (process-output (worker-process-of self))))

(define-method dispatch-to-worker ((self <kahua-worker>) header body cont)
  (let* ((spvr (ref self 'spvr))
         (sock (make-client-socket
                (worker-id->sockaddr (worker-id-of self)
                                     (ref spvr 'sockbase))))
         (out  (socket-output-port sock))
	 )

    (define (handle fd flags)
      (guard (e (else
                 (socket-close sock)
                 (cont '(("x-kahua-status" "SPVR-ERROR"))
                       (list (ref e 'message) (kahua-error-string e #t)))))
        (receive (header body) (receive-message (socket-input-port sock))
          (socket-close sock)
          (cont header body))))

    (send-message out header body)
    (handle #f #f)
    )
  )

;;;=================================================================
;;; Supervisor commands
;;;

(define (handle-spvr-command body)
  (define (worker-info w)
    (list :worker-id    (worker-id-of w)
          :worker-count (worker-count-of w)
          :worker-type  (worker-type-of w)
          :worker-pid   (process-pid (worker-process-of w))
          :start-time   (start-time-of w)))
  
  (unless (pair? body) (error "bad spvr command:" body))
  (case (car body)
    ((ls)    ;; list active workers
     (map worker-info (list-workers *spvr*)))
    ((run)   ;; start specified worker type
     (map (lambda (type) (worker-info (run-worker *spvr* type))) (cdr body)))
    ((kill)  ;; kill the specified worker or worker(s) of type
     (for-each
      (lambda (type-or-count)
        (cond ((eq? type-or-count '*) 
	       (nuke-all-workers *spvr*))
              ((or (symbol? type-or-count)
                   (string? type-or-count)
                   (integer? type-or-count))
               (let loop ()
                 (let1 w (find-worker *spvr* type-or-count)
                   (when w (terminate w) (check-workers *spvr*) (loop)))))))
      (cdr body))
     (map worker-info (list-workers *spvr*)))
    ((types)  ;; returns list of known worker types
     (map car *worker-types*))
    ((reload) ;; reload app-servers file
     (begin
       (if (load-app-servers-file)
	   (run-default-workers *spvr*)
	   #f)
       ))
    ((restart)
     (fold (lambda (spec lis)
             (if (or (symbol? spec)
                       (string? spec)
                       (integer? spec))
               (append lis
                       (restart-workers *spvr* (find-workers *spvr* spec)))
               lis))
           '()
           (cdr body)))
    ((shutdown) ;; shutting down the server
     (log-format "[spvr] shutdown requested")
     (sys-kill (sys-getpid) SIGTERM))
    ((help)   ;; returns list of commands
     '(ls run kill types reload restart help version shutdown))
    ((version) (kahua-version))
    (else
     (error "unknown spvr command:" body))))

;;;=================================================================
;;; Server Loop
;;;

;;; Common dispatching routine
(define-method handle-common ((self <kahua-spvr>) header body cont)
  (let*-values (((stat-gsid cont-gsid) (get-gsid-from-header header))
                ((stat-h stat-b) (decompose-gsid stat-gsid))
                ((cont-h cont-b) (decompose-gsid cont-gsid))
                ((wtype) (get-worker-type header)))
    (log-format "[spvr] header: ~s" header)
    (cond
     ((equal? wtype 'spvr)
      ;; this is a supervisor command.
      (cont '(("x-kahua-status" "OK")) (handle-spvr-command body)))
     (cont-h
      ;; we know which worker handles the request
      (let ((w (find-worker self cont-h)))
        (unless w
          (spvr-errorf <spvr-expired-session> "Session key expired"))
        (dispatch-to-worker w header body cont)))
     (else
      ;; this is a session-initiating request.  wtype must be symbol.
      (let ((w (find-worker self wtype)))
        (unless w
          (if (assq wtype *worker-types*)
            (spvr-errorf <spvr-worker-not-running>
                         "Application server for ~a is not running currently."
                         wtype)
            (spvr-errorf <spvr-unknown-worker-type> "/~a" wtype)))
        (dispatch-to-worker w header body cont))))
    ))

;;; "Kahua request" handler.  Client is kahua.cgi or kahua-admin.
(define-method handle-kahua ((self <kahua-spvr>) client-sock)
  (guard (e
          (#t (let ((error-log (kahua-error-string e #t)))
                (send-message (socket-output-port client-sock)
                              '(("x-kahua-status" "SPVR-ERROR"))
                              (list (ref e 'message) error-log)))))
    (receive (header body) (receive-message (socket-input-port client-sock))
      (handle-common self header body
                     (lambda (header body)
                       (guard (e
                               (#t (log-format "[spvr]: client closed connection")))
                         (send-message (socket-output-port client-sock)
                                       header body)))))
    ))

;;; HTTP request handler.  This is provided for testing convenience,
;;; and not intended to turn kahua-spvr a full-featured httpd.
;;; (using Apache mod_proxy may be a feasible solution, though)
;;;
;;; Cf. ftp://ftp.isi.edu/in-notes/rfc2616.txt

(define-method handle-http ((self <kahua-spvr>) client-sock)

  (let ((in  (socket-input-port client-sock))
        (out (socket-output-port client-sock))
        (static-doc-rx (string->regexp #`"^,(regexp-quote (kahua-static-document-url \"\"))/"))
        (ignore-paths '("/favicon.ico"))
        (mime-types '((".jpg"  . "image/jpeg")
                      (".jpeg" . "image/jpeg")
                      (".png"  . "image/png")
                      (".gif"  . "image/gif")
                      (".pdf"  . "application/pdf")
                      (".ps"   . "application/postscript")
                      (".eps"  . "application/postscript")
                      (".doc"  . "application/msword")
                      (".xls"  . "application/ms-excel")
                      (".ppt"  . "application/ms-powerpoint")
                      (".rtf"  . "application/rtf")
                      (".swf"  . "application/x-shockwave-flash")
                      (".html" . "text/html")
                      (".htm"  . "text/html")
                      (".xml"  . "text/xml")
                      (".txt"  . "text/plain")))
        )

    ;; HTTP request handling
    (define (receive-http-request)
      (let1 start-line (read-line in)
        (if (eof-object? start-line)
          (bad-request "bad request")
          (begin
            (log-format "[spvr] http> ~a" start-line)
            (match (string-split start-line #[\s])
              ((method request-uri (? #/HTTP\/1.[01]/ version))
               (if (member method '("GET" "HEAD" "POST"))
                 (process-body (rfc822-header->list in) method request-uri)
                 (not-implemented method)))
              (else
               (bad-request "bad request")))))))

    (define (process-body headers method request-uri)
      (if (and-let* ((expect (rfc822-header-ref headers "expect")))
            (string-ci=? expect "100-continue"))
        (display "HTTP/1.1 100 Continue\r\n" out)
        (flush out))
      (receive (path path-info query fragment) (analyze-uri request-uri)
        (cond
         ((member path ignore-paths)
          (not-found path))
         ((static-doc-rx path) => serve-static-document)
         (else (serve-via-worker headers method query path-info)))))

    (define (serve-static-document m)
      (let* ((relpath (uri-decode-string (m 'after)))
             (abspath (simplify-path (kahua-static-document-path relpath))))
        (cond
         ;; make sure the client does not trick the server
         ((not (string-prefix? (kahua-static-document-path "") abspath))
          (not-found relpath))
         ((file-is-readable? abspath)
          (let* ((suffix (#/\.[^\.]+$/ abspath))
                 (mime-type (if suffix
                              (assoc-ref mime-types (suffix))
                              "application/octet-stream"))
                 (content-size (file-size abspath))
                 (content (call-with-input-file abspath
                            (cut read-block content-size <>))))
            (reply-to-client `(("content-type" ,mime-type)) content)))
         ((file-exists? abspath)
          (forbidden relpath))
         (else
          (not-found relpath)))))

    (define (serve-via-worker headers method query path-info)
      (let* ((params (parse-query method headers query))
             (cont-gsid (or (cgi-get-parameter "x-kahua-cgsid" params)
                            (if (and path-info (pair? (cdr path-info)))
                              (cadr path-info)
                              #f)))
             (worker-type (and path-info (car path-info)))
             (worker-id   (and cont-gsid (gsid->worker-id cont-gsid)))
             (state-gsid (cgi-get-parameter "x-kahua-sgsid" params))
             (header (list*
                      '("x-kahua-bridge" "")
                      `("x-kahua-server-uri" ,#`"http://,(ref self 'httpd-name)")
                      (cond-list
                       (worker-type `("x-kahua-worker" ,worker-type))
                       (state-gsid  `("x-kahua-sgsid" ,state-gsid))
                       (cont-gsid   `("x-kahua-cgsid" ,cont-gsid))
                       (path-info   `("x-kahua-path-info" ,path-info))
                       )))
             )
        (handle-common self header params reply-to-client)))

    (define (analyze-uri uri)
      ;; Returns path, path-info list, query, and fragment
      ;; NB: for now, ignore scheme, user, host and port.
      (receive (scheme user host port path query fragment) (uri-parse uri)
        (values path (get-path-info path) query fragment)))

    (define (get-path-info path)
      (let1 l (cdr (string-split (uri-decode-string path) #[/]))
        (and (pair? l) (not (equal? (car l) ""))
             (filter-map (lambda (p) (if (equal? p "") #f p)) l))))

    ;; HTTP response sending
    (define (reply-to-client header body)
      (let ((status (assoc-ref header "x-kahua-status"))
            (header (if (assoc-ref header "content-type")
                      header
                      `(("content-type"
                         ,(format "text/html; charset=~a"
                                  (gauche-character-encoding)))
                        ,@header))))
        (if (and status (equal? (car status) "SPVR-ERROR"))
          (internal-error (cadr body)) ;;NB: need more comprehensive error page
          (receive (state cont) (get-gsid-from-header header)
            (let ((headers `(,@(map (cut list "Set-cookie" <>)
                                    (construct-cookie-string
                                     `(("x-kahua-sgsid" ,state :path "/"))))
                             ,@(filter (lambda (hdr)
                                         (not (#/^x-kahua-/ (car hdr))))
                                       header)))
                  (status  (find (lambda (hdr)
                                    (string-ci=? "status" (car hdr)))
                                  header))
                  )
              ;; NB: The redirection should use "303 See Other" in HTTP/1.1,
              ;; but see the note of section 10.3.4. of RFC2616.
              (if status
                (http-response (cadr status) headers body)
                (http-response "200 OK" headers body)))))))

    (define (parse-query method headers query)
      (let1 cookie (rfc822-header-ref headers "cookie")
        (parameterize ((cgi-metavariables (cond-list
                                           (cookie `("HTTP_COOKIE" ,cookie)))))
          (if (equal? method "POST")
            (parse-post-query headers)
            (cgi-parse-parameters :query-string (or query "")
                                  :merge-cookies #t)))))

    (define (parse-post-query headers)
      (let1 nlen
          (and-let* ((len  (rfc822-header-ref headers "content-length"))
                     (nlen (x->integer len))
                     ((positive? nlen)))
            nlen)
        (or (and-let* ((ctype (rfc822-header-ref headers "content-type"))
                       ((equal? (take* (mime-parse-content-type ctype) 2)
                                '("multipart" "form-data"))))
              (cgi-parse-parameters :content-type ctype
                                    :content-length nlen
                                    :mime-input in
                                    :merge-cookies #t
                                    :part-handlers '((#t file+name))))
            (and-let* ((nlen)
                       (body (read-block nlen in))
                       ((not (eof-object? body))))
              (cgi-parse-parameters :query-string (ces-convert body "*jp")
                                    :merge-cookies #t))
            '())))      

    (define (http-response status headers body)
      (log-format "[spvr] http< ~a" status)
      (let* ((body (if (pair? body) (tree->string body) body))
             (len  (string-size body)))
        (guard (e
                (#t (log-format "http< [spvr] client closed connection")))
          (with-output-to-port out
            (lambda ()
              (print "HTTP/1.1 " status "\r")
              (dolist (h headers)
                (print (car h) ": " (cadr h) "\r"))
              (print "Server: kahua-spvr\r")
              (print "Content-length: " len "\r")
              (print "\r")
              (display body)
              (flush))))
        (for-each sys-unlink (cgi-temporary-files)) ;remove tmpfiles for upload
        (socket-shutdown client-sock)
        (socket-close client-sock)))

    ;; HTTP diagnostic response
    (define (http-diag-response status body)
      (http-response status
                     '(("Content-type" "text/html"))
                     `(,(html-doctype)
                       ,(html:html
                         (html:head (html:title status))
                         (html:body (html:h1 status) body
                                    (kahua-version-footer))))))

    (define (kahua-version-footer)
      (list (html:hr)
            (html:i "Kahua-spvr Version " (kahua-version)
                    " running at " (ref self 'httpd-name))))

    (define (bad-request msg)
      (http-diag-response "400 Bad Request"
                          (html:p (html-escape-string msg))))

    (define (forbidden path)
      (http-diag-response "403 Forbidden"
                          (html:p "You don't have permission to access "
                                  (html:tt (html-escape-string path))
                                  ".")))

    (define (not-found uri)
      (http-diag-response "404 Not Found"
                          (html:p "The requested URL "
                                  (html:tt (html-escape-string uri))
                                  " was not found on this server.")))

    (define (internal-error msg)
      (http-diag-response "500 Internal Server Error"
                          (html:pre (html-escape-string msg))))

    (define (not-implemented msg)
      (http-diag-response "501 Not Implemented"
                          (html:p "The requested method isn't supported: "
                                  (html-escape-string msg))))

    (define (service-unavailable msg)
      (http-diag-response "503 Service Unavailable"
                          (list
                           (html:p "Service temporarily unavailable ("
                                   (html-escape-string msg)
                                   ")")
                           (html:p "Please try to access later."))))

    (define (session-expired)
      (http-response "200 OK"
                     '((content-type "text/html"))
                     `(,(html-doctype)
                       ,(html:html
                         (html:head (html:title "Session expired"))
                         (html:body (html:h1 "Session expired")
                                    (kahua-version-footer))))))

    ;; The body of handle-http
    (guard (e
            ((<spvr-unknown-worker-type> e)
             (not-found (ref e 'message)))
            ((<spvr-worker-not-running> e)
             (service-unavailable (ref e 'message)))
            ((<spvr-expired-session> e)
             (session-expired))
            (else
             (internal-error (kahua-error-string e #t))))
      (receive-http-request))
    ))

;;
;; Actual server loop
;;
(define (run-server spvr kahua-sock http-socks use-listener)
  (let ((listener (and use-listener
                       (make <listener>
                         :prompter (lambda () (display "kahua> ")))))
        )
    (when kahua-sock
      (selector-add! (selector-of spvr)
                     (socket-fd kahua-sock)
                     (lambda (fd flags)
		       (thread-start!
			(make-thread
			 (lambda ()
			   (handle-kahua spvr (socket-accept kahua-sock))))))
                     '(r)))
    (when http-socks
      (dolist (http-sock http-socks)
        (selector-add! (selector-of spvr)
                       (socket-fd http-sock)
                       (lambda (fd flags)
			 (thread-start!
			  (make-thread
			   (lambda ()
			     (handle-http spvr (socket-accept http-sock))))))
		       '(r))))
    (when listener
      (let1 listener-handler (listener-read-handler listener)
        (set! (port-buffering (current-input-port)) :none)
        (selector-add! (selector-of spvr)
                       (current-input-port)
                       (lambda _ 
			 (listener-handler))
                       '(r)))
      (listener-show-prompt listener))

    ;; The signal mask of "root" thread is changed unexpectedly on Mac OS X 10.4.5,
    ;; maybe something wrong,  but I don't know what is wrong.
    ;; So, I restore the signal mask of "root" thread periodically.
    ;; FIXME!!
    (do () (#f)
      (sys-sigmask SIG_SETMASK *default-sigmask*)
      (selector-select (selector-of spvr) 10.0e6)
      (check-workers spvr))
    ))

;;;=================================================================
;;; Main
;;;

(define (usage)
  (print "Usage: kahua-spvr [options ...]")
  (print "Options:")
  (print "  -c, --conf-file=file  Alternative location of kahua.conf")
  (print "  -i, --interactive     Interactive REPL prompt to stdio")
  (print "  -s, --sockbase=spec   Alternative socket base")
  (print "  -l, --logfile=file    Alternative log file ('-' for stdout)")
  (print "      --user=user       User-custom setting")
  (print "  -H, --httpd=[host:]port  Accept http connection on port")
  (print "  -h, --help            Show this")
  (print "See http://www.kahua.org/ for the details")
  (exit 0))

(define (main args)
  (let-args (cdr args)
      ((conf-file "c|conf-file=s")
       (listener  "i|interactive")
       (sockbase  "s|sockbase=s")  ;; overrides conf file settings
       (logfile   "l|logfile=s")  ;; overrides conf file settings
       (user      "user=s")
       (gosh      "gosh=s")  ;; wrapper script adds this.
       (httpd     "H|httpd=s") ;; standalone httpd mode
       (help      "h|help" => usage)
       (else _ (app-error "Unknown option.  Try --help for the usage."))
       )
    (let ((lib-path (car *load-path*))) ; kahua library path.  it is
                                        ; always the first one, since the
                                        ; wrapper script adds it.
      ;; initialization
      (kahua-init conf-file :user user) ; this must come after getting lib-path
                                        ; since kahua-init adds to *load-path*
      (sys-unlink (kahua-pidpath))
      (with-output-to-file (kahua-pidpath) (lambda () (write (sys-getpid))))
      (when sockbase (set! (kahua-sockbase) sockbase))
      (cond ((equal? logfile "-") (log-open #t :prefix "~Y ~T ~P[~$]: "))
            (logfile (log-open logfile :prefix "~Y ~T ~P[~$]: "))
            (else    (log-open (kahua-logpath "kahua-spvr.log")
                               :prefix "~Y ~T ~P[~$]: ")))
      
      (let* ((sockaddr (supervisor-sockaddr (kahua-sockbase)))
             (spvr     (make <kahua-spvr>
                         :gosh-path gosh
                         :lib-path lib-path
                         :httpd-name (canonicalize-httpd-option httpd)))
             (kahua-sock (make-server-socket sockaddr :reuse-addr? #t))
             (http-socks (and httpd
                              (make-server-sockets "localhost"
                                                   (httpd-port spvr)
                                                   :reuse-addr? #t)))
             (cleanup  (lambda ()
                         (when (is-a? sockaddr <sockaddr-un>)
                           (sys-unlink (sockaddr-name sockaddr)))
                         (nuke-all-workers spvr)
                         (stop-keyserv spvr)
                         (when http-socks (map socket-close http-socks))
                         (log-format "[spvr] exitting")
                         (sys-unlink (kahua-pidpath))))
             )
        (set! *spvr* spvr)
        ;; hack
        (when (is-a? sockaddr <sockaddr-un>)
          (sys-chmod (sockaddr-name sockaddr) #o770))
        (start-keyserv spvr)
        (log-format "[spvr] started at ~a" sockaddr)
        (when http-socks
          (log-format "[spvr] also accepting http at ~a" http-socks))
        (call/cc
         (lambda (bye)
	   (define (finish-server sig)
	     (log-format "[spvr] ~a" (sys-signal-name sig))
	     (cleanup) (bye 0))
	   (set-signal-handler! *TERMINATION-SIGNALS* finish-server)
	   (set-signal-handler! SIGPIPE #f) ; ignore SIGPIPE
	   (set! *default-sigmask* (sys-sigmask 0 #f))
           (guard (e (else
                      (log-format "[spvr] error in main:\n~a" 
                                  (kahua-error-string e #t))
                      (report-error e)
                      (cleanup)
                      (bye 70)))
             (load-app-servers-file)
             (run-default-workers spvr)
             (run-server spvr kahua-sock http-socks listener)
             (sys-unlink (kahua-pidpath))
             (bye 0))))
        ))))

;; Local variables:
;; mode: scheme
;; end:
