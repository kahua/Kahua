;; Generic application server script.
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-server.scm,v 1.20 2006/10/08 06:00:12 bizenn Exp $
;;
;; This script would be called with a name of the actual application server
;; module name.
;;
;;    kahua-server [-c <conf-file>] <app-server> <args> ...
;; 
;; <app-server> is a module name that implements the service, such as
;; kahua.app.standard.
;;
;; This server code doesn't depend much on kahua internals, to make it
;; to reload the server code without restarting.
;;

(define-module kahua-server
  (use gauche.logger)
  (use gauche.net)
  (use gauche.selector)
  (use gauche.parameter)
  (use gauche.collection)
  (use gauche.parseopt)
  (use gauche.hook)
  (use gauche.charconv)
  (use file.util)
  (use srfi-1)
  (use kahua)
  (export primary-database-name
          main-proc
          kahua-error-proc
          kahua-app-args
          initialize-main-proc
          kahua-server-main
          kahua-add-hook!
          kahua-delete-hook!
          kahua-write-static-file
          )
  )
(select-module kahua-server)

(define primary-database-name (make-parameter #f))
(define kahua-app-args (make-parameter #f))
(define-constant *TERMINATION-SIGNALS* (sys-sigset-add! (make <sys-sigset>) SIGTERM SIGINT SIGHUP))

(define (kahua-application-environment)
  (let ((sandbox (make-sandbox-module))
        (cm (current-module)))
    (for-each (lambda (e)
                (eval `(define-in-module ,sandbox ,(car e) ,(cdr e))
                      cm))
              (map (lambda (s) (cons s (eval s cm))) (module-exports cm)))
    ;; define load-kahua-module as load in sandbox.
    (eval `(define-in-module ,sandbox load ,load-kahua-module) cm)
    sandbox))

;; set sandbox module in main proc.
(define kahua-app-server #f)

(define main-proc (make-parameter (lambda _
 				    (error "Not initialized!"))))
(define kahua-error-proc (make-parameter #f))
(define (initialize-main-proc proc)
  (main-proc proc))

(define kahua-hook-initial  (make-parameter (make-hook)))
(define kahua-hook-before (make-parameter (make-hook)))
(define kahua-hook-after  (make-parameter (make-hook)))

(define-values (kahua-add-hook! kahua-delete-hook!)
  (let1 make-hook-action
      (lambda (action place thunk)
        (case place
          ((initial)  (action (kahua-hook-initial)  thunk))
          ((before) (action (kahua-hook-before) thunk))
          ((after)  (action (kahua-hook-after)  thunk))
          (else     (error "illigal place is specified: ~S" place))
          ))
    (values (lambda (place thunk)
              (make-hook-action add-hook! place thunk))
            (lambda (place thunk)
              (make-hook-action delete-hook! place thunk)))))

(define (database-name)
  (kahua-dbpath (or (primary-database-name)
		    (kahua-default-database-name))))

(define (run-kahua-hook-initial)
  (let1 dbname (database-name)
    (with-db (db dbname)
             (run-hook (kahua-hook-initial)))))

(define (ping-request? header)
  (assoc "x-kahua-ping" header))

(define (handle-request header body reply-cont selector)
  (if (ping-request? header)
    (reply-cont #t #t)
    (with-sigmask SIG_BLOCK *TERMINATION-SIGNALS*
      (lambda ()
	(let1 db (current-db)
	  (unless (kahua-db-ping db)
	    (kahua-db-reopen db))
	  (let1 do-reply (with-kahua-db-transaction db
			   (lambda (db)
			     (run-hook (kahua-hook-before))
			     (begin0
			       (kahua-default-handler header body reply-cont default-handler
						      :error-proc (kahua-error-proc)
						      :eval-environment (current-module))
			       (run-hook (kahua-hook-after))
			       )))
	    (do-reply)))))
      ))

(define (default-handler) ((main-proc)))

(define (load-kahua-module mod)
  (with-error-handler
      (lambda (e)
	(report-error e)
        (print "ERROR loading module")
        (exit 0))
    (lambda ()
      (load mod :environment kahua-app-server))))

(define (run-server worker-id sockaddr profile)
  (let ((sock (make-server-socket sockaddr :reuse-addr? #t :backlog SOMAXCONN))
        (selector (make <selector>)))
    (define (accept-handler fd flag)
      (let1 client (socket-accept sock)
	(call-with-client-socket client
	  (lambda (input output)
	    (set! (port-buffering input) :none)
	    (guard (e
		    (#t (log-format
			 "[server]: Read error occured in accept-handler: ~a" (ref e 'message))))
	      (let ((header (read input))
		    (body   (read input)))
		(handle-request
		 header body
		 (lambda (r-header r-body)
		   (guard (e
			   (#t (log-format
				"[server]: client closed connection")))
		     (begin
		       (write r-header output) (newline output)
		       (write r-body output)   (newline output)
		       (flush output))))
		 selector)))))))

    ;; hack
    (when (is-a? sockaddr <sockaddr-un>)
      (sys-chmod (sockaddr-name sockaddr) #o770))
    (run-kahua-hook-initial)
    (format #t "~a\n" worker-id)
    (selector-add! selector (socket-fd sock) accept-handler '(r))
    (with-kahua-db-connection (database-name)
      (lambda (db)
	(do () (#f)
	  (kahua-profiler-start profile)
	  (dotimes (_ 100)
	    (selector-select selector))
	  (kahua-profiler-flush profile)
	  )))))

(define *kahua-top-module* #f)

;; update server with top-module or some files
(define (update-server . files)
  (define (load-k-module mod)
    (with-error-handler
     (lambda (e)
       (report-error e)
       #f)
     (lambda ()
       (load mod :environment kahua-app-server))))

  (if (pair? files)
      (every (lambda (f) (load-k-module f)) files)
      (load-k-module *kahua-top-module*)))

;; Profiler Facility

(define (kahua-profiler-start pfile)
  (when pfile
    (guard (e (else (log-format "[server] cannot start profiler: ~a" (ref e 'message))))
      (profiler-start))))

(define (kahua-profiler-flush pfile)
  (when pfile
    (guard (e (else (log-format "[server] fail to flush profiling result: ~a" (ref e 'message))))
      (profiler-stop)
      (with-output-to-file pfile
	(lambda ()
	  (format #t "====Profiler flushed: ~s\n" (sys-localtime (sys-time)))
	  (profiler-show :max-rows #f))
	:if-exists :append)
      (profiler-reset))))

(define (kahua-server-main args)
  (let-args (cdr args) ((site "S=s" #f)
			(conf-file "c=s" #f)
                        (user "user=s" #f)
                        (keyserv "k=s" #f)
			(db "default-db=s" #f)
			(prof "profile=s" #f)
                        . mods)
    (unless (pair? mods)
      (error "usage: kahua-server [-S <site>] [-c <conf>] [-user <user>] [-k <keyserv-id>] [-default-db <default-db-path>] [-profile <profile-out>] <app-server> <args> ..." mods))
    (set! *kahua-top-module* (car mods))
    (kahua-common-init site conf-file user)
    (set! kahua-app-server (kahua-application-environment))
    (primary-database-name db)
    (initialize-plugins)
    (kahua-app-args (cdr mods))
    (load-kahua-module (car mods))
    (let* ((worker-name (car (string-split (sys-basename (car mods)) #\.))) 
           (worker-id (kahua-init-server worker-name keyserv))
	   (profile (and prof (string-append prof "." worker-id)))
           (sockbase  (kahua-sockbase))
           (sockaddr  (worker-id->sockaddr worker-id sockbase))
           (cleanup   (lambda ()
                        (log-format "[~a] exit" worker-name)
                        (when (is-a? sockaddr <sockaddr-un>)
                          (sys-unlink (sockaddr-name sockaddr)))
			(and-let* ((db (current-db))
				   ((active? db)))
			  (kahua-db-close db #f)))))
      (begin0
	(call/cc
	 (lambda (bye)
	   (define (finish-server sig)
	     (log-format "[~a] ~a" worker-name (sys-signal-name sig)) (bye 0))
	   (log-open (kahua-logpath "kahua-spvr.log") :prefix "~Y ~T ~P[~$]: ")
	   (set-signal-handler! *TERMINATION-SIGNALS* finish-server)
	   (with-error-handler
	     (lambda (e)
	       (log-format "[server] error in main:\n~a"
			   (kahua-error-string e #t))
	       (report-error e)
	       (bye 70))
	     (lambda ()
	       (log-format "[~a] start" worker-name)
	       (run-server worker-id sockaddr profile)
	       (bye 0)))))
	(cleanup))
      )))

(define (kahua-write-static-file path nodes context . rargs)
  (when (string-scan path "../")
    (error "can't use 'up' component in kahua-write-static-file" path))
  (let ((write-path (kahua-static-document-path path)))
    (make-directory* (sys-dirname write-path))
    (with-output-to-file write-path
      (lambda ()
	(display (kahua-render nodes context)))
      :encoding (get-keyword :encoding rargs (gauche-character-encoding)))))

;; Main -----------------------------------------------------

(select-module user)
(import kahua-server)

(define (main args) (kahua-server-main args))

;; Local variables:
;; mode: scheme
;; end:
