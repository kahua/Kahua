;; Generic application server script.
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-server.scm,v 1.6.2.2 2005/11/29 15:13:47 cut-sea Exp $

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
  (use gauche.threads)
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
          )
  )
(select-module kahua-server)

(define primary-database-name (make-parameter #f))
(define kahua-app-args (make-parameter #f))

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

(define kahua-hook-before (make-parameter (make-hook)))
(define kahua-hook-after  (make-parameter (make-hook)))

(define (kahua-add-hook! place thunk)
  (case place
    ((before) (add-hook! (kahua-hook-before) thunk))
    ((after)  (add-hook! (kahua-hook-after)  thunk))
    (else     (error "illigal place is specified: ~S" place))
    ))

(define (kahua-delete-hook! place thunk)
  #f)

(define (ping-request? header)
  (assoc "x-kahua-ping" header))

(define (handle-request header body reply-cont)
  (if (ping-request? header)
    (reply-cont #t #t)
    (let1 dbname (or (primary-database-name)
                     (build-path (ref (kahua-config) 'working-directory) "db"))
      (with-db (db dbname)
        (run-hook (kahua-hook-before))
        (begin0
          (kahua-default-handler header body reply-cont default-handler
                                 :error-proc (kahua-error-proc)
                                 :eval-environment (current-module))
          (run-hook (kahua-hook-after))
          )))))

(define (default-handler) ((main-proc)))

(define (load-kahua-module mod)
  (with-error-handler
      (lambda (e)
	(report-error e)
        (print "ERROR loading module")
        (exit 0))
    (lambda ()
      (load mod :environment kahua-app-server))))

(define (run-server worker-id sockaddr)
  (define (accept-handler input output)
    (thread-start!
     (make-thread
      (lambda ()
	(guard (e
		(#t (log-format
		     "[server]: Read error occured in accept-handler")))
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
			     (flush output)))
		    (socket-close client))
		  )))))))

  (let loop ((sock (make-server-socket sockaddr :reuse-addr? #t)))
    ;; hack
    (when (is-a? sockaddr <sockaddr-un>)
      (sys-chmod (sockaddr-name sockaddr) #o770))
    (format #t "~a\n" worker-id)
    (let* ((client (socket-accept sock))
	   (input  (socket-input-port client :buffered? #t))
	   (output (socket-output-port client)))
      (accept-handler input output))
    (loop sock))
  )

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

(define (kahua-server-main args)
  (let-args (cdr args) ((conf-file "c=s" #f)
                        (user "user=s" #f)
                        (keyserv "k=s" #f)
                        . mods)
    (unless (pair? mods)
      (error "usage: kahua-server [-c <conf>] [-user <user>] [-k <keyserv-id>] <app-server> <args> ..." mods))
    (set! *kahua-top-module* (car mods))
    (kahua-init conf-file :user user)
    (set! kahua-app-server (kahua-application-environment))
    (initialize-plugins)
    (kahua-app-args (cdr mods))
    (load-kahua-module (car mods))
    (let* ((worker-name (car (string-split (sys-basename (car mods)) #\.))) 
           (worker-id (kahua-init-server worker-name keyserv))
           (sockbase  (kahua-sockbase))
           (sockaddr  (worker-id->sockaddr worker-id sockbase))
           (cleanup   (lambda ()
                        (log-format "[~a] exit" worker-name)
                        (when (is-a? sockaddr <sockaddr-un>)
                          (sys-unlink (sockaddr-name sockaddr))))))
      (log-open (kahua-logpath "kahua-spvr.log") :prefix "~Y ~T ~P[~$]: ")
      (set-signal-handler! SIGINT  (lambda _
                                     (log-format "[~a] SIGINT" worker-name)
                                     (cleanup)
                                     (exit 0)))
      (set-signal-handler! SIGHUP  (lambda _
                                     (log-format "[~a] SIGHUP" worker-name)
                                     (cleanup)
                                     (exit 0)))
      (set-signal-handler! SIGTERM (lambda _
                                     (log-format "[~a] SIGTERM" worker-name)
                                     (cleanup)
                                     (exit 0)))
;;      (set-signal-handler! SIGPIPE #f) ; ignore SIGPIPE

      (with-error-handler
          (lambda (e)
            (log-format "[server] error in main:\n~a"
                        (kahua-error-string e #t))
            (report-error e)
            (cleanup)
            70)
        (lambda ()
          (log-format "[~a] start" worker-name)
          (run-server worker-id sockaddr))))
    ))

;; Main -----------------------------------------------------

(select-module user)
(import kahua-server)

(define (main args) (kahua-server-main args))

;; Local variables:
;; mode: scheme
;; end:
