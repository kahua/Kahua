;; Common server operations
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: server.scm,v 1.1 2003/12/11 05:39:12 nobsun Exp $

;; This module integrates various kahua.* components, and provides
;; application servers a common utility to communicate kahua-server
;; framework.

(define-module kahua.server
  (use srfi-2)
  (use srfi-27)
  (use text.html-lite)
  (use gauche.parameter)
  (use util.list)
  (use kahua.gsid)
  (use kahua.session)
  (use kahua.persistence)
  (use kahua.user)
  (export kahua-init-server
          kahua-bridge-name
          kahua-default-handler
          kahua-context-add
          kahua-context-ref
          kahua-current-user)
  )
(select-module kahua.server)

;; internally keep worker-id
(define worker-id (make-parameter "dummy"))

;; KAHUA-INIT-SERVER worker-type
;;   Application server should use it within init-server procedure.
;;   Returns worker id.
(define (kahua-init-server worker-type)
  (random-source-randomize! default-random-source)
  (let ((wid (make-worker-id worker-type)))
    (session-manager-init wid)
    (worker-id wid)
    wid))

;; KAHUA-BRIDGE-NAME
;;   A parameter that holds the name of cgi bridge.
;;   App server shouldn't set this value.   Cgi-bridge tells
;;   its name to kahua-server, and KAHUA-DEFAULT-HANDLER will
;;   set it to this parameter.
(define kahua-bridge-name (make-parameter "kahua.cgi")) ;; dummy

;; KAHUA-DEFAULT-HANDLER header body reply-cont default-proc
;;                       &keyword stale-proc error-proc eval-proc
;;                                eval-environment
;;   Default server handler.  The simplest app server can just call
;;   this within handle-request.
;;
;;   stale-proc : When given cont gsid isn't registered (or expired),
;;                This procedure is called with one argument, context.
;;   error-proc : When an error occurred within continuation handler
;;                (or default-proc/stale-proc).  An argument is an
;;                exception object.
;;   eval-proc  : When evaluation of sexpr requested, this proc is
;;                called with two arg, sexpr and module.  This proc
;;                evaluates sexpr.  Should return two values;
;;                first one is either #t for success or #f for error;
;;                The second one is a list of results or an error message.
;;                Each result must be serialized into a string.
;;   eval-environment : A module passed to eval-proc when evaluation is
;;                requested.  The default is the user module.
;;
;; NB: eval protocol will be likely to change; probably adding some
;; authentication mechanism using shared key between spvr and worker.
;;
(define (kahua-default-handler header body reply-cont default-proc . args)
  
  (let-keywords* args ((wrapper-proc identity)
                       (stale-proc kahua-stale-proc)
                       (eval-proc  kahua-eval-proc)
                       (eval-environment (find-module 'user))
                       (error-proc #f))

    (define (run-cont handler input)
      (with-error-handler
          (lambda (e)
            ;; This is the last resort to capture an error.
            ;; App server should provide more appropriate error page
            ;; within its handler.
            (html:html
             (html:head (html:title "Kahua error"))
             (html:body (html:pre (html-escape-string
                                   (call-with-output-string
                                     (cut with-error-to-port <>
                                          (lambda () (report-error e)))))))))
        (if error-proc
          (lambda ()
            (with-error-handler error-proc
              (lambda () (wrapper-proc (handler input)))))
          (lambda () (wrapper-proc (handler input))))))

    (receive (state-id cont-id) (get-gsid-from-header header)
      (let* ((state-id (or state-id (session-state-register)))
             (state   (session-state-get state-id))
             (bridge  (or (car (assoc-ref header "x-kahua-bridge" '(#f)))
                          (kahua-bridge-name)))
             (eval?  (car (assoc-ref header "x-kahua-eval" '(#f))))
             (header (add-gsid-to-header header state-id #f))
             )
        (parameterize ((kahua-bridge-name bridge))
          (if eval?
            (let ((error-output (open-output-string))
                  (std-output   (open-output-string)))
              (receive (ok? result)
                  (with-error-to-port error-output
                    (cut with-output-to-port std-output
                         (cut eval-proc body eval-environment)))
                (if ok?
                  (reply-cont '(("x-kahua-status" "OK"))
                              (list* (get-output-string error-output)
                                     (get-output-string std-output)
                                     result))
                  (reply-cont '(("x-kahua-status" "ERROR"))
                              (string-append (get-output-string error-output)
                                             (get-output-string std-output)
                                             result)))))
            (reply-cont header
                        (run-cont (if cont-id
                                    (or (session-cont-get cont-id) stale-proc)
                                    default-proc)
                                  (cons (list "session-state" state) body)))))
        )))
  )

;; default stale proc
(define (kahua-stale-proc context)
  (html:html
   (html:head (html:title "Kahua error"))
   (html:body (html:h1 "Kahua error - stale session key")
              (html:p "The given session key is wrong, or expired."))))

;; default eval proc
(define (kahua-eval-proc body env)
  (with-error-handler
      (lambda (e)
        (values #f
                (call-with-output-string
                  (cut with-error-to-port <>
                       (lambda () (report-error e))))))
    (lambda ()
      (receive r (eval body env)
        (values #t
                (map (cut write-to-string <>) r))))
    ))

;; KAHUA-CONTEXT-ADD context key1 val1 key2 val2 ...
;; KAHUA-CONTEXT-REF context key
;;
;;  Context is a list of lists, and a key may be a string or a symbol
;;  so it's not just as simple as an alist.

(define (kahua-context-add context . args)
  (let loop ((a args))
    (cond ((null? a) context)
          ((null? (cdr a))
           (error "kahua-context-add: arg list is not even:" args))
          (else (cons (list (car a) (cadr a)) (loop (cddr a)))))))

(define (kahua-context-ref context key . maybe-default)
  (or (and-let* ((p (assoc key context))
                 ((pair? (cdr p))))
        (cadr p))
      (get-optional maybe-default #f)))

;; KAHUA-CURRENT-USER context
;; (setter KAHUA-CURRENT-USER) context user
;;
;;   Gets/sets current user (<kahua-user> object) in the session state
;;   object.   The application must call these procs within with-db's
;;   dynamic extent.

(define kahua-current-user
  (getter-with-setter
   (lambda (context)
     (and-let* ((logname (ref (kahua-context-ref context "session-state")
                              'user)))
       (find-kahua-instance <kahua-user> logname)))
   (lambda (context logname)
     (set! (ref (kahua-context-ref context "session-state") 'user) logname))))

(provide "kahua/server")


