;; Common server operations
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: server.scm,v 1.6 2004/01/21 01:25:11 shiro Exp $

;; This module integrates various kahua.* components, and provides
;; application servers a common utility to communicate kahua-server
;; framework.

(define-module kahua.server
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use srfi-13)
  (use srfi-27)
  (use text.html-lite)
  (use gauche.parameter)
  (use gauche.sequence)
  (use rfc.uri)
  (use util.list)
  (use sxml.tools)
  (use file.util)
  (use kahua.gsid)
  (use kahua.session)
  (use kahua.persistence)
  (use kahua.user)
  (export kahua-init-server
          kahua-bridge-name
          kahua-default-handler
          kahua-current-context
          kahua-context-ref
          kahua-current-user
          kahua-worker-type
          kahua-merge-headers
          not-accessible?
          add-element!
          define-element
          define-entry
          entry-lambda
          interp-html
	  interp-html-rec)
  )
(select-module kahua.server)

;; internally keep worker-id
(define worker-id (make-parameter "dummy"))

;; internally keep worker-type
(define worker-type (make-parameter "dummy"))
(define (kahua-worker-type)
        (worker-type))

;; utility
(define (ref-car cmp lis item . maybe-default)
  (cond ((assoc item lis cmp) => cadr)
        (else (get-optional maybe-default #f))))
(define assq-ref-car  (pa$ ref-car eq?))
(define assoc-ref-car (pa$ ref-car equal?))

;; Context
;;  A context is established every time the control is passed from
;;  the client to the server.  It is available as a parameter
;;  kahua-current-context.

(define kahua-current-context (make-parameter '()))

;; KAHUA-INIT-SERVER worker-type
;;   Application server should use it within init-server procedure.
;;   Returns worker id.
(define (kahua-init-server wtype)
  (random-source-randomize! default-random-source)
  (let ((wid (make-worker-id wtype)))
    (session-manager-init wid)
    (worker-type wtype)
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
;;                                render-proc
;;                                eval-environment
;;   Default server handler.  The simplest app server can just call
;;   this within handle-request.
;;
;;   stale-proc : When given cont gsid isn't registered (or expired),
;;                This procedure is called with one argument, context.
;;   error-proc : When an error occurred within continuation handler
;;                (or default-proc/stale-proc).  An argument is an
;;                exception object.
;;   render-proc : ([SXML], Context) -> (Stree, Context).
;;                The SXML nodeset the handler returns is passed to
;;                this procedure, which should render the SXML to
;;                appropriate format.
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
  
  (let-keywords* args ((render-proc kahua-render-proc)
                       (stale-proc kahua-stale-proc)
                       (eval-proc  kahua-eval-proc)
                       (eval-environment (find-module 'user))
                       (error-proc #f))

    ;; (Handler, Context) -> (Stree, Context)
    (define (run-cont handler context)
      (parameterize ((kahua-current-context context))
        (with-error-handler
            (lambda (e)
              ;; This is the last resort to capture an error.
              ;; App server should provide more appropriate error page
              ;; within its handler.
              (values
               (html:html
                (html:head (html:title "Kahua error"))
                (html:body (html:pre
                            (html-escape-string
                             (call-with-output-string
                               (cut with-error-to-port <>
                                    (lambda () (report-error e))))))))
               context))
          (if error-proc
            (lambda ()
              (with-error-handler error-proc
                (lambda ()
                  (render-proc (handler) context))))
            (lambda ()
              (render-proc (handler) context))))))

    ;; Handles 'eval' protocol
    ;; () -> ([Headers], [Result])
    (define (run-eval)
      (let ((error-output (open-output-string))
            (std-output   (open-output-string)))
        (receive (ok? result)
            (with-error-to-port error-output
              (cut with-output-to-port std-output
                   (cut eval-proc body eval-environment)))
          (if ok?
            (values '(("x-kahua-status" "OK"))
                    (list* (get-output-string error-output)
                           (get-output-string std-output)
                           result))
            (values '(("x-kahua-status" "ERROR"))
                    (string-append (get-output-string error-output)
                                   (get-output-string std-output)
                                   result))))))
     
    ;; Main dispatcher body
    (receive (state-id cont-id) (get-gsid-from-header header)
      (let* ((state-id (or state-id (session-state-register)))
             (state   (session-state-get state-id))
             (bridge  (or (car (assoc-ref header "x-kahua-bridge" '(#f)))
                          (kahua-bridge-name)))
             (path-info (car (assoc-ref header "x-kahua-path-info" '(()))))
             (eval?  (car (assoc-ref header "x-kahua-eval" '(#f))))
             (header (add-gsid-to-header header state-id #f))
             )
        (parameterize ((kahua-bridge-name bridge))
          (if eval?
            (receive (headers result) (run-eval)
              (reply-cont headers result))
            (receive (stree context)
                (run-cont (if cont-id
                            (or (session-cont-get cont-id) stale-proc)
                            default-proc)
                          (list*
                           (list "session-state" state)
                           (list "x-kahua-path-info"
                                 (drop* path-info 2))
                           body))
              (let1 extra-headers
                  (assoc-ref-car context "extra-headers" '())
                (reply-cont (kahua-merge-headers header extra-headers)
                            stree)))))
        )))
  )

;; default stale proc
(define (kahua-stale-proc)
  `((html (head (title "Kahua error"))
          (body (h1 "Kahua error - stale session key")
                (p "The given session key is wrong, or expired.")))))

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

;; default render proc
;; TODO: should apply interp-html-rec to all nodes!
(define (kahua-render-proc nodes context)
  (interp-html-rec (car nodes) context values))

;; KAHUA-CONTEXT-REF key [default]
;;
;;  Context is a list of lists, and a key may be a string or a symbol
;;  so it's not just as simple as an alist.

(define (kahua-context-ref key . maybe-default)
  (apply assoc-ref-car (kahua-current-context) key maybe-default))

;; KAHUA-CURRENT-USER
;; (setter KAHUA-CURRENT-USER) user
;;
;;   Gets/sets current user (<kahua-user> object) in the session state
;;   object.   The application must call these procs within with-db's
;;   dynamic extent.

(define kahua-current-user
  (getter-with-setter
   (lambda ()
     (and-let* ((logname (ref (kahua-context-ref "session-state") 'user)))
       (find-kahua-instance <kahua-user> logname)))
   (lambda (logname)
     (set! (ref (kahua-context-ref "session-state") 'user) logname))))

;; KAHUA-MERGE-HEADERS :: ([Headers],...) -> [Headers]
;;
;;   Headers are list of list, e.g.
;;     (("content-type" "text/html")
;;      ("x-kahua-cgsid" "r903t:0r9aen:2425"))
;;   This function merges given header list.  If the same field
;;   appears, the last one take precedence.

(define (kahua-merge-headers headers . more-headers)
  (let outer ((headers headers)
              (more    more-headers))
    (if (null? more)
      headers
      (let inner ((headers headers)
                  (header (car more)))
        (if (null? header)
          (outer headers (cdr more))
          (inner (cons (car header) (alist-delete (caar header) headers))
                 (cdr header)))
        ))
    ))

;;==========================================================
;; Access control
;;

(define (not-accessible? auxs context)
  (cond ((assq-ref auxs 'require-role)
         => (lambda (roles)
              (not (kahua-user-has-role? (kahua-current-user) roles))))
        (else #f)))

;;==========================================================
;; Entry management
;;

;; Define-entry creates a "well-known" or "permanent" continuation ID.
;; That means the user can always visit that node by using URL like
;; 'kahua.cgi/type/entry-name', where 'type' is an application type
;; and 'entry-name' is the name of the permanent entry.
;; Note that it is the same format with the transient continuation ID;
;; continuation IDs are distinguished by the name that matches #/^\d+-/.
;;
;; An entry can also receive a parameters, by name (using QUERY_STRING)
;; and/or by position (using PATH_INFO).
;;

;;  [syntax] entry-lambda (arg ... :keyword karg ...)
;;
;;   This creates a special procedure, which can be used as an entry
;;   procedure.  arg ... will be bound to a positional arguments,
;;   and kargs ... will be bound to a keyword arguments.

(define-syntax entry-lambda
  (syntax-rules ()
    ;; finishing expansion
    ((entry-lambda "finish" args pargs kargs body)
     (make-parameterized-entry-closure 'pargs 'kargs (lambda args . body)))
    ;; collecting positional args
    ((entry-lambda "pargs" () pargs body)
     (entry-lambda "finish" pargs pargs () body))
    ((entry-lambda "pargs" (:keyword . syms) pargs body)
     (entry-lambda "kargs" syms pargs pargs () body))
    ((entry-lambda "pargs" (sym . syms) (parg ...) body)
     (entry-lambda "pargs" syms (parg ... sym) body))
    ;; collecting keyword args
    ((entry-lambda "kargs" () args pargs kargs body)
     (entry-lambda "finish" args pargs kargs body))
    ((entry-lambda "kargs" (sym . syms) (arg ...) pargs (karg ...) body)
     (entry-lambda "kargs" syms (arg ... sym) pargs (karg ... sym) body))
    ;; initial entry
    ((entry-lambda args body1 . bodies)
     (entry-lambda "pargs" args () (body1 . bodies)))
    ;; error handling
    ((entry-lambda . _)
     (syntax-error "malformed entry-lambda:" (entry-lambda . _)))
    ))

;; helper procedure
(define (make-parameterized-entry-closure pargs kargs proc)
  (let ((pargs-str (map x->string pargs))
        (kargs-str (map x->string kargs)))
    (lambda ()
      (let1 path-info (kahua-context-ref "x-kahua-path-info" '())
        (apply proc
               (append (map-with-index (lambda (ind parg)
                                         (list-ref path-info ind #f))
                                       pargs-str)
                       (map kahua-context-ref kargs-str)))))
    ))

;;  [syntax] define-entry (name arg ... :keyword karg ...)
;;  [syntax] define-entry name (entry-lambda ....)

(define-syntax define-entry
  (syntax-rules ()
    ((define-entry (name . args) . body)
     (define-entry name (entry-lambda args . body)))
    ((define-entry name expr)
     (define name
       (let ((x expr))
         (add-entry! 'name x)
         x)))
    ))

(define (add-entry! name proc)
  (session-cont-register proc (symbol->string name)))

;;==========================================================
;;  Default SXML tree interpreter - generates HTML
;;

;; interp-html :: Node -> Context -> Stree
;;    where Stree is a list of string segments passed to tree->string.
(define (interp-html nodes context)
  (interp-html-rec (car nodes) context (lambda (s _) s)))

;; internal loop 
(define (interp-html-rec node context cont)
  (let ((name (sxml:element-name node)))
    (if (not name)
      (cont (if (string? node) 
              (sxml:string->xml node)
              "")
            context)
      (let ((attrs (sxml:attr-list-u node))
            (auxs  (sxml:aux-list-u node))
            (contents (sxml:content node)))
        (if (not-accessible? auxs context)
          (cont "" context)
          (cond ((get-element-handler name)
		 => (cut <> attrs auxs contents context
			 (lambda (nds cntx)
			   (handle-element-contents nds cntx cont))))
                (else 
                 (default-element-handler name attrs contents context cont)))
          )))
    ))

(define (default-element-handler tag attrs content context cont)
  (handle-element-contents content context
                           (lambda (stree context)
			     (cont `("<" ,tag ,(map sxml:attr->xml attrs) ">"
                                     ,@stree
                                     "</" ,tag "\n>")
                                   context))))

(define (handle-element-contents contents context cont)
  (if (null? contents)
    (cont '() context)
    (interp-html-rec (car contents) context
                     (lambda (stree context)
                       (handle-element-contents (cdr contents) context
                                                (lambda (stree2 context)
                                                  (cont (cons stree stree2)
                                                        context)
                                                  ))))))

;;==========================================================
;; Element management
;;

;; define-element allows the application programmer to define
;; a special SXML node type (element).  When the SXML
;; interpreter sees the node, its element handler is invoked,
;; which can translate the node into SXML.
;;
;;  element-handler :: Attrs -> Auxs -> [Node] -> Context ->
;;                     ([Node] -> Context -> a) -> a
;;
;;    where 'a' depends on the interpreter --- element handler is
;;    agnostic about it.

(define-syntax define-element
  (syntax-rules ()
    ((define-element name args . body)
     (add-element! 'name (lambda args . body)))))

(define-values (add-element! get-element-handler)
  (let ((table (make-hash-table)))
    (values
     (lambda (tag handler)
       (hash-table-put! table tag handler))
     (lambda (tag)
       (hash-table-get table tag #f)))))

;; Utilities for element handler

;; Comb out arguments to pass to continuation procedure.
;; [Args] -> [PositionalArgs], [KeywordArgs]

(define (extract-cont-args args form)
  (define (valid-value? v)
    (or (string? v) (symbol? v) (number? v)))

  (receive (pargs kargs)
      (partition (lambda (arg)
                   (if (pair? arg)
                     (if (and (symbol? (car arg))
                              (pair? (cdr arg))
                              (valid-value? (cadr arg)))
                       #f
                       (error "bad continuation argument in element:" form))
                     (if (valid-value? arg)
                       #t
                       (error "bad continuation argument in element:" form))))
                 args)
    (values (map x->string pargs)
            (map (lambda (p) (cons (car p) (x->string (cadr p)))) kargs))))

;;-----------------------------------------------------------
;; Pre-defined element handlers
;;

;;
;; a/cont element - embeds continuation within a link node.
;;
;; `(a/cont (@@ (cont ,closure [arg ...])) contents)
;;
;;  With arg ..., you can pass parameters to the continuation.
;;  Each arg should either be:
;;
;;    <value> : passed as a positional argument (via PATH_INFO).
;;    (<symbol> <value>) : passed as a keyword argument. (via QUERY_STRING).
;;
;;      where <value> is either a string, symbol, or a number.
;;      (x->string is applied on <value>).

(define-element a/cont (attrs auxs contents context cont)

  (define (build-argstr cont-args)
    (receive (pargs kargs)
        (extract-cont-args cont-args `(a/cont ,attrs ,auxs ,@contents))
      (string-concatenate
       `(,(string-join (map uri-encode-string pargs) "/" 'prefix)
         ,@(if (null? kargs)
             '()
             `("?"
               ,@(string-join (map (lambda (karg)
                                     (format "~a=~a"
                                             (uri-encode-string (car karg))
                                             (uri-encode-string (cdr karg))))
                                   kargs)
                              "&")))))))
  
  (let* ((clause (assq-ref auxs 'cont))
         (id     (and clause (session-cont-register (car clause))))
         (argstr (and clause (build-argstr (cdr clause)))))
    (cont
     `((a (@ (href ,(format "~a/~a/~a~a"
                            (kahua-bridge-name) (kahua-worker-type)
                            (or id "") (or argstr ""))))
          ,@contents))
     context)))

;;
;; form/cont
;; 
;; `(form/cont (@@ (cont ,closure)) contents)
;;

(define-element form/cont (attrs auxs contents context cont)
  (let* ((cont-closure (assq-ref auxs 'cont))
         (id (session-cont-register (car cont-closure))))
    (cont
     `((form (@ (method "POST") 
                (action ,(format "~a/~a/~a"
                                 (kahua-bridge-name)
                                 (kahua-worker-type)
                                 id)))
             ,@contents))
     context)))

;;
;; extra-header - inserts protocol header to the reply message
;; 
;; `(extra-header (@@ (name ,name) (value ,value)))
;;

(define-element extra-header (attrs auxs contents context cont)
  (let* ((name    (assq-ref-car attrs 'name))
         (value   (assq-ref-car attrs 'value))
         (headers (assoc-ref-car context "extra-headers" '())))
    (and name value
         (cont '()
               (cons `("extra-headers"
                       ,(kahua-merge-headers headers `((,name ,value))))
                     context)))))

(provide "kahua/server")
