;; Common server operations
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: server.scm,v 1.33 2005/09/09 15:03:31 cut-sea Exp $

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
  (use gauche.charconv)
  (use rfc.uri)
  (use util.list)
  (use sxml.tools)
  (use file.util)
  (use kahua.gsid)
  (use kahua.partcont)
  (use kahua.session)
  (use kahua.persistence)
  (use kahua.user)
  (use kahua.util)
  (use kahua.elem)
  (use kahua.pdf)
  (export kahua-init-server
          kahua-bridge-name
          kahua-server-uri
          kahua-self-uri
          kahua-self-uri-full
          kahua-default-handler
          kahua-current-context
          kahua-context-ref
          kahua-context-ref*
          kahua-current-user
          kahua-current-user-name
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

;; KAHUA-INIT-SERVER worker-type [session-server-id]
;;   Application server should use it within init-server procedure.
;;   Returns worker id.
(define (kahua-init-server wtype . maybe-ssid)
  (random-source-randomize! default-random-source)
  (let ((wid (make-worker-id wtype)))
    (apply session-manager-init wid maybe-ssid)
    (worker-type wtype)
    (worker-id wid)
    wid))

;; KAHUA-BRIDGE-NAME
;;   A parameter that holds the name of cgi bridge.
;;   App server shouldn't set this value.   Cgi-bridge tells
;;   its name to kahua-server, and KAHUA-DEFAULT-HANDLER will
;;   set it to this parameter.
(define kahua-bridge-name (make-parameter "kahua.cgi")) ;; dummy

;; KAHUA-SERVER-URI
;;   A parameter that holds the server URI (scheme://server:port).
;;   The cgi-bridge tells this info to the app server.
(define kahua-server-uri (make-parameter "http://localhost")) ;; dummy

;; KAHUA-SELF-URI path ...
;; KAHUA-SELF-URI-FULL path ...
;;   Generates a self-referencing uri.  arguments has to be uriencoded.

(define (kahua-self-uri . paths)
  (apply build-path
         (format "~a/~a/" (kahua-bridge-name) (kahua-worker-type))
         paths))

(define (kahua-self-uri-full . paths)
  (apply build-path
         (format "~a~a/~a/"
                 (kahua-server-uri) (kahua-bridge-name) (kahua-worker-type))
         paths))

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
                             (kahua-error-string e #t)))))
               context))
          (if error-proc
            (lambda ()
              (with-error-handler
                  (lambda (e)
                    (render-proc (error-proc e) context))
                (lambda ()
                  (render-proc (reset/pc (handler)) context))))
            (lambda ()
              (render-proc (reset/pc (handler)) context))))))

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
      (let* ((state-id  (or state-id (session-state-register)))
             (state     (session-state-get state-id))
             (header    (add-gsid-to-header header state-id #f)))
        (parameterize ((kahua-bridge-name
                        (assoc-ref-car header "x-kahua-bridge" 
                                       (kahua-bridge-name)))
                       (kahua-server-uri
                        (assoc-ref-car header "x-kahua-server-uri"
                                       (kahua-server-uri)))
                       )
          (if (assoc-ref-car header "x-kahua-eval" #f)
            (receive (headers result) (run-eval)
              (reply-cont headers result))
            (receive (stree context)
                (run-cont (if cont-id
                            (or (session-cont-get cont-id) stale-proc)
                            default-proc)
                          (list*
                           `("session-state" ,state)
                           `("x-kahua-path-info"
                             ,(drop* (assoc-ref-car header "x-kahua-path-info"
                                                    '())
                                     2))
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
        (values #f (kahua-error-string e #t)))
    (lambda ()
      (receive r (eval body env)
        (values #t
                (map (cut write-to-string <>) r))))
    ))

;; default render proc
;; TODO: should apply interp-html-rec to all nodes!
(define (kahua-render-proc nodes context)
  (let* ((expanded (cond 
                    ((procedure? nodes) (car (rev-nodes (exec '() nodes)))) 
                    ((eq? (car nodes) 'node-set) (cadr nodes))
                    (else (car nodes))))
         (interp (get-interp expanded)))
    (interp expanded context values)))

(define-values (add-interp! get-interp)
  (let ((table (make-hash-table))
        (default-interp #f))
    (values
     (lambda (type proc . default)
       (let ((option (get-optional default #f)))
         (hash-table-put! table type proc)
         (if (or (not default-interp) option)
           (set! default-interp proc))))
     (lambda (nodes)
       (hash-table-get table (car nodes) default-interp)))))


;; KAHUA-CONTEXT-REF key [default]
;;
;;  Context is a list of lists, and a key may be a string or a symbol
;;  so it's not just as simple as an alist.

(define (kahua-context-ref key . maybe-default)
  (apply assoc-ref-car (kahua-current-context) key maybe-default))

;; KAHUA-CONTEXT-REF* key [default]
;;
;;  Like kahua-context-ref, but retrieves multiple values as a list.
;;  It returns '() if the data corresponding to the key isn't found.

(define (kahua-context-ref* key . maybe-default)
  (or (apply assoc-ref (kahua-current-context) key maybe-default)
      '()))

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

;; KAHUA-CURRENT-USER-NAME
;; (setter KAHUA-CURRENT-USER-NAME) user
;;
;;   Gets/sets current user name (N.B. not <kahua-user> object)
;;   in the session state object.

(define kahua-current-user-name
  (getter-with-setter
   (lambda ()
     (ref (kahua-context-ref "session-state") 'user))
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

;;  [syntax] entry-lambda (arg ... :keyword karg ... :rest restarg)
;;
;;   This creates a special procedure, which can be used as an entry
;;   procedure.  arg ... will be bound to a positional arguments,
;;   and kargs ... will be bound to a keyword arguments.
;;   restarg can be used to receive remaining positional arguments.
;;   (NB: it doesn't include keyword args).

(define-syntax entry-lambda
  (syntax-rules ()
    ;; finishing expansion
    ((entry-lambda "finish" args pargs kargs #f body)
     (make-parameterized-entry-closure 'pargs 'kargs #f
                                       (lambda args . body)))
    ((entry-lambda "finish" () pargs kargs rarg body)
     (make-parameterized-entry-closure 'pargs 'kargs 'rarg
                                       (lambda rarg . body)))
    ((entry-lambda "finish" (args ...) pargs kargs rarg body)
     (make-parameterized-entry-closure 'pargs 'kargs 'rarg
                                       (lambda (args ... . rarg) . body)))
    ;; collecting positional args
    ((entry-lambda "pargs" () pargs body)
     (entry-lambda "finish" pargs pargs () #f body))
    ((entry-lambda "pargs" (:rest rarg) pargs body)
     (entry-lambda "finish" pargs pargs () rarg body))
    ((entry-lambda "pargs" (:rest rarg :keyword . syms) pargs body)
     (entry-lambda "kargs" syms pargs pargs () rarg body))
    ((entry-lambda "pargs" (:rest . _) pargs body)
     (syntax-error "malformed entry-lambda :rest form:" (:rest . _)))
    ((entry-lambda "pargs" (:keyword . syms) pargs body)
     (entry-lambda "kargs" syms pargs pargs () #f body))
    ((entry-lambda "pargs" (sym . syms) (parg ...) body)
     (entry-lambda "pargs" syms (parg ... sym) body))
    ;; collecting keyword args
    ((entry-lambda "kargs" () args pargs kargs rarg body)
     (entry-lambda "finish" args pargs kargs rarg body))
    ((entry-lambda "kargs" (:rest rarg) args pargs kargs #f body)
     (entry-lambda "finish" args pargs kargs rarg body))
    ((entry-lambda "kargs" (:rest rarg) args pargs kargs rarg1 body)
     (syntax-error "duplicate :rest args in entry-lambda:" (:rest rarg)))
    ((entry-lambda "kargs" (:rest . _) args pargs kargs rarg1 body)
     (syntax-error "malformed entry-lambda :rest form:" (:rest . _)))
    ((entry-lambda "kargs" (sym . syms) (arg ...) pargs (karg ...) rarg body)
     (entry-lambda "kargs" syms (arg ... sym) pargs (karg ... sym) rarg body))
    ;; initial entry
    ((entry-lambda args body1 . bodies)
     (entry-lambda "pargs" args () (body1 . bodies)))
    ;; error handling
    ((entry-lambda . _)
     (syntax-error "malformed entry-lambda:" (entry-lambda . _)))
    ))

;; helper procedure
(define (make-parameterized-entry-closure pargs kargs rarg proc)
  (let ((kargs-str (map x->string kargs)))
    (lambda ()
      (let1 path-info (kahua-context-ref "x-kahua-path-info" '())
        (apply proc
               (append (map-with-index (lambda (ind parg)
                                         (list-ref path-info ind #f))
                                       pargs)
                       (map kahua-context-ref kargs-str)
                       (if rarg
                         (drop* path-info (length pargs))
                         '())))))
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
(define (interp-html-rec-gen default-handler)
  (lambda (node context cont)
    (let ((name (sxml:element-name node)))
      (if (not name)
          (cont (if (string? node) 
                    (sxml:string->html node)
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
                       (default-handler name attrs contents context cont)))
                )))
      )))

;; customized version of sxml:attr->xml
;;  - fixes single-quote bug
;;  - omits attributes with value #f
(define (sxml:attr->xml-bis attr)
  (if (cadr attr)
    (let* ((v (x->string (cadr attr)))
           (q (if (string-any #\' v) "\"" "'")))
      (list " " (sxml:ncname attr) "=" q v q))
    ""))

(define (default-element-handler tag attrs content context cont)
  (handle-element-contents
   content context
   (lambda (stree context)
     (if (memq tag '(area base basefont br col frame hr img
                          input isindex link meta param))
       (cont `("<" ,tag ,(map sxml:attr->xml-bis attrs) " />") context)
       (cont `("<" ,tag ,(map sxml:attr->xml-bis attrs) ">"
               ,@stree
               "</" ,tag "\n>")
             context)))))

(define (default-element-handler-bis tag attrs content context cont)
  (handle-element-contents-bis
   content context
   (lambda (stree context)
     (cont `("<" ,tag ,(map sxml:attr->xml-bis attrs) ">"
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

(define (handle-element-contents-bis contents context cont)
  (if (null? contents)
    (cont '() context)
    (interp-html-rec-bis
     (car contents) context
     (lambda (stree context)
       (handle-element-contents-bis (cdr contents) context
                                    (lambda (stree2 context)
                                      (cont (cons stree stree2)
                                            context)
                                      ))))))

(define interp-html-rec (interp-html-rec-gen default-element-handler))
(define interp-html-rec-bis  (interp-html-rec-gen default-element-handler-bis))

;; set interp-html-rec as default interp
(add-interp! 'html interp-html-rec #t)

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
  (define (val v)
    (cond ((not v) #f) ;; giving value #f makes keyword arg omitted
          ((or (string? v) (symbol? v) (number? v))
           (x->string v))
          (else
           (errorf "bad continuation argument ~a in element ~a" v form))))
  (receive (kargs pargs) (partition pair? args)
    (values (map val pargs)
            (filter-map (lambda (p)
                          (and-let* ((v (if (null? (cdr p))
                                          '()
                                          (val (cadr p)))))
                            (cons (x->string (car p)) v)))
                        kargs))))

;;-----------------------------------------------------------
;; Pre-defined element handlers
;;

;;
;; a/cont element - embeds continuation within a link node.
;;
;; `(a/cont (@@ (cont ,closure [arg ...]) (fragment ,id))
;;          contents)
;;
;;  With arg ..., you can pass parameters to the continuation.
;;  Each arg should either be:
;;
;;    <value> : passed as a positional argument (via PATH_INFO).
;;    (<symbol> <value>) : passed as a keyword argument. (via QUERY_STRING).
;;
;;  Where <value> is either a string, symbol, or a number.
;;  (x->string is applied on <value>).
;;
;;  Example: (a/cont (@@ (cont ,closure show time (id 40))) contents)
;;   => <a href='kahua.cgi/app-type/closure/show/time?id=40'>contents</a>
;;
;;  If (fragment <id>) is given, <id> is used as a fragment ID of
;;  the generated URL.  <id> isn't passed to the continuation closure;
;;  it is used by the client browser to jump to the specified fragment.
;;
;;  A variation of 'cont' can be used to pass the control to other
;;  application server:
;;
;; `(a/cont (@@ (remote-cont <server-type> <cont-id> [arg ...]) 
;;              (fragment ,id)
;;              (return-cont ,closure [arg ...]))
;;          contents)
;;
;;  This will produce a URL kahua.cgi/<server-type>/<cont-id>..., so
;;  that the control is transferred to other application server
;;  specified by <server-type> and <cont-id>.
;;
;;  If 'return-cont' clause is also given, it is used to generate a keyword
;;  argument 'return-cont' for the remote call, so that the remote server
;;  can pass the control back to the current server.
;;
;;  NB: it would be nice if a/cont can specify arbitrary number of
;;  return continuations, so that the remote server can choose the return
;;  point depending on its operation.  Future extension.

(define-element a/cont (attrs auxs contents context cont)

  (define (build-argstr pargs kargs)
    (string-concatenate
     `(,(string-join (map uri-encode-string pargs) "/" 'prefix)
       ,@(if (null? kargs)
           '()
           `("?"
             ,(string-join
               (map (lambda (karg)
                      (if (null? (cdr karg))
                        (uri-encode-string (car karg))
                        (format "~a=~a"
                                (uri-encode-string (car karg))
                                (uri-encode-string (cdr karg)))))
                    kargs)
               "&"))))))

  (define (fragment auxs)
    (cond ((assq-ref auxs 'fragment)
           => (lambda (p) #`"#,(uri-encode-string (car p))"))
          (else "")))

  (define (local-cont clause)
    (let ((id     (session-cont-register (car clause)))
          (argstr ((compose build-argstr extract-cont-args)
                   (cdr clause) 'a/cont)))
      (nodes (kahua-self-uri #`",|id|,|argstr|,(fragment auxs)"))))

  (define (return-cont-uri)
    (and-let* ((clause (assq-ref auxs 'return-cont))
               (id     (session-cont-register (car clause)))
               (argstr ((compose build-argstr extract-cont-args)
                        (cdr clause) 'a/cont)))
      (format "~a/~a~a" (kahua-worker-type) id argstr)))

  (define (remote-cont clause)
    (let* ((server-type (car clause))
           (cont-id (cadr clause))
           (return  (return-cont-uri))
           (argstr  (receive (pargs kargs)
                        (extract-cont-args (cddr clause) 'a/cont)
                      (build-argstr pargs
                                    (if return
                                      `(("return-cont" . ,return) ,@kargs)
                                      kargs)))))
      (nodes (format "~a/~a/~a~a"
                     (kahua-bridge-name) server-type cont-id argstr))))

  (define (nodes path)
    (cont `((a (@ ,@(cons `(href ,path)
                          (remove (lambda (x)
                                    (eq? 'href (car x))) attrs)))
               ,@contents)) context))

  (cond ((assq-ref auxs 'cont) => local-cont)
        ((assq-ref auxs 'remote-cont) => remote-cont)
        (else (nodes (kahua-self-uri (fragment auxs)))))
  )

;;
;; form/cont
;; 
;; `(form/cont (@@ (cont ,closure [arg ...])) contents)
;;
;;  Where arg ... is like a/cont, except you can omit the value of
;;  keyword arguments.  If so, the value of the form's QUERY_STRING
;;  is taken.

(define-element form/cont (attrs auxs contents context cont)

  (define (build-argstr&hiddens cont-args)
    (receive (pargs kargs) (extract-cont-args cont-args 'form/cont)
      (cons
       (string-join (map uri-encode-string pargs) "/" 'prefix)
       (filter-map (lambda (karg)
                     (and (not (null? (cdr karg)))
                          `(input (@ (type "hidden") (name ,(car karg))
                                     (value ,(cdr karg))))))
                   kargs))))
  
  (let* ((clause (assq-ref auxs 'cont))
         (id     (if clause (session-cont-register (car clause)) ""))
         (argstr (if clause (build-argstr&hiddens (cdr clause)) '(""))))
    (cont
     `((form (@ ,@(append `((method "POST") 
                              (action ,(kahua-self-uri 
                                        (string-append id (car argstr)))))
                            (remove (lambda (x)
                                      (or (eq? 'method (car x))
                                          (eq? 'action (car x)))) attrs)))
             ,@(cdr argstr)
             ,@contents))
     context)))

;;
;; frame/cont
;;
;; `(frame/cont (@@ (cont ,closure [arg ...])))
;;

(define-element frame/cont (attrs auxs contents context cont)
  (let* ((clause (assq-ref auxs 'cont))
         (id     (if clause (session-cont-register (car clause)) "")))
    (cont `((frame (@ ,@attrs (src ,(kahua-self-uri id))))) context)))

;;
;; extra-header - inserts protocol header to the reply message
;; 
;; `(extra-header (@ (name ,name) (value ,value)))
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


;;==========================================================
;;  SXML tree interpreter - generates PDF
;;
;; interp-pdf :: Node -> Context -> Stree
(define (interp-pdf nodes context cont)
  (let ((data (reverse (map reverse-lines
                            (boxes-of-state
                             (exec/state (make-state 0 0 #t '() '())
                                         (interp-html-pdf nodes))))))
        (port (open-output-string)))
    (with-docdata-to-port port (lambda () data))

    ;;for extra headers
    (receive (stree context)
        (interp-html-rec nodes context cont)
      (let1 headers (assoc-ref-car context "extra-headers" '())
        (cont
         (list (get-output-string port))
         (if (assoc "content-type" headers)
           context
           (cons `("extra-headers"
                   ,(kahua-merge-headers
                     headers '(("content-type" "application/pdf"))))
                 context)))))
    ))

(add-interp! 'pdf interp-pdf)

;;===========================================================
;; SXML tree interpreter - for RSS
;;
;; interp-rss :: Node -> Context -> Stree

(define interp-rss
  (let1 enc (symbol->string (gauche-character-encoding))
        (lambda (nodes context cont)
          (receive (stree context)
             (interp-html-rec-bis nodes context cont)
             (values
              ;; Stree
              (cons #`"<?xml version=\"1.0\" encoding=\",|enc|\" ?>\n"
                    stree)
              ;; Context
              (let1 headers (assoc-ref-car context "extra-headers" '())
                    (if (assoc "content-type" headers)
                        context
                        (cons `("extra-headers"
                                ,(kahua-merge-headers
                                  headers 
                                  `(("content-type" 
                                     ,#`"text/xml; charset=,|enc|"
                                     ))))
                              context))))))))

(add-interp! 'rss interp-rss)

;;===========================================================
;; SXML tree interpreter - for XML
;;
;; interp-xml :: Node -> Context -> Stree
;;
;; This is copied and a little modify form interp-rss.
;; From this reason, the interp is not satisfy for XML interp.
;;
(define interp-xml
  (let1 enc (symbol->string (gauche-character-encoding))
        (lambda (nodes context cont)
          (receive (stree context)
              ;; (cadr nodes) deletes xml symbol tag.
              ;;
             (interp-html-rec-bis (cadr nodes) context cont)
             (values
              ;; Stree
              (cons #`"<?xml version=\"1.0\" encoding=\",|enc|\" ?>\n"
                    stree)
              ;; Context
              (let1 headers (assoc-ref-car context "extra-headers" '())
                    (if (assoc "content-type" headers)
                        context
                        (cons `("extra-headers"
                                ,(kahua-merge-headers
                                  headers 
                                  `(("content-type" 
                                     ,#`"text/xml; charset=,|enc|"
                                     ))))
                              context))))))))

(add-interp! 'xml interp-xml)

(provide "kahua/server")
