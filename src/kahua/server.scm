;; Common server operations
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

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
  (use text.tree)
  (use gauche.parameter)
  (use gauche.sequence)
  (use gauche.charconv)
  (use gauche.logger)
  (use rfc.uri)
  (use util.list)
  (use util.match)
  (use sxml.tools)
  (use sxml.adaptor)
  (use file.util)
  (use kahua.gsid)
  (use kahua.partcont)
  (use kahua.session)
  (use kahua.persistence)
  (use kahua.user)
  (use kahua.util)
  (use kahua.elem)
  (use kahua.config)
  (use kahua.protocol.worker)
  (extend kahua.css)
  (export kahua-init-server
          kahua-bridge-name
          kahua-server-uri
          kahua-self-uri
          kahua-self-uri-full
          kahua-default-handler
          kahua-current-context
          kahua-context-ref
          kahua-meta-ref
	  kahua-cookie-ref
          kahua-context-ref*
	  kahua-local-session-ref
	  kahua-local-session-set!
	  define-session-object
          kahua-current-entry-name
          kahua-current-user
	  with-kahua-user
	  with-kahua-local-user
          kahua-current-user-name
	  kahua-login
	  kahua-logout
	  kahua-authorized?
          kahua-worker-type
          kahua-merge-headers
          kahua-header-set!
          not-accessible?
          add-element!
          define-element
          define-entry
          define-entry-method
          apply-entry-method
          define-method-rule
	  kahua-call-with-current-context
	  kall/cc
          path->objects
          entry-lambda
          interp-html
	  interp-html-rec
          redirect/cont
          kahua-render
          <json-base>
          x->json

	  with-validation
          )
  )
(select-module kahua.server)

;; internally keep worker-id
(define worker-id (make-parameter "dummy"))

;; internally keep worker-type
(define worker-type (make-parameter "dummy"))
(define (kahua-worker-type)
        (worker-type))

;; internally keep explicitly specified worker uri
(define (worker-uri)
  (or (kahua-worker-uri)
      (kahua-context-ref "x-kahua-worker-uri")))

;; Context
;;  A context is established every time the control is passed from
;;  the client to the server.  It is available as a parameter
;;  kahua-current-context.

(define kahua-current-context (make-parameter '()))

;; KAHUA-INIT-SERVER worker-type [session-server-id]
;;   Application server should use it within init-server procedure.
;;   Returns worker id.
(define (kahua-init-server wtype ssid)
  (random-source-randomize! default-random-source)
  (let ((wid (make-worker-id wtype)))
    (session-manager-init wid ssid)
    (worker-type wtype)
    (worker-id wid)
    wid))

;; KAHUA-BRIDGE-NAME
;;   A parameter that holds the name of cgi bridge.
;;   App server shouldn't set this value.   Cgi-bridge tells
;;   its name to kahua-server, and KAHUA-DEFAULT-HANDLER will
;;   set it to this parameter.
(define kahua-bridge-name (make-parameter "/kahua.cgi")) ;; dummy

;; KAHUA-SERVER-URI
;;   A parameter that holds the server URI (scheme://server:port).
;;   The cgi-bridge tells this info to the app server.
(define kahua-server-uri (make-parameter "http://localhost")) ;; dummy

(define kahua-worker-uri (make-parameter #f))

;; KAHUA-SELF-URI path ...
;; KAHUA-SELF-URI-FULL path ...
;;   Generates a self-referencing uri.  arguments has to be uriencoded.

(define (kahua-self-uri . paths)
  (path-info->abs-path
   (simplify-path-info
    (cond ((worker-uri) =>
	   (lambda (w) (append! (string-split w #\/) paths)))
	  (else
	   (append! (string-split (kahua-bridge-name) #\/)
		    (cons (kahua-worker-type) paths)))))))

(define (kahua-self-uri-full . paths)
  (string-append (kahua-server-uri) (apply kahua-self-uri paths)))

(define (kahua-session-domain-uri)
  (define (drop-worker-name paths)
    (cond ((null? paths) (string-append (kahua-server-uri) "/"))
	  (else (string-append (kahua-server-uri)
			       (path-info->abs-path
				(let1 paths (reverse! paths)
				  (if (string=? (car paths) (kahua-worker-type))
				      (reverse! (cdr paths))
				      (reverse! paths))))))))
  (define (site-domain-uri)
    (cond ((path->path-info (kahua-worker-uri)) => drop-worker-name)
	  (else
	   (string-append (kahua-server-uri)
			  (path-info->abs-path
			   (simplify-path-info
			    (append! (string-split (kahua-bridge-name) #\/)
				     '(".."))))))))
  (define (bridge-domain-uri)
    (cond ((path->path-info (kahua-worker-uri)) => drop-worker-name)
	  ((kahua-bridge-name) (compose not string-null?)
	   => (cut string-append (kahua-server-uri) <>))
	  (else (string-append (kahua-server-uri) "/"))))
  (case (kahua-session-domain)
    ((:site) (site-domain-uri))
    ((:bridge) (bridge-domain-uri))
    ((:worker) (kahua-self-uri-full))
    (else => (lambda (d) (and (string? d) d)))))

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
                       (stale-proc kahua-default-stale-proc)
                       (eval-proc  kahua-eval-proc)
                       (eval-environment (find-module 'user))
                       (error-proc kahua-default-error-proc))

    ;; (Handler, Context) -> (Stree, Context)
    (define (run-cont handler context)
      (parameterize ((kahua-current-context context))
	(guard (e (else
		   (raise-with-db-error e)
		   (guard (e2 (else (render-proc (kahua-default-error-proc e) context)))
		     (render-proc (error-proc e) context))))
	  (render-proc (reset/pc (handler)) context))))

    ;; Handles 'eval' protocol
    ;; () -> ([Headers], [Result])
    (define (run-eval state)
      (parameterize ((kahua-current-context `(("session-state" ,state))))
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
				       result)))))))

    (define (make-context state header body)
      `(("session-state" ,state)
	,@(map (lambda (hdr)
		 (receive (k v) (car+cdr hdr)
		   (cond ((string=? "x-kahua-path-info" k)
			  (list "x-kahua-path-info" (drop* (car v) 2)))
			 (else hdr))))
	       header)
	("x-kahua-headers" ,(make-hash-table 'string=?))
	,@body))
     
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
		       (kahua-worker-uri
			(assoc-ref-car header "x-kahua-worker-uri"
				       (kahua-worker-uri))))
          (if (assoc-ref-car header "x-kahua-eval" #f)
	      (receive (headers result) (run-eval state)
		(lambda ()
		  (reply-cont headers result)))
	      (receive (stree context)
		  (run-cont (if cont-id
				(or (session-cont-get cont-id) stale-proc)
				default-proc)
			    (make-context state header body))
		(let1 session-domain-uri (kahua-session-domain-uri)
		  (lambda ()
		    (reply-cont
		     (kahua-merge-headers (cons `("x-kahua-session-domain" ,session-domain-uri)
						(alist-delete "x-kahua-metavariables" header))
					  (assoc-ref-car context "extra-headers" '())
					  (hash-table-map
					      (assoc-ref-car context "x-kahua-headers" '())
					    list))
		     stree))))))))
    ))

;; default stale proc
(define (kahua-default-stale-proc)
  `((html
     (extra-header (@ (name "Status") (value "404 Not Found")))
     (head (title "Kahua error"))
     (body (h1 "Kahua error - stale session key")
	   (p "The given session key is wrong, or expired.")))))

;; default error proc
(define (kahua-default-error-proc e)
  `((html
     (extra-header (@ (name "Status") (value "500 Internal Server Error")))
     (head (title "Kahua error"))
     (body (pre ,(html-escape-string (kahua-error-string e #t)))))))

;; default eval proc
(define (kahua-eval-proc body env)
  (guard (e (else
	     (raise-with-db-error e)
	     (values #f (kahua-error-string e #t))))
    (receive r (eval body env)
      (values #t
	      (map (cut write-to-string <>) r)))))

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

(define (kahua-render nodes context)
  (tree->string (kahua-render-proc nodes context)))

;; KAHUA-CONTEXT-REF key [default]
;;
;;  Context is a list of lists, and a key may be a string or a symbol
;;  so it's not just as simple as an alist.

(define (kahua-context-ref key . maybe-default)
  (apply assoc-ref-car (kahua-current-context) key maybe-default))

;; KAHUA-META-REF key [default]
;;
;;  Gets CGI metavaliable.
;;  Key should be in upper case plus underscore.

(define (kahua-meta-ref key . maybe-default)
  (apply assoc-ref-car
         (kahua-context-ref "x-kahua-metavariables" '()) key maybe-default))


(define (kahua-header-set! key val)
  (hash-table-put! (kahua-context-ref "x-kahua-headers")
                   key val))

;; KAHUA-COOKIE-REF key [default]
;;
;; Gets CGI metavariable as HTTP_COOKIE
;; and get cookie's key value
;;
(define (kahua-cookie-ref key . maybe-default)
  (let1 maybe-default (get-optional maybe-default #f)
    (let ((regex (string->regexp #`",|key|=([^\; ]*)")))
      (cond ((and-let* ((cookie (kahua-meta-ref "HTTP_COOKIE"))
			(match (regex cookie)))
	       match) => (cut <> 1))
	    (else maybe-default)))))

;; KAHUA-CONTEXT-REF* key [default]
;;
;;  Like kahua-context-ref, but retrieves multiple values as a list.
;;  It returns '() if the data corresponding to the key isn't found.
;;  But at GET REQUEST, It returns #f.

(define (kahua-context-ref* key . maybe-default)
  (assoc-ref (kahua-current-context) key (get-optional maybe-default '())))

;; KAHUA-LOCAL-SESSION-REF var
;;
;; var is symbol, which is any slot name of <session-state>.
;;
(define kahua-local-session-ref
  (let1 get-session
      (lambda ()
	(kahua-context-ref "session-state"))
    (getter-with-setter
     (lambda (key) (ref (get-session) key))
     (lambda (key val) (set! (ref (get-session) key) val)))))

;; KAHUA-LOCAL-SESSION-SET! var val
;;
;; var is symbol, which is any slot name of <session-state>.
;; and val is permitted every value include non-persistent object.
;; the value object is local object in only app server.
;;
(define (kahua-local-session-set! key val)
  (set! (kahua-local-session-ref key) val))

;; DEFINE-SESSION-OBJECT name create
;;
;; name is the object name which we use session object.
;; and init-value is normally make expression.
;; so, the init-value expression is called only first. 
;;
;; it's used as like as parameter.
;;
(define-macro (define-session-object name init-value)
  (let1 get-session (gensym)
    `(define ,name
       (let1 ,get-session
	   (lambda ()
	     (kahua-context-ref "session-state"))
	 (getter-with-setter
	  (lambda () (cond ((ref (,get-session) ',name))
			   (else (set! (ref (,get-session) ',name) ,init-value)
				 (ref (,get-session) ',name))))
	  (lambda (val) (set! (ref (,get-session) ',name) val)))))))
			 

;; KAHUA-LOCAL-SESSION-REF var
;;
;; var is symbol, which is any slot name of <session-state>.
;;
(define kahua-local-session-ref
  (let1 get-session
      (lambda ()
	(kahua-context-ref "session-state"))
    (getter-with-setter
     (lambda (key) (ref (get-session) key))
     (lambda (key val) (set! (ref (get-session) key) val)))))

;; KAHUA-LOCAL-SESSION-SET! var val
;;
;; var is symbol, which is any slot name of <session-state>.
;; and val is permitted every value include non-persistent object.
;; the value object is local object in only app server.
;;
(define (kahua-local-session-set! key val)
  (set! (kahua-local-session-ref key) val))

;; DEFINE-SESSION-OBJECT name create
;;
;; name is the object name which we use session object.
;; and init-value is normally make expression.
;; so, the init-value expression is called only first. 
;;
;; it's used as like as parameter.
;;
(define-macro (define-session-object name init-value)
  (let1 get-session (gensym)
    `(define ,name
       (let1 ,get-session
	   (lambda ()
	     (kahua-context-ref "session-state"))
	 (getter-with-setter
	  (lambda () (cond ((ref (,get-session) ',name))
			   (else (set! (ref (,get-session) ',name) ,init-value)
				 (ref (,get-session) ',name))))
	  (lambda (val) (set! (ref (,get-session) ',name) val)))))))
			 

;; KAHUA-CURRENT-ENTRY-NAME
;;  A parameter that holds the name of entry.

(define kahua-current-entry-name (make-parameter ""))

;; KAHUA-CURRENT-USER
;; (setter KAHUA-CURRENT-USER) user
;;
;;   Gets/sets current user (<kahua-user> object) in the session state
;;   object.   The application must call these procs within with-db's
;;   dynamic extent.

(define (find-login-state dbpath . maybe-include-anon?)
  (and-let* ((session-state (kahua-context-ref "session-state"))
	     (login-states (ref session-state 'login-states)))
    (or (find (lambda (e) (equal? (cdr e) dbpath)) login-states)
	(and (get-optional maybe-include-anon? #f)
	     dbpath
	     (find (lambda (e) (not (cdr e))) login-states)))))

(define (register-login-state login-name dbpath)
  (let* ((login-states (or (ref (kahua-context-ref "session-state") 'login-states) '()))
	 (entry (find (lambda (e) (equal? (cdr e) dbpath)) login-states)))
    (set! (ref (kahua-context-ref "session-state") 'login-states)
	  (cond (entry
		 (if login-name
		     (begin
		       (set-car! entry login-name)
		       login-states)
		     (remove! (lambda (e) (equal? (cdr e) dbpath)) login-states)))
		((string? login-name)
		 (acons login-name dbpath login-states))
		(else login-states)))))

(define kahua-local-current-user (make-parameter #f))
(define kahua-current-user
  (getter-with-setter
   (lambda ()
     (cond ((kahua-local-current-user)
	    => (lambda (u) (and (kahua-user? u) u)))
	   (else
	    (and-let* ((u (find-login-state (path-of (current-db))))
		       (user (kahua-find-user (car u))))
	      (cond ((active? user) user)
		    (else (register-login-state #f (path-of (current-db))) #f))))))
   (lambda (user)
     (let1 u (cond ((kahua-user? user) user)
		   ((string? user) (kahua-find-user user))
		   (else #f))
       (cond ((kahua-local-current-user)
	      (kahua-local-current-user (if (kahua-user? u) u #t)))
	     (else
	      (if (and u (active? u))
		  (register-login-state (ref u 'login-name) (dbpath-of u))
		  (register-login-state #f (path-of (current-db))))))))
   ))

(define (with-kahua-user user proc)
  (let ((old-user (kahua-current-user))
	(new-user (cond ((kahua-user? user) user)
			((string? user) (kahua-find-user user))
			(else #f))))
    (dynamic-wind
	(lambda () (set! (kahua-current-user) new-user))
	(cut proc new-user)
	(lambda () (set! (kahua-current-user) old-user)))))

(define (with-kahua-local-user user proc)
  (parameterize ((kahua-local-current-user #t))
    (with-kahua-user user proc)))

;; KAHUA-CURRENT-USER-NAME
;; (setter KAHUA-CURRENT-USER-NAME) user
;;
;;   Gets/sets current user name (N.B. not <kahua-user> object)
;;   in the session state object.

(define kahua-current-user-name
  (getter-with-setter
   (lambda ()
     (and-let* ((u (find-login-state (and-let* ((db (current-db)))
				       (path-of db))
				     #t)))
       (car u)))
   (lambda (login-name)
     (if (current-db)
	 (set! (kahua-current-user) login-name)
	 (register-login-state login-name #f)))))

;;
;; Simple User Authorization
;;

(define (kahua-login user-name password)
  (and-let* ((u (kahua-check-user user-name password))
	     ((active? u)))
    (set! (kahua-current-user) u)
    u))

(define (kahua-logout)
  (set! (kahua-current-user) #f))

(define (kahua-authorized? . roles)
  (and-let* ((u (kahua-current-user))
	     ((active? u)))
    (or (null? roles)
	(kahua-user-has-role? u roles))))

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

; (define-syntax entry-lambda
;   (syntax-rules ()
;     ;; finishing expansion
;     ((entry-lambda "finish" args pargs kargs #f body)
;      '(make-parameterized-entry-closure 'pargs 'kargs #f
;                                        (lambda args . body)))
;     ((entry-lambda "finish" () pargs kargs rarg body)
;      '(make-parameterized-entry-closure 'pargs 'kargs 'rarg
;                                        (lambda rarg . body)))
;     ((entry-lambda "finish" (args ...) pargs kargs rarg body)
;      '(make-parameterized-entry-closure 'pargs 'kargs 'rarg
;                                        (lambda (args ... . rarg) . body)))
;     ;; collecting positional args
;     ((entry-lambda "pargs" () pargs body)
;      (entry-lambda "finish" pargs pargs () #f body))
;     ((entry-lambda "pargs" (:rest rarg) pargs body)
;      (entry-lambda "finish" pargs pargs () rarg body))
;     ((entry-lambda "pargs" (:rest rarg :keyword . syms) pargs body)
;      (entry-lambda "kargs" syms pargs pargs () rarg body))
;     ((entry-lambda "pargs" (:rest . _) pargs body)
;      (syntax-error "malformed entry-lambda :rest form:" (:rest . _)))
;     ((entry-lambda "pargs" (:keyword . syms) pargs body)
;      (entry-lambda "kargs" syms pargs pargs () #f body))
;     ((entry-lambda "pargs" (sym . syms) (parg ...) body)
;      (entry-lambda "pargs" syms (parg ... sym) body))
;     ;; collecting keyword args
;     ((entry-lambda "kargs" () args pargs kargs rarg body)
;      (entry-lambda "finish" args pargs kargs rarg body))
;     ((entry-lambda "kargs" (:rest rarg) args pargs kargs #f body)
;      (entry-lambda "finish" args pargs kargs rarg body))
;     ((entry-lambda "kargs" (:rest rarg) args pargs kargs rarg1 body)
;      (syntax-error "duplicate :rest args in entry-lambda:" (:rest rarg)))
;     ((entry-lambda "kargs" (:rest . _) args pargs kargs rarg1 body)
;      (syntax-error "malformed entry-lambda :rest form:" (:rest . _)))
;     ((entry-lambda "kargs" (sym . syms) (arg ...) pargs (karg ...) rarg body)
;      (entry-lambda "kargs" syms (arg ... sym) pargs (karg ... sym) rarg body))
;     ;; initial entry
;     ((entry-lambda args body1 . bodies)
;      (entry-lambda "pargs" args () (body1 . bodies)))
;     ;; error handling
;     ((entry-lambda . _)
;      (syntax-error "malformed entry-lambda:" (entry-lambda . _)))
;     ))

;
; entry-lambda
;
(define-macro (entry-lambda args body1 . bodys)
  ;; helper
  (define (parse-args args)
    (define (dispatch proc args ps ks ms rs)
      (match args
	     ;; illegal case reject
	     ((:rest (? keyword? bad) . more)
	      (error "malformed entry-lambda :rest form:" (list key bad)))
	     (((? keyword? tail))
	      (error "malformed entry-lambda keyword tail form:" tail))
	     ;; legal case
	     (() (values (reverse ps) (reverse ks) (reverse ms) rs))
	     ((:keyword var . more)
	      (dispatch parse-key (cdr args) ps ks ms rs))
	     (((or :multi-value-keyword :mvkeyword) var . more)
	      (dispatch parse-mvkey (cdr args) ps ks ms rs))
	     ((:rest . vars)
	      (dispatch parse-rest (cdr args) ps ks ms rs))
	     (((? symbol? var) .  more)
	      (proc args ps ks ms rs))
	     (else (error "malformed entry-lambda form:" (cdr args)))))

    (define (parse-path args ps ks ms rs)
      (dispatch parse-path (cdr args) (cons (car args) ps) ks ms rs))
    (define (parse-key args ps ks ms rs)
      (dispatch parse-key (cdr args) ps (cons (car args) ks) ms rs))
    (define (parse-mvkey args ps ks ms rs)
      (dispatch parse-mvkey (cdr args) ps ks (cons (car args) ms) rs))
    (define (parse-rest args ps ks ms rs)
      (if rs
	  (error "malformed entry-lambda :rest form:" `(:rest ,args))
	  (dispatch parse-rest (cdr args) ps ks ms (car args))))

    (dispatch parse-path args () () () #f))

  (receive (ps ks ms rs) (parse-args args)
    `(,make-parameterized-entry-closure
      ',ps ',ks ',ms ',rs (lambda (,@ps ,@ks ,@ms . ,rs) ,body1 ,@bodys))))


;; helper procedure
(define (make-parameterized-entry-closure pargs kargs mvkargs rarg proc)
  (let ((kargs-str (map x->string kargs))
	(mvkargs-str (map x->string mvkargs)))
    (lambda ()
      (let1 path-info (kahua-context-ref "x-kahua-path-info" '())
        (apply proc
               (append (map-with-index (lambda (ind parg)
                                         (list-ref path-info ind #f))
                                       pargs)
                       (map kahua-context-ref kargs-str)
		       (map kahua-context-ref* mvkargs-str)
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
       (let* ((closure expr)
              (x (lambda ()
                   (parameterize
                    ((kahua-current-entry-name (symbol->string 'name)))
                    (closure)))))
         (add-entry! 'name x)
         x)))
    ))

;; entry-method specializer
(define-class <entry-method> ()())
(define entry-specializer (make <entry-method>))

;; helper syntax

(define-class <rule-generic> (<generic>)
  ((rules :init-form (make-hash-table 'equal?))))

(define-method write-object ((obj <rule-generic>) port)
  (format port "#<rule-generic ~a>" (ref obj 'name)))

;; ((arg1 <class1>) arg2 ...)
;; => (<class1> <top> ...)
(define (gen-specializers specs)
  (map (lambda (spec)
         (cond ((pair? spec)
                (cadr spec))
               (else
                '<top>))) specs))

;; ((arg1 <class1>) "str1" arg3 ...)
;; => ((arg1 <class1>) G93 arg3 ...)
(define (gen-method-args specs)
  (map (lambda (spec)
         (cond ((string? spec) (gensym))
               (else spec)))
       specs))

;; ((arg1 <class1>) G93 arg3 ...)
;; => (arg1 G93 arg3 ...)
(define (gen-lambda-args specs)
  (map (lambda (spec)
         (cond ((pair? spec) (car spec))
               (else spec)))
       specs))

;; ((arg1 <class1>) "str1" "str2" ...)
;; => ((1 . "str1") (2 . "str2") ...)
(define (gen-rules specs)
  (reverse (fold-with-index
            (lambda (idx a b)
              (if (string? a)
                  (acons idx a b)
                b)) '() specs)))


(define (apply-rule rules args)
  (let loop ((rules rules))
    (if (null? rules)
        (error "no applicable rule")
      (let ((rule (caar rules))
            (proc (cdar rules)))
        (let loop2 ((rule rule))
          (if (or (null? rule)
                  (not (list? rule)))
              (apply proc args)
            (let ((pos (caar rule))
                  (val (cdar rule)))
              (if (equal? val
                          (list-ref args pos))
                  (loop2 (cdr rule))
                (loop (cdr rules))))))))))

(define (sort-applicable-rules rules)
  (sort rules (lambda (x y) (> (length (car x))
                                  (length (car y))))))

(define-values (add-rule! rule-exist?)
  (let1 rule-list '()
    (values
     (lambda (rule)
       (push! rule-list rule))
     (lambda (rule)
       (memq rule rule-list)))))

(define-macro (define-method-rule name args . body)
  (let* ((rarg (if (dotted-list? args) (cdr (last-pair args)) '()))
         (args (map identity args))
         (specs (append (gen-specializers args) (if (null? rarg) rarg 'rarg)))
         (rules (append (gen-rules args) rarg))
         (method-args (append (gen-method-args args) rarg))
         (lambda-args (append (gen-lambda-args method-args) rarg)))
    `(begin
       ,(unless (rule-exist? name)
                (add-rule! name)
                `(define-generic ,name :class ,<rule-generic>))
       (hash-table-update! (ref ,name 'rules) ',specs
                           (lambda (val)
                             (,sort-applicable-rules
                              (acons ',rules (lambda ,lambda-args ,@body)
                                     val)))
                           '())
       (define-method ,name ,method-args
         (let1 rules (hash-table-get (ref ,name 'rules) ',specs)
           (,apply-rule rules (append (list ,@(map identity lambda-args)) ,rarg)))))))

;;
;; kahua-call-with-current-context
;;
;
; [sample code]
;
; (define (callee return)
;   (html/
;    (body/
;     (h1/ "callee")
;     (a/cont/ (@@/ (cont return))
; 	     "return caller"))))
;
; (define-entry (caller)
;   (kahua-call-with-current-context
;    (lambda (self)
;      (html/
;       (body/
;        (h1/ (sys-time))
;        (h2/ (or (kahua-context-ref "query") ""))
;        (a/cont/ (@@/ (cont (lambda ()
; 			     (callee self))))
; 		"call"))))))
;
(define (kahua-call-with-current-context proc)
  (letrec ((ctxt (kahua-current-context))
	   (return (lambda ()
		     (proc (lambda ()
			     (parameterize
				 ((kahua-current-context ctxt))
			       (return)))))))
    (return)))
;; Alias
(define kall/cc kahua-call-with-current-context)

(define-syntax regist-entry-method
  (syntax-rules ()
    ((_ name)
     (begin
       (define-method name ()
         (parameterize ((kahua-current-entry-name (symbol->string 'name)))
           (let1 path (kahua-context-ref "x-kahua-path-info" '())
             (apply name entry-specializer (path->objects path)))))
       (add-entry! 'name name)))))

(define (apply-entry-method gf . args)
  (apply gf entry-specializer args))

;;  [syntax] define-entry-method name (arg ... :keyword karg ... :rest restarg) body ...

(define-syntax define-entry-method
  (syntax-rules ()
    ((_ "finish" name pargs kargs body)
     (begin
       (define-method-rule name pargs
         ((entry-lambda kargs
            . body)))
       (regist-entry-method name)))
    ;; add entry specializer
    ((_ "specilize" name pargs kargs body)
     (define-entry-method "finish" name ((_ <entry-method>) . pargs) kargs body))
    ;; collecting positional args
    ((_ "pargs" name () pargs body)
     (define-entry-method "specilize" name pargs () body))
    ((_ "pargs" name (:rest sym) (parg1 ...) body)
     (define-entry-method "specilize" name (parg1 ... . sym) (:rest sym) body))
    ((_ "pargs" name (:keyword . syms) pargs body)
     (define-entry-method "specilize" name pargs (:keyword . syms) body))
    ((_ "pargs" name (:multi-value-keyword . syms) pargs body)
     (define-entry-method "specilize" name pargs (:multi-value-keyword . syms) body))
    ((_ "pargs" name (:mvkeyword . syms) pargs body)
     (define-entry-method "specilize" name pargs (:mvkeyword . syms) body))
    ((_ "pargs" name (sym . syms) (parg ...) body)
     (define-entry-method "pargs" name syms (parg ... sym) body))
    ;; initial entry
    ((_ name args body1 . bodies)
     (define-entry-method "pargs" name args () (body1 . bodies)))
    ;; error handling
    ((_ . _)
     (syntax-error "malformed define-entry-method:" (define-entry-method . _)))
    ))

(define *class&id-delim* "\t")

(define (path->objects paths)
  (define (uri->object uri)
    (let1 class&id (string-split uri *class&id-delim*)
      (find-kahua-instance
       (find-kahua-class (string->symbol (car class&id)))
       (cadr class&id))))

  (map (lambda (uri)
         (if (string-scan uri *class&id-delim*)
             (uri->object uri)
           uri))
       paths))

(define (add-entry! name proc)
  (session-cont-register proc (symbol->string name)))

;;==========================================================
;;  Default SXML tree interpreter - generates HTML
;;

(define *namespace-prefix-table* (make-parameter '()))

;; interp-html :: Node -> Context -> Stree
;;    where Stree is a list of string segments passed to tree->string.
;;
;; Maybe dead code.
(define (interp-html nodes context)
  (interp-html-rec (car nodes) context (lambda (s _) s)))

;; internal loop
(define (interp-html-rec-gen default-handler)
  (lambda (node context cont)
    (let ((name (sxml:element-name node)))
      (if (not name)
          (cont (cond ((string? node)
                       (sxml:string->xml-bis node))
                      ((no-escape? node)
                       (ref node 'src))
                      (else ""))
                context)
          (let ((attrs (sxml:attr-list-u node))
                (auxs  (sxml:aux-list-u node))
                (contents (sxml:content node)))
            (if (not-accessible? auxs context)
                (cont "" context)
                (cond ((and (not (assq 'expand-finished auxs))
			    (get-element-handler name))
                       => (cut <> name attrs auxs contents context
                               (lambda (nds cntx)
                                 (handle-element-contents nds cntx cont))))
                      (else 
                       (default-handler name attrs contents context cont)))
                )))
      )))

;; customized version of sxml:attr->xml
;;  - omits attributes with value #f
(define (sxml:attr->xml-bis attr)
  (or (and-let* ((value (cadr attr)))
	(list " " (sxml:name attr) "='"
	      (sxml:string->xml-bis (x->string value)) "'"))
      ""))

;; Internet Explorer cannot recognize &apos;
;; So we cannot use sxml:string->xml itself :-(
(define sxml:string->xml-bis
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") 
     (#\" . "&quot;") (#\' . "&#39;"))))

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

;;
;; interp-xhtml - This is very transitional code.
;;

(define (interp-xhtml node context cont)
  (define (extract-namespaces node)
    (cond ((assq '*NAMESPACES* node)
	   => (lambda (node)
		(map (lambda (e)
		       (list (or (and-let* ((prefix (car e)))
				   #`"xmlns:,|prefix|")
				 "xmlns")
			     (cadr e)))
		     (cdr node))))
	  (else '())))
  (define (extract-xml-decl node)
    (if (eq? (car node) 'xml)
	(list (apply format "<?~a ~a?>\n" node))
	'()))
  (define (extract-doctype node)
    (if (eq? (car node) 'html)
	(list (apply format "<!DOCTYPE ~a\n PUBLIC ~s\n ~s>\n" node))
	'()))
  (define (interp-xhtml-prologue node)
    (let loop ((node node)
	       (ns-list '())
	       (xml-decl '())
	       (doctype '()))
      (if (null? node)
	  (values node ns-list xml-decl doctype)
	  (let1 e (car node)
	    (cond ((eq? e '*TOP*) (loop (cdr node) ns-list xml-decl doctype))
		  ((pair? e)
		   (let1 name (car e)
		     (case name
		       ((@@)        (loop (cdr node) (extract-namespaces (cdr e)) xml-decl doctype))
		       ((*PI*)      (loop (cdr node) ns-list (extract-xml-decl (cdr e)) doctype))
		       ((*DOCTYPE*) (loop (cdr node) ns-list xml-decl (extract-doctype (cdr e))))
		       ((html)      (values e ns-list xml-decl doctype))
		       (else        (values node ns-list xml-decl doctype))))))))))
  (define (html-root-handler attrs content context cont)
    (handle-element-contents
     content context
     (lambda (stree context)
       (cont `("<html"
	       ,(map sxml:attr->xml-bis (append (*namespace-prefix-table*) attrs))
	       ">"
	       ,@stree
	       "</html\n>")
	     context))))

  (let*-values (((node ns-list xml-decl doctype) (interp-xhtml-prologue node))
		((stree context) (parameterize ((*namespace-prefix-table* ns-list))
				   (let ((attrs (sxml:attr-list-u node))
					 (contents (sxml:content node)))
				     (html-root-handler attrs contents context cont)))))
    (cont `(,@xml-decl ,@doctype ,stree) context)))

(add-interp! '*TOP* interp-xhtml)

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
    ((_ name proc)
     (add-element! 'name proc))
    ;; for backward compatibility
    ((_ name (attrs auxs contents context cont) . body)
     (define-element name (_ attrs auxs contents context cont) . body))
    ;; new syntax
    ((_ name (n attrs auxs contents context cont) . body)
     (add-element! 'name (lambda (n attrs auxs contents context cont) . body)))
    ((_ . _)
     (syntax-error "malformed define-element"))))

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

  (define (obj->uri obj)
    (format "~a~a~a"
            (class-name (class-of obj))
            *class&id-delim*
            (key-of obj)))

  (define (canonicalize v)
    (define stringnizable? (any-pred string? symbol? number?))
    (cond ((not v) #f) ;; giving value #f makes keyword arg omitted
          ((kahua-persistent-base? v) (obj->uri v))
          ((stringnizable? v)         (x->string v))
          (else (errorf "bad continuation argument ~a in element ~a" v form))))
  (receive (kargs pargs) (partition pair? args)
    (values (map canonicalize pargs)
            (filter-map (lambda (p)
                          (and-let* ((v (if (null? (cdr p))
					    '()
					    (let1 vlist (filter-map canonicalize (cdr p))
					      (and (pair? vlist) vlist)))))
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

;; utils
(define (build-argstr pargs kargs)
  (define delimit-/ (cut write-char #\/))
  (define delimit-? (cut write-char #\?))
  (define delimit-& (cut write-char #\&))
  (with-output-to-string
    (lambda ()
      (with-port-locking (current-output-port)
	(lambda ()
	  (for-each (lambda (p)
		      (delimit-/)
		      (display (uri-encode-string p)))
		    pargs)
	  (fold (lambda (karg delimit)
		  (let ((key (uri-encode-string (car karg)))
			(vlist (map uri-encode-string (cdr karg))))
		    (delimit)
		    (if (null? vlist)
			(display key)
			(fold (lambda (v delimit)
				(delimit) (format #t "~a=~a" key v)
				delimit-&)
			      (lambda _ #f)
			      vlist)))
		  delimit-&)
		delimit-?
		kargs))))))

(define (remove-attrs attrs . names)
  (remove (lambda (x) (memq (car x) names)) attrs))

(define (fragment auxs)
  (cond ((assq-ref auxs 'fragment)
         => (lambda (p)
	      (cond ((null? p) "")
		    ((car p) =>
		     (lambda (f)
		       (format "#~a" (uri-encode-string (x->string f)))))
		    (else      ""))))
        (else "")))

(define (local-cont auxs . maybe-params-proc)
  (lambda (clause)
    (receive (pargs kargs) (extract-cont-args (cdr clause) 'a/cont)
      (let* ((id (session-cont-register (car clause)))
	     (params-proc (get-optional maybe-params-proc #f))
	     (argstr (build-argstr pargs (if params-proc '() kargs)))
	     (uri (kahua-self-uri #`",|id|,|argstr|,(fragment auxs)")))
	(if params-proc
	    (values uri (params-proc kargs))
	    uri)))))

(define (remote-cont auxs . maybe-params-proc)
  (lambda (clause)
    (define (return-cont-uri)
      (and-let* ((clause (assq-ref auxs 'return-cont))
                 (id     (session-cont-register (car clause)))
                 (argstr ((compose build-argstr extract-cont-args)
                          (cdr clause) 'a/cont)))
        (format "~a/~a~a" (kahua-worker-type) id argstr)))

    (match clause
      (((? string? ret)) (kahua-self-uri ".." ret))
      (else (receive (pargs kargs) (extract-cont-args (cddr clause) 'a/cont)
	      (let* ((server-type (car clause))
		     (cont-id (cadr clause))
		     (return  (return-cont-uri))
		     (kargs (if return (cons `("return-cont" ,return) kargs) kargs))
		     (params-proc (get-optional maybe-params-proc #f))
		     (argstr (build-argstr pargs (if params-proc '() kargs)))
		     (uri (kahua-self-uri ".." (x->string server-type)
					  #`",|cont-id|,|argstr|,(fragment auxs)")))
		(if params-proc
		    (values uri (params-proc kargs))
		    uri)))))))

(define (%a/cont-handler _ attrs auxs contents context cont)
  (define (auxs->uri auxs)
    (cond ((assq-ref auxs 'cont)        => (local-cont  auxs))
	  ((assq-ref auxs 'remote-cont) => (remote-cont auxs))
	  (else                            (kahua-self-uri (fragment auxs)))))

  (define (nodes href)
    (cont `((a (@@ (expand-finished))
	       (@ ,href ,@(remove-attrs attrs 'href))
               ,@contents)) context))

  (nodes (or (assq 'href attrs) `(href ,(auxs->uri auxs)))))

(define-element a/cont %a/cont-handler)
(define-element a      %a/cont-handler)

;;
;; form/cont
;; 
;; `(form/cont (@@ (cont ,closure [arg ...])) contents)
;;
;;  Where arg ... is like a/cont, except you can omit the value of
;;  keyword arguments.  If so, the value of the form's QUERY_STRING
;;  is taken.

(define (%form/cont-handler name attrs auxs contents context cont)
  (define (kargs->hiddens kargs)
    (fold-right (lambda (karg accum)
		  (if (null? (cdr karg))
		      accum		; is this right?
		      (fold-right (lambda (v accum)
				    (cons `(input (@ (type "hidden")
						     (name ,(car karg))
						     (value ,v)))
					  accum))
				  accum
				  (cdr karg))))
		'()
		kargs))
  (define (auxs->uri&hiddens auxs)
    (cond ((assq-ref auxs 'cont)        => (local-cont auxs kargs->hiddens))
	  ((assq-ref auxs 'remote-cont) => (remote-cont auxs kargs->hiddens))
	  (else                            (values (kahua-self-uri (fragment auxs)) '()))))
  (define (nodes uri method hiddens)
    (cont `((form (@@ (expand-finished))
		  (@ ,@(list* `(method ,method)
			      `(action ,uri)
			      (remove-attrs attrs 'method 'action)))
		  ,@hiddens
		  ,@contents))
	  context))

  (receive (uri hiddens) (cond ((assq-ref-car attrs 'action)
				=> (cut values <> '()))
			       (else (auxs->uri&hiddens auxs)))
    (nodes uri (or (assq-ref-car attrs 'method) "POST") hiddens)))

(define-element form/cont %form/cont-handler)
(define-element form %form/cont-handler)

;;
;; frame/cont
;;
;; `(frame/cont (@@ (cont ,closure [arg ...])))
;;

(define (%frame/cont-handler name attrs auxs contents context cont)
  (define (auxs->uri auxs)
    (cond ((assq-ref auxs 'cont)        => (local-cont  auxs))
	  ((assq-ref auxs 'remote-cont) => (remote-cont auxs))
	  (else                            (kahua-self-uri (fragment auxs)))))
  (cont `((frame (@@ (expand-finished))
		 (@ ,(or (assq 'src attrs) `(src ,(auxs->uri auxs)))
		    ,@(remove-attrs attrs 'src)))) context))

(define-element frame/cont %frame/cont-handler)
(define-element frame %frame/cont-handler)

;;
;; img/cont
;;
;; `(img/cont (@@ (cont ,closure [arg ...])))
;;

(define (%img/cont-handler name attrs auxs contents context cont)
  (define (auxs->uri auxs)
    (cond ((assq-ref auxs 'cont)        => (local-cont  auxs))
	  ((assq-ref auxs 'remote-cont) => (remote-cont auxs))
	  (else                            (kahua-self-uri (fragment auxs)))))
  (cont `((img (@@ (expand-finished))
	       (@ ,(or (assq 'src attrs) `(src ,(auxs->uri auxs)))
		  ,@(remove-attrs attrs 'src)))) context))

(define-element img/cont %img/cont-handler)
(define-element img      %img/cont-handler)

;;
;; redirect/cont
;;
;; (redirect/cont (cont closure [arg ...]) ...
;;
(define-syntax redirect/cont
  (syntax-rules ()
    ((_ (name var ...) ...)
     (%redirect/cont% (list (list 'name var ...) ...)))))

(define (%redirect/cont% auxs)

  (define (nodes path)
    (let/pc pc
      `((html (extra-header
               (@ (name "Status") (value "302 Found")))
              (extra-header
               (@ (name "Location")
                  (value ,path)))))))

  (cond ((assq-ref auxs 'cont) => (compose nodes (local-cont auxs)))
        ((assq-ref auxs 'remote-cont) => (compose nodes (remote-cont auxs)))
        (else (nodes (kahua-self-uri (fragment auxs))))))


;;
;; script
;; style
;;
(define (%script-element-handler tag attrs _ content context cont)
  (define (string->script-string str)
    (with-string-io str
      (lambda ()
	(with-port-locking (current-input-port)
	  (lambda ()
	    (with-port-locking (current-output-port)
	      (lambda ()
		(letrec ((in-code (lambda (c)
				    (unless (eof-object? c)
				      (write-char c)
				      (case c
					((#\") (in-string (read-char)))
					(else  (in-code (read-char)))))))
			 (in-string (lambda (c)
				      (unless (eof-object? c)
					(write-char c)
					(case c
					  ((#\\) (escape-char (read-char)))
					  ((#\<) (maybe-escape (read-char)))
					  ((#\") (in-code (read-char)))
					  (else (in-string (read-char)))))))
			 (escape-char (lambda (c)
					(unless (eof-object? c)
					  (write-char c)
					  (in-string (read-char)))))
			 (maybe-escape (lambda (c)
					 (unless (eof-object? c)
					   (when (char=? #\/ c)
					     (write-char #\\))
					   (write-char c)
					   (in-string (read-char))))))
		  (in-code (read-char))))))))))
  (define (make-cdata-node str)
    (list '%%CDATA str))
  (define (proc-content c)
    (cond ((null? c) c)
	  ((pair? c) (map proc-content c))
	  (else (make-cdata-node (string->script-string (x->string c))))))
  (cont `((,tag (@@ (expand-finished))
		(@ ,@attrs)
		,@(proc-content content)))
	context))

(define-element style %script-element-handler)
(define-element script %script-element-handler)

(define-element %%CDATA (_ attrs auxs contents context cont)
  (cont (list (apply make-no-escape-text-element contents)) context))

;;
;; extra-header - inserts protocol header to the reply message
;; 
;; `(extra-header (@ (name ,name) (value ,value)))
;;

(define (add-extra-header context name value)
  (let loop ((ctxt context)
	     (done '()))
    (if (null? ctxt)
	(cons `("extra-headers" ((,name ,value))) context)
	(let1 e (car ctxt)
	  (if (string=? "extra-headers" (car e))
	      (fold cons (cons `("extra-headers"
				 ,(kahua-merge-headers (cadr e)
						       `((,name ,value))))
			       (cdr ctxt))
		    done)
	      (loop (cdr ctxt) (cons e done)))))))

(define-element extra-header (attrs auxs contents context cont)
  (and-let* ((name (assq-ref-car attrs 'name))
	     (value (assq-ref-car attrs 'value)))
    (cont '() (add-extra-header context name value))))

;; Conditional Comments for Internet Explorer
;; <!--[if gte IE 5]> IE 5.0 - 6.x
;; <!--[if IE 5]> IE 5.0
;; <!--[if IE 5.5000]> IE 5.5
;; <!--[if IE 6]> IE 6.0
;; <!--[if gte IE 5.5000]> IE 5.5 - 6.x
;; <!--[if lt IE 6]>IE 5.0 - 5.5

(define-element with-ie (attrs auxs contents context cont)
  (let1 condition (assq-ref-car attrs 'condition "IE")
    (cont `(,(make-no-escape-text-element (format "<!--[if ~a]>" condition))
            ,@contents
            ,(make-no-escape-text-element "<![endif]-->"))
          context)))

;; No Escape Node
;;
;;(define-element no-escape (attrs auxs contents context cont)
;;  (cont (list (apply make-no-escape-text-element contents))
;;	context))

;; character entity reference
;;
(define-element & (attrs auxs contents context cont)
  (cont (list (apply make-no-escape-text-element
		     (map (lambda (c)
			    (cond ((or (string? c) (symbol? c)) (format "&~a;" c))
				  ((char?   c) (format "&#x~x;" (char->ucs c)))
				  ((integer? c) (format "&#x~x;" c))
				  (else (error "& node require string or symbol(character name), integer(character code) or character itself, but got " c))))
			  contents)))
	context))

;;===========================================================
;; Text tree interpreter - for Plain Text
;;
;; interp-text ;; Nodes(Stree) -> Context -> Stree

(define (interp-text nodes context cont)
  (cont nodes (add-extra-header context "content-type"
				(make-content-type "text/plain"))))

(add-interp! 'text interp-text)

;;===========================================================
;; SXML tree interpreter - for RSS
;;
;; interp-rss :: Node -> Context -> Stree

(define (interp-rss nodes context cont)
  (receive (stree context)
      (interp-html-rec-bis nodes context cont)
    (values
     ;; Stree
     (cons #`"<?xml version=\"1.0\" encoding=\",|*default-charset*|\" ?>\n"
	   stree)
     ;; Context
     (add-extra-header context "content-type"
		       (make-content-type "text/xml")))))

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
	 (add-extra-header context "content-type"
			   (make-content-type "text/xml")))))))

(add-interp! 'xml interp-xml)

;;===========================================================
;; CSS tree interpreter - for generates CSS
;;
;; (define-entry (test.css)
;;   `((css
;;      (.status-completed
;;       (background-color "rgb(231,231,231)"))
;;
;;      (.status-open
;;       (background-color "rgb(255, 225, 225)")))))
;;
;; (head/ (link/ (@/ (rel "stylesheet") (type "text/css")
;; 		     (href (kahua-self-uri-full "test.css")))))

(define-constant *css-media-type* "text/css")

(define (interp-css nodes context cont)
  (cont
   (parse-stylesheet (cdr nodes))
   (add-extra-header context "content-type"
		     (make-content-type *css-media-type*))))

(add-interp! 'css interp-css)

(define-constant *json-media-type* "application/x-javascript")
;; RFC4627, but most of implementations don't support this media type.
;(define-constant *json-media-type* "application/json")

(define-class <json-base> () ())
(define-method x->json ((self <json-base>))
  (let* ((class (class-of self))
         (slots (class-slots class)))
    (x->json
     (list->vector
      (filter-map
       (lambda (slot)
         (and (slot-definition-option slot :json #f)
              (let1 slot-name (slot-definition-name slot)
                (cons slot-name
                      (ref self slot-name)))))
       slots)))))

(define (interp-json nodes context cont)
  (define write-str (pa$ format "~s"))
  (define (write-ht vec)
    (list "{"
          (intersperse
           ","
           (map (lambda (entry)
                  (let ((k (car entry))
                        (v (cdr entry)))
                    (list (cond
                           ((symbol? k) k )
                           ((string? k) (write-str k))
                           (else (error "Invalid JSON table key in interp-json" k)))
                          ": "
                          (x->json v))))
                vec))
          "}"))

  (define (write-array a)
    (list "["
          (intersperse
           ","
           (map (lambda (v)
                  (x->json v))
                a))
          "]"))

  (define-method x->json (x)
    (cond
     ((hash-table? x) ((compose write-ht list->vector hash-table->alist)
                       x))
     ((vector? x) (write-ht x))
     ((pair? x) (write-array x))
     ((symbol? x) (write-str (symbol->string x))) ;; for convenience
     ((number? x) x)
     ((string? x) (write-str x))
     ((boolean? x) (if x "true" "false"))
     ((is-a? x <collection>) (write-array x))
     ;; ((eq? x (void)) (display "null" p))
     (else (error "Invalid JSON object in interp-json" x))))

  (cont (list "(" (x->json (cadr nodes)) ")\n")
	(add-extra-header context "content-type"
			  (make-content-type *json-media-type*))))

(add-interp! 'json interp-json)

;; Declarative Validation Syntax (experimental)
(define-syntax with-validation
  (syntax-rules (=>)
    ((_ ((val validator error) ...) => err-hdr body ...)
     (let1 check (fold (lambda (e r)
			 (apply (lambda (v vldr err)
				  (if (vldr v)
				      r
				      (cons (err v) r)))
				e))
		       '() `((,val ,validator ,error) ...))
       (if (null? check)
	   (begin body ...)
	   (err-hdr (reverse! check)))))))

(provide "kahua/server")
