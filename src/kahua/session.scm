;; Session manager
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: session.scm,v 1.9 2004/05/18 03:54:15 nobsun Exp $

;; This module manages two session-related structure.
;;
;;  (1) Continuation session table
;;       This table manages association of a continuation session ID and
;;       the actual continuation.
;;
;;  (2) State session table
;;       This table manages association of a state session ID and
;;       a state session object, which can contain long-living
;;       information such as login user.
;;
;; State session may be stored in process-local table or a global
;; key server (see kahua-keyserv.in).  It is determined by whether
;; the second argument is passed to session-manager-init; if it is
;; omitted or #f, process-local table is used.  If it is a string,
;; which should be a worker-id of the running key server, then the
;; session key is stored in the key server.
;;
;; NB: In the normal mode of Kahua operation, key server should be
;; used.  The process-local mode is only kept for testing and
;; backward compatibility.


(define-module kahua.session
  (use kahua.gsid)
  (use kahua.config)
  (use kahua.persistence)
  (use gauche.parameter)
  (use gauche.validator)
  (use gauche.net)
  (use util.list)
  (use srfi-2)
  (use srfi-27)
  (export session-manager-init
          session-cont-register
          session-cont-get
          session-cont-discard
          session-cont-sweep
          <session-state>
          session-state-register
          session-state-get
	  session-state-ref
          session-state-discard
          session-state-sweep
          session-state-refresh
          session-flush-all
	  session-state-all-keys)
  )
(select-module kahua.session)

;;; initialization --------------------------------------------
;;
;; application server must tell this module its worker Id.

(define worker-id (make-parameter #f))

(define session-server-id (make-parameter #f))

(define (session-manager-init wid . maybe-ssid)
  (let-optionals* maybe-ssid ((ssid #f))
    (worker-id wid)
    (when (string? ssid) (session-server-id ssid))
    #t))

(define (check-initialized)
  (unless (worker-id) (error "session manager is not initialized")))

(define-constant IDRANGE #x10000000)

(define (sweep-hash-table table pred)
  (let1 discards
      (hash-table-fold table
                       (lambda (key val lis)
                         (if (pred val)
                           (cons key lis)
                           lis))
                       '())
    (for-each (cut hash-table-delete! table <>) discards)
    (length discards)))

;;; continuation session table --------------------------------

(define-class <session-cont> (<validator-mixin>)
  ((key               :init-keyword :key       ;; ID key string
                      :validator (lambda (o v) (x->string v)))
   (closure           :init-keyword :closure)  ;; closure
   (permanent?        :init-keyword :permanent? ;; permanent id?
                      :init-value #f)
   (timestamp         :init-keyword :timestamp
                      :init-form (sys-time))   ;; timestamp
   (key->session      :allocation :class
                      :init-form (make-hash-table 'string=?))
   (closure->session  :allocation :class
                      :init-form (make-hash-table 'eq?))
   ))

(define-method initialize ((self <session-cont>) initargs)
  (next-method)
  ;; NB: in MT, this is a critical section
  (unless (slot-bound? self 'key)
    (slot-set! self 'key (make-cont-key)))
  (hash-table-put! (ref self 'key->session) (ref self 'key) self)
  (hash-table-put! (ref self 'closure->session) (ref self 'closure) self)
  ;; end of critical section
  )

(define-method replace-key ((self <session-cont>) new-key)
  (let1 keytab (ref self 'key->session)
    (hash-table-delete! keytab (ref self 'key))
    (set! (ref self 'key) new-key)
    (hash-table-put! keytab new-key self)
    new-key))

(define (cont-closure->session clo)
  (hash-table-get (class-slot-ref <session-cont> 'closure->session) clo #f))

(define (cont-key->session key)
  (hash-table-get (class-slot-ref <session-cont> 'key->session) key #f))

(define (make-cont-key)
  (check-initialized)
  (let loop ((id (make-gsid (worker-id)
                            (number->string (random-integer IDRANGE) 36))))
    (if (cont-key->session id) (loop) id)))

;; SESSION-CONT-REGISTER cont [ id ]
;;   Register continuation closure CONT with id ID.  Usually ID should be
;;   omitted, and the session manager generates one for you.  Returns
;;   the assigned ID.   If the same closure of CONT is already registered,
;;   already-assigned ID is returned (explicitly giving different ID 
;;   removes old entry).  If CONT is not a procedure, returns #f.
(define (session-cont-register cont . maybe-id)
  (and (procedure? cont)
       (let ((entry (cont-closure->session cont))
             (given-id (get-optional maybe-id #f)))
         (if entry
           (if (or (null? maybe-id)
                   (equal? given-id (ref entry 'key)))
             (ref entry 'key)
             (replace-key entry given-id))
           (ref (apply make <session-cont>
                       :closure cont
                       (if given-id
                         `(:key ,given-id :permanent? #t)
                         '()))
                'key)))))

;; SESSION-CONT-GET id
;;   Returns the continuation procedure associated with ID.
;;   If such a procedure doesn't exist, returns #f.
(define (session-cont-get id)
  (and-let* ((entry (cont-key->session id)))
    ;; update timestamp
    (slot-set! entry 'timestamp (sys-time))
    (session-cont-sweep (* 60 (ref (kahua-config) 'timeout-mins)))
    (ref entry 'closure)))

;; SESSION-CONT-DISCARD id
;;   Discards the session specified by ID.
(define (session-cont-discard id)
  (and-let* ((entry (cont-key->session id)))
    (hash-table-delete! (ref entry 'key->session) (ref entry 'key))
    (hash-table-delete! (ref entry 'closure->session) (ref entry 'closure))))

;; SESSION-CONT-SWEEP age
;;   Discards sessions that are older than AGE (in seconds)
;;   Returns # of sessions discarded.
(define (session-cont-sweep age)
  (let ((cutoff (- (sys-time) age)))
    (map (lambda (tab)
           (sweep-hash-table
            (class-slot-ref <session-cont> tab)
            (lambda (entry)
              (and (not (ref entry 'permanent?))
                   (< (ref entry 'timestamp) cutoff)))))
         '(key->session closure->session))))

;;; state session table ---------------------------------------
;;
;; This module associates state session ID and <session-state>
;; object.   This module doesn't care the content of <session-state>;
;; it's up to the application to use it.
;;
;; The content of session-state may be passed around across the
;; process boundary, so only serializable object can be stored.
;; 
;; If session-server-id is given to session-manager-init, the
;; session key is stored in the key server.

(define-class <session-state> ()
  ((%session-id :init-keyword :session-id)
   (%timestamp  :init-keyword :timestamp)
   (%properties :init-value '())))

;; pseudo getter
(define-method slot-missing ((class <class>) (obj <session-state>) slot)
  (assq-ref (ref obj '%properties) slot))

;; pseudo setter
(define-method slot-missing ((class <class>) (obj <session-state>) slot val)
  (unless (kahua-serializable-object? val)
    (error "attempted to enter unserializable object to <session-state>: ~s"
           val))
  (set! (ref obj '%properties)
        (assq-set! (ref obj '%properties) slot val))
  (update-session-state obj (cons slot val))
  )

;; Obtain the newest session-state.
(define-method update-session-state ((self <session-state>) . attrs)
  (when (session-server-id)
    (synchronize-session-state
     self
     (keyserver (cons (ref self '%session-id) attrs)))))

(define-method synchronize-session-state ((self <session-state>) result)
  (set! (ref self '%timestamp) (assq-ref (cdr result) '%ctime))
  (set! (ref self '%properties) (cdr result)))

;; Local session table.  Keeps key <-> <session-state>
(define state-sessions
  (make-parameter (make-hash-table 'string=?)))

;; Creates local key.  Only used in process-local mode.
(define (make-state-key)
  (check-initialized)
  (let loop ((id (make-gsid (worker-id) (x->string (random-integer IDRANGE)))))
    (if (hash-table-exists? (state-sessions) id)
      (loop)
      id)))

;; Communicate to keyserver
(define (keyserver request)
  (call-with-client-socket
      (make-client-socket (worker-id->sockaddr (session-server-id)
                                               (kahua-sockbase)))
    (lambda (in out)
      (write request out) (flush out)
      (let1 result (read in)
        (when (not (pair? result))
          (error "keyserver failure: check log file"))
        result))))

;; SESSION-STATE-REGISTER [id]
;;   Register a new session state.  Returns a state session ID.
;;   You can specify ID, but otherwise the system generates one for you.
(define (session-state-register . args)
  (if (session-server-id)
    (car (keyserver (list (get-optional args #f))))
    (let* ((id (get-optional args (make-state-key)))
           (state (make <session-state> :session-id id :timestamp (sys-time))))
      (hash-table-put! (state-sessions) id state)
      id)))

;; SESSION-STATE-GET id
;;   Returns a session state object corresponding ID.
;;   If no session is associated with the ID, a new session state
;;   object is created.
(define (session-state-get id . rest)
  (let-keywords* rest ((refresh? :refresh? #t))
    (if (session-server-id)
        (let* ((result (keyserver (if refresh?
                                      (list id)
                                      `(ref ,id))))
               (state (make <session-state> :session-id id)))
          (synchronize-session-state state result)
          (session-state-sweep (* 60 (ref (kahua-config) 'timeout-mins)))
          state)
        (let1 state (or (hash-table-get (state-sessions) id #f)
                        (hash-table-get (state-sessions)
                                        (session-state-register id)))
	  (if refresh? (session-state-refresh id))
	  (session-state-sweep (* 60 (ref (kahua-config) 'timeout-mins)))
          state))))

(define (session-state-ref id)
  (session-state-get id :refresh? #f))

;; SESSION-STATE-DISCARD id
;;   Discards the session specified by ID.
(define (session-state-discard id)
  (if (session-server-id)
    #f ;; for now, we don't discard global session id
    (hash-table-delete! (state-sessions) id)))

;; SESSION-STATE-SWEEP age
;;   Discards sessions that are older than AGE (in seconds)
;;   Returns # of sessions discarded.
(define (session-state-sweep age)
  (if (session-server-id)
    (keyserver (list 'flush age))
    (let ((cutoff (- (sys-time) age)))
      (sweep-hash-table (state-sessions)
                        (lambda (val) (< (ref val '%timestamp) cutoff))))))

;; SESSION-STATE-REFRESH id
;;   Update session timestamp.
(define (session-state-refresh id)
  (if (session-server-id)
    (keyserver (list id))
    (set! (ref (hash-table-get (state-sessions) id) '%timestamp)
          (sys-time))))

(define (session-state-all-keys)
  (if (session-server-id)
      (keyserver '(keys))
      (hash-table-keys (state-sessions))))

;;; common API ----------------------------------------------

;; SESSION-FLUSH-ALL
;;   Discards all sessions.
(define (session-flush-all)
  (cont-sessions  (make-hash-table 'string=?))
  (state-sessiosn (make-hash-table 'string=?)))

(provide "kahua/session")

