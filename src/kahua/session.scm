;; Session manager
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: session.scm,v 1.1 2003/12/11 05:39:12 nobsun Exp $

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

(define-module kahua.session
  (use kahua.gsid)
  (use gauche.parameter)
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
          session-state-discard
          session-state-sweep
          session-flush-all)
  )
(select-module kahua.session)

;;; initialization --------------------------------------------
;;
;; application server must tell this module its worker Id.

(define worker-id (make-parameter #f))

(define (session-manager-init wid)
  (worker-id wid)
  #t)

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
;;
;; For now, we keep a pair of the timestamp when the continuation
;; is registered, and the continuation itself.

(define cont-sessions
  (make-parameter (make-hash-table 'string=?)))

(define (make-cont-key)
  (check-initialized)
  (let loop ((id (make-gsid (worker-id) (x->string (random-integer IDRANGE)))))
    (if (hash-table-exists? (cont-sessions) id)
      (loop)
      id)))

;; SESSION-CONT-REGISTER cont [ id ]
;;   Register continuation of CONT with id ID.  Usually ID should be
;;   omitted, and the session manager generates one for you.  Returns
;;   the assigned ID.
(define (session-cont-register cont . maybe-id)
  (let ((id (get-optional maybe-id (make-cont-key)))
        (timestamp (sys-time)))
    (hash-table-put! (cont-sessions) id (cons timestamp cont))
    id))

;; SESSION-CONT-GET id
;;   Returns the continuation procedure associated with ID.
;;   If such a procedure doesn't exist, returns #f.
(define (session-cont-get id)
  (and-let* ((p (hash-table-get (cont-sessions) id #f)))
    (cdr p)))

;; SESSION-CONT-DISCARD id
;;   Discards the session specified by ID.
(define (session-cont-discard id)
  (hash-table-delete! (cont-sessions) id))

;; SESSION-CONT-SWEEP age
;;   Discards sessions that are older than AGE (in seconds)
;;   Returns # of sessions discarded.
(define (session-cont-sweep age)
  (let ((cutoff (- (sys-time) age)))
    (sweep-hash-table (cont-sessions)
                      (lambda (val) (< (car val) cutoff)))))

;;; state session table ---------------------------------------
;;
;; This module associates state session ID and <session-state>
;; object.   This module doesn't care the content of <session-state>;
;; it's up to the application to use it.

(define-class <session-state> ()
  ((%properties :init-value '())))

(define-method slot-missing ((class <class>) (obj <session-state>) slot)
  (assq-ref (ref obj '%properties) slot))

(define-method slot-missing ((class <class>) (obj <session-state>) slot val)
  (set! (ref obj '%properties)
        (assq-set! (ref obj '%properties) slot val)))

(define state-sessions
  (make-parameter (make-hash-table 'string=?)))

(define (make-state-key)
  (check-initialized)
  (let loop ((id (make-gsid (worker-id) (x->string (random-integer IDRANGE)))))
    (if (hash-table-exists? (state-sessions) id)
      (loop)
      id)))

;; SESSION-STATE-REGISTER [id]
;;   Register a new session state.  Returns a state session ID.
;;   You can specify ID, but otherwise the system generates one for you.
(define (session-state-register . args)
  (let ((id (get-optional args (make-state-key)))
        (state (make <session-state>))
        (timestamp (sys-time)))
    (hash-table-put! (state-sessions) id
                     (cons timestamp state))
    id))

;; SESSION-STATE-GET id
;;   Returns a session state object corresponding ID.
;;   If no session is associated with the ID, a new session state
;;   object is created.
(define (session-state-get id)
  (let1 p (or (hash-table-get (state-sessions) id #f)
              (hash-table-get (state-sessions)
                              (session-state-register id)))
    (cdr p)))

;; SESSION-STATE-DISCARD id
;;   Discards the session specified by ID.
(define (session-state-discard id)
  (hash-table-delete! (state-sessions) id))

;; SESSION-STATE-SWEEP age
;;   Discards sessions that are older than AGE (in seconds)
;;   Returns # of sessions discarded.
(define (session-state-sweep age)
  (let ((cutoff (- (sys-time) age)))
    (sweep-hash-table (state-sessions)
                      (lambda (val) (< (car val) cutoff)))))

;;; common API ----------------------------------------------

;; SESSION-FLUSH-ALL
;;   Discards all sessions.
(define (session-flush-all)
  (cont-sessions  (make-hash-table 'string=?))
  (state-sessiosn (make-hash-table 'string=?)))

(provide "kahua/session")

