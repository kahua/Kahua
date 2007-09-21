;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.session
;; $Id: session.scm,v 1.3 2005/07/04 05:09:21 nobsun Exp $

;; NB: first we test state session without session key server (local mode),
;; then start up the server process and test the shared key mode.

(use gauche.test)
(use gauche.process)
(use gauche.net)
(use util.list)
(use kahua.gsid)
(use kahua.config)

(test-start "kahua.session")
(use kahua.session)
(test-module 'kahua.session)

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o777)
(sys-mkdir "_work" #o777)

;;------------------------------------------------------------------
(test-section "initialization")

(test* "detecting uninitialized" *test-error*
       (session-cont-register values))

(test* "initialization" #t
       (begin (session-manager-init "worker") #t))

(kahua-init "./test.conf")

;;------------------------------------------------------------------
(test-section "cont session")

(define a-cont (lambda _ 'a))
(define b-cont (lambda _ 'b))

(define a-cont-id #f)
(define b-cont-id #f)

(test* "register1" #t
       (begin
         (set! a-cont-id (session-cont-register a-cont))
         (string? a-cont-id)))

(test* "get1" 'a
       ((session-cont-get a-cont-id)))

(test* "register2" #f
       (begin
         (set! b-cont-id (session-cont-register b-cont))
         (equal? b-cont-id a-cont-id)))

(test* "get2" 'b
       ((session-cont-get b-cont-id)))

(test* "get3" #f
       (session-cont-get "nosuchid"))

(test* "discard" #f
       (begin
         (session-cont-discard a-cont-id)
         (session-cont-get a-cont-id)))

(test* "sweep" 'b
       (begin
         (session-cont-sweep 10000000)
         ((session-cont-get b-cont-id))))

(test* "sweep" #f
       (begin
         (session-cont-sweep -1)
         (session-cont-get b-cont-id)))

;;------------------------------------------------------------------
(test-section "state session (process local)")

(define a-state-id #f)
(define b-state-id #f)

(test* "register1" #t
       (begin
         (set! a-state-id (session-state-register))
         (string? a-state-id)))

(test* "get1" #t
       (is-a? (session-state-get a-state-id) <session-state>))

(test* "storing stuff" '(c b)
       (let ((state (session-state-get a-state-id)))
         (set! (ref state 'a-slot) 'a)
         (set! (ref state 'b-slot) 'b)
         (set! (ref state 'a-slot) 'c)
         (list (ref state 'a-slot) (ref state 'b-slot))))

(test* "implicit creation" '(#f #f)
       (let ((state (session-state-get "nosuchid")))
         (list (ref state 'a-slot) (ref state 'b-slot))))

(test* "explicit id access" '(x y)
       (begin
         (let ((state (session-state-get "nosuchid")))
           (set! (ref state 'x-slot) 'x)
           (set! (ref state 'y-slot) 'y))
         (let ((state (session-state-get "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

(test* "discarding" '(#f #f)
       (begin
         (session-state-discard a-state-id)
         (let ((state (session-state-get a-state-id)))
           (list (ref state 'a-slot) (ref state 'b-slot)))))

(test* "sweep" '(x y)
       (begin
         (session-state-sweep 10000)
         (let ((state (session-state-get "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

(test* "sweep" '(#f #f)         
       (begin
         (session-state-sweep -1)
         (let ((state (session-state-get "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

;;------------------------------------------------------------------
(test-section "session key server")

(define kserv #f)
(define kserv-id #f)

(test* "start key server" #t
       (let* ((p (run-process "gosh" "-I../src" "../src/kahua-keyserv"
                              "-c" "./test.conf" :output :pipe))
              (id (read-line (process-output p))))
         (set! kserv p)
         (set! kserv-id id)
         (string? id)))

(define (get-session-key request)
  (call-with-client-socket
      (make-client-socket (worker-id->sockaddr kserv-id "unix:_tmp"))
    (lambda (in out)
      (write request out) (close-output-port out)
      (read in))))

(let ((key #f))
  (test* "retrieve new key" #t
         (let1 reply (get-session-key '(#f))
           (and (pair? reply)
                (begin (set! key reply)
                       (string? (car key))))))

  (test* "retrieve another key" #t
         (let1 reply (get-session-key '(#f))
           (and (pair? reply)
                (string? (car reply))
                (not (string=? (car key) (car reply))))))
               
  (test* "attach attr" '("adagio" e-moll)
         (let1 reply (get-session-key (list (car key)
                                            '(tempo . "adagio")
                                            '(key . e-moll)))
           (list (assq-ref (cdr reply) 'tempo)
                 (assq-ref (cdr reply) 'key))))

  (test* "check attr" '("adagio" e-moll)
         (let1 reply (get-session-key (list (car key)))
           (list (assq-ref (cdr reply) 'tempo)
                 (assq-ref (cdr reply) 'key))))

  (test* "change attr" '("adagio" D-dur)
         (let1 reply (get-session-key (list (car key)
                                            '(key . D-dur)))
           (list (assq-ref (cdr reply) 'tempo)
                 (assq-ref (cdr reply) 'key))))

  (test* "ref" '("adagio" D-dur)
         (let1 reply (get-session-key `(ref ,(car key)))
           (list (assq-ref (cdr reply) 'tempo)
                 (assq-ref (cdr reply) 'key))))

  (test* "ref (check %ctime is not changed)" #t
         (let* ((g (get-session-key (list (car key))))
                (_ (sys-sleep 2))
                (r (get-session-key `(ref ,(car key)))))
           (= (assq-ref g '%ctime)
              (assq-ref r '%ctime))))

  (test* "admin message (stat)" '(2)
         (get-session-key '(stat)))

  (test* "admin message (flush)" '(0)
         (get-session-key '(flush -1)))

  (test* "after flush" '(#f #f)
         (let1 reply (get-session-key (list (car key)))
           (list (assq-ref (cdr reply) 'tempo)
                 (assq-ref (cdr reply) 'key))))

  (set-signal-handler! SIGPIPE #f)

  (test* "bad request" #t
         (begin
           (set! key (get-session-key '(#f)))
           (let1 sock
               (make-client-socket (worker-id->sockaddr kserv-id "unix:_tmp"))
             (call-with-client-socket sock
               (lambda (in out)
                 (display "(poge" out) (close-output-port out)
                 (socket-shutdown sock 1)
                 (read in))))
           (equal? (car key) (car (get-session-key (list (car key)
                                                         '(a . "b")))))))

  (test* "bad request2" "b"
         (begin
           (let1 sock
               (make-client-socket (worker-id->sockaddr kserv-id "unix:_tmp"))
             (call-with-client-socket sock
               (lambda (in out)
                 (display "#<zz>" out) (close-output-port out)
                 (read in))))
           (let1 reply (get-session-key (list (car key)))
             (assq-ref (cdr reply) 'a))))

  (test* "all-keys" (car (get-session-key '(stat)))
         (length (get-session-key '(keys))))
  )

;;------------------------------------------------------------------
(test-section "state session (key server)")

;; now we use state-session keys via key server

(session-manager-init "worker" kserv-id)

(define a-state-id #f)
(define b-state-id #f)

(test* "register1" #t
       (begin
         (set! a-state-id (session-state-register))
         (string? a-state-id)))

(test* "get1" #t
       (is-a? (session-state-get a-state-id) <session-state>))

(test* "storing stuff" '(c b)
       (let ((state (session-state-get a-state-id)))
         (set! (ref state 'a-slot) 'a)
         (set! (ref state 'b-slot) 'b)
         (set! (ref state 'a-slot) 'c)
         (list (ref state 'a-slot) (ref state 'b-slot))))

(test* "implicit creation" '(#f #f)
       (let ((state (session-state-get "nosuchid")))
         (list (ref state 'a-slot) (ref state 'b-slot))))

(test* "explicit id access" '(x y)
       (begin
         (let ((state (session-state-get "nosuchid")))
           (set! (ref state 'x-slot) 'x)
           (set! (ref state 'y-slot) 'y))
         (let ((state (session-state-get "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

(test* "session-state-ref" '(x y)
       (begin
         (let ((state (session-state-ref "nosuchid")))
           (set! (ref state 'x-slot) 'x)
           (set! (ref state 'y-slot) 'y))
         (let ((state (session-state-ref "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

(test* "session-state-ref (timestamp is not changed)" #t
       (let* ((g (session-state-get "nosuchid"))
              (_ (sys-sleep 2))
              (r (session-state-ref "nosuchid")))
         (= (ref g '%timestamp)
            (ref r '%timestamp))))

(test* "sweep" '(x y)
       (begin
         (session-state-sweep 10000)
         (let ((state (session-state-get "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

(test* "sweep" '(#f #f)         
       (begin
         (session-state-sweep -1)
         (let ((state (session-state-get "nosuchid")))
           (list (ref state 'x-slot) (ref state 'y-slot)))))

(set! a-state-id (session-state-register))
(set! b-state-id (session-state-register))

(test* "session-state-all-keys" (sort (list a-state-id b-state-id "nosuchid"))
        (sort (session-state-all-keys)))

(when kserv
  (process-send-signal kserv SIGHUP)
  (process-wait kserv))

(test-end)
