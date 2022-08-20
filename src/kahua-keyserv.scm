;; session-key server.
;;
;;  Copyright (c) 2004-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

;; This will eventually becomes generic object broker.
;; For now, this only handles state session object.
;; This script should be invoked by kahua-spvr.
;; When started, objserv reports the worker id to stdout.

;; Protocol:
;;   For the time being, a distributed object is simply a list of:
;;     (<key> (<name> . <value>) (<name2> . <value2>) ...)
;;   where <key> is a unique string, <name> is an attribute name (symbol)
;;   and <value> is the attribute value.
;;   For object conciseness, the attribute is not recorded if its value is #f.
;;
;;   A client can give the following list as request.
;;     (<key> (<name> . <value>) ...)
;;   The following action(s) is(are) taken:
;;     * If the object with <key> doesn't exist, one is created.
;;     * If (<name> . <value>) is given, the named attribute of
;;       the object is set.
;;   After those actions, the complete object description is
;;   returned to the client.
;;
;;   As a special case, a client can give a request
;;     (#f)
;;   which lets the server allocate a fresh key.
;;
;;   Administrative requests:
;;     (flush <timeout>)
;;        Forces the server to discard keys older than <timeout> seconds.
;;        Replies (<number-of-current-keys>)
;;     (stat)
;;        Replies (<number-of-current-keys>)
;;     (keys)
;;        Replies list of all existing keys
;;
;; This protocol favors simplicity over performance.  It won't scale,
;; so we'll watch out how it behaves.

(use gauche.net)
(use gauche.parseopt)
(use gauche.selector)
(use gauche.threads)
(use control.thread-pool)
(use util.list)
(use util.match)
(use srfi-27)
(use kahua.config)
(use kahua.util)
(use kahua.gsid)

(define-constant *default-timeout* (* 3 3600))
(define-constant *TERMINATION-SIGNALS* (sys-sigset-add! (make <sys-sigset>) SIGTERM SIGINT SIGHUP))

(define (main args)
  (let-args (cdr args) ((site "S=s")
                        (conf-file "c=s")
                        (thnum "t:threads=i" #f))
    (kahua-common-init site conf-file)
    (write-pid-file (kahua-keyserv-pidpath))
    (kahua:log-open (kahua-logpath "kahua-keyserv.log") :prefix "~Y ~T ~P[~$]: ")
    (random-source-randomize! default-random-source)
    (let* ((wid (make-worker-id "%keyserv"))
           (sockaddr (worker-id->sockaddr wid (kahua-sockbase)))
           (tpool (make-thread-pool (or thnum (kahua-keyserv-concurrency)))))
      (unwind-protect
       (begin
         (set-signal-handler! SIGPIPE #f)
         (sys-sigmask SIG_BLOCK *TERMINATION-SIGNALS*)
         (run-server wid sockaddr tpool)
         (sys-sigwait *TERMINATION-SIGNALS*)
         0)
       (begin
         (when (is-a? sockaddr <sockaddr-un>)
           (sys-unlink (sockaddr-name sockaddr)))
         (terminate-all! tpool 0)
         (sys-unlink (kahua-keyserv-pidpath)))))))

(define (usage)
  (print "kahua-keyserv [-S <site-bundle>] [-c <conf-file>] [-t <thread-count>]")
  (exit 0))

;; server main loop - similar to kahua-server.  call for refactoring.
(define (run-server worker-id sockaddr tpool)
  (let ((sock (make-server-socket sockaddr :reuse-addr? #t :backlog SOMAXCONN))
        (selector (make <selector>)))
    (define (accept-handler fd flag)
      (add-job! tpool (cute handle-request (socket-accept sock))))
    (define (orphan-handler in flag)
      (when (eof-object? (read-byte in))
        (display "keyserv: parent(maybe spvr) has gone, so me too!!\n" (current-error-port))
        (selector-delete! selector in #f #f)
        (sys-kill (sys-getpid) SIGTERM)))

    ;; hack
    (when (is-a? sockaddr <sockaddr-un>)
      (sys-chmod (sockaddr-name sockaddr) #o770))
    (format #t "~a\n" worker-id) ;; tell spvr about myself
    (close-output-port (current-output-port))
    (selector-add! selector (current-input-port) orphan-handler '(r))
    (selector-add! selector (socket-fd sock) accept-handler '(r))

    (let1 t (make-thread (lambda ()
                           (do () (#f)
                             (when (zero? (selector-select selector 60.0e6))
                               (sweep-objects *default-timeout*))))
                         "dispatcher")
      (thread-start! t))))

(define (handle-request client)
  (call-with-client-socket client
    (lambda (input output)
      (guard (e (else #f))
        (unwind-protect
         (guard (e (else (kahua:log-format "~a" (kahua-error-string e #t))))
           (let loop ((request (read input)))
             (unless (eof-object? request)
               (let1 result (match request
                              (('flush . maybe-sec)
                               (sweep-objects (x->integer (get-optional maybe-sec #f)))
                               (list (num-objects)))
                              (('stat) (list (num-objects)))
                              (('keys) (all-keys))
                              (('ref key) (ref-object key))
                              ((key . attrs) (handle-object-command key attrs))
                              (else    #f))
                 (write result output)
                 (newline output)
                 (flush output)
                 (loop (read input))))))
         (with-ignoring-exception (cut socket-shutdown client SHUT_RDWR))))))
  0)

(define (handle-object-command key attrs)
  (let loop ((obj (get-object key))
             (attrs attrs))
    (if (null? attrs)
        obj
        (begin (set-cdr! obj (assq-set! (cdr obj) (caar attrs) (cdar attrs)))
               (loop obj (cdr attrs))))))

;; Object pool -------------------------------------------

(define-constant *object-pool* (make-hash-table 'string=?))
(define-constant *mutex*       (make-mutex))

(define-constant *max-key* (expt 2 64))

(define (get-object key)
  (with-locking-mutex *mutex*
    (lambda ()
      (let1 key (if (string? key)
                    key
                    (let loop ()
                      (let1 candidate (number->string (random-integer *max-key*) 36)
                        (if (hash-table-get *object-pool* candidate #f)
                            (loop)
                            candidate))))
        (cond ((hash-table-get *object-pool* key #f)
               => (lambda (k)
                    (set! (cdr k) (assq-set! (cdr k) '%ctime (sys-time)))
                    k))
              (else
               (let1 newobj `(,key (%ctime . ,(sys-time)))
                 (hash-table-put! *object-pool* key newobj)
                 newobj)))))
    ))

(define (ref-object key)
  (and (string? key)
       (with-locking-mutex *mutex* (cut hash-table-get *object-pool* key #f))))

(define (sweep-objects timeout)
  (define now (sys-time))
  (define (check k v)
    (let1 ctime (assq-ref (cdr v) '%ctime)
      (when (and ctime (< ctime (- now timeout)))
        (hash-table-delete! *object-pool* k))))
  (with-locking-mutex *mutex*
    (cut hash-table-for-each *object-pool* check)))

(define (num-objects)
  (with-locking-mutex *mutex* (cut hash-table-num-entries *object-pool*)))

(define (all-keys)
  (with-locking-mutex *mutex* (cut hash-table-keys *object-pool*)))

;; Local variables:
;; mode: scheme
;; end:
