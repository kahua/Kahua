;; Interactive shell script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-shell.scm,v 1.3 2005/12/22 11:22:33 cut-sea Exp $

(use srfi-1)
(use gauche.net)
(use gauche.parseopt)
(use util.list)
(use kahua)
(use gauche.process)
(use gauche.termios)
(use kahua.developer)


;; Deal with login -----------------------------------
(define (login-processor)
    (with-error-handler
     (lambda (e)
       (print "ERROR: " (ref e 'message))
       login-processor)
     (lambda ()
       (display "Welcome to Kahua.") (newline)
       (let* ((user-mode (ref (kahua-config) 'user-mode))
              (username (or user-mode
                            (begin
                              (format #t "username: ")
                              (flush)
                              (read-line))))
              (password (if (sys-getenv "TERM")
                          (get-password "password: ")
                          (begin (format #t "password: ")
                                 (flush)
                                 (read-line)))))
         (cond ((find eof-object? (list username password)) (exit 0))
               ((kahua-check-developer username password)
                select-worker-processor)
               (else
                (newline)
                (display "Permission denied.")
                (newline)
                (exit 0)))))))


;; Deal with select workers ------------------------------------
(define (show-workers)
  (define (show-time time)
    (sys-strftime "%b %e %H:%M" (sys-localtime time)))
  (let ((workers (send-command #f '(ls))))
    (format #t "wno type         since        wid\n")
    (dolist (w workers)
            (format #t "~3d ~12a ~10a ~a\n"
                    (get-keyword :worker-count w)
                    (get-keyword :worker-type w)
                    (show-time (get-keyword :start-time w))
                    (get-keyword :worker-id w)))))

(define (select-worker-processor)
  (with-error-handler
      (lambda (e)
        (let ((errmsg (ref e 'message)))
          (print "ERROR: " errmsg)
          (if (#/connect failed to/ errmsg)
              (exit)
              select-worker-processor)))
    (lambda ()
      (show-workers)
      (format #t "select wno> ")
      (flush)
      (let1 line (read-line)
        (if (eof-object? line)
          (exit 0)
          (let1 cmd (call-with-input-string line port->sexp-list)
            (if (null? cmd)
              select-worker-processor
              (connect-worker (car cmd)))))))))


;; Deal with worker command ------------------------------------
(define (connect-worker wno)
  (let* ((workers (send-command #f '(ls)))
         (the-worker (find (lambda (w)
                             (eqv? (get-keyword :worker-count w) wno))
                           workers)))
    (if the-worker
      (make-worker-command-processor
       (get-keyword :worker-type the-worker)
       (get-keyword :worker-id the-worker))
      (begin
        (format #t "No such worker: ~a\n" wno)
        select-worker-processor))))

(define (make-worker-command-processor type wid)
  (rec (worker-processor)
    (with-error-handler
        (lambda (e)
          (display (ref e 'message)) (flush)
          worker-processor)
      (lambda ()
        (format #t "~a(~a)> " type wid)
        (flush)
        (let1 expr (read)
          (cond
           ((eof-object? expr) (exit 0))
           ((memq expr '(disconnect bye)) (read-line) select-worker-processor)
           (else
            ;; NB: the first two elts of reply is error-output and std-output
            (let1 reply (send-command wid `(eval ',expr kahua-app-server))
              (display (car reply)) (display (cadr reply))
              (for-each (lambda (r) (display r) (newline)) (cddr reply))
              worker-processor))
           ))
        ))
    ))


;; Utility -----------------------------------------------------
(define (send-command wid cmd)
  (let ((sockaddr (worker-id->sockaddr wid (kahua-sockbase))))
    (call-with-client-socket (make-client-socket sockaddr)
      (lambda (in out)
        (if wid
          (write '(("x-kahua-eval" "#t")) out)
          (write '(("x-kahua-worker" "spvr")) out))
        (newline out)
        (write cmd out)
        (newline out)
        (flush out)
        ;; special treatment of 'shutdown'-command: we won't get
        ;; reply from that command.
        (if (and (not wid) (eq? (car cmd) 'shutdown))
          '()
          (let* ((header (read in))
                 (body   (read in)))
            (if (equal? (assoc-ref header "x-kahua-status") '("OK"))
              body
              (errorf "~a" body))))))))

(define (get-password prompt)
  (let* ((port (current-input-port))
         (attr (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag)))
    ;; Show prompt
    (display prompt)
    (flush)
    ;; Turn off echo during reading.
    (dynamic-wind
        (lambda ()
          (slot-set! attr 'lflag (logand lflag (lognot ECHO)))
          (sys-tcsetattr port TCSAFLUSH attr))
        (lambda ()
          (read-line port))
        (lambda ()
          (slot-set! attr 'lflag lflag)
          (sys-tcsetattr port TCSANOW attr)))))

;; Entry -------------------------------------------------------
(define (main args)
  (let-args (cdr args)
      ((conf-file "c=s")
       (user      "user=s")
       (gosh      "gosh=s")  ;; wrapper script adds this.  ignore.
       )
    (set-signal-handler! SIGINT  (lambda _ (exit 0)))
    (set-signal-handler! SIGHUP  (lambda _ (exit 0)))
    (set-signal-handler! SIGTERM (lambda _ (exit 0)))
    (kahua-init conf-file :user user)
    (let loop ((command-processor login-processor))
      (loop (command-processor)))))

;; Local variables:
;; mode: scheme
;; end:
