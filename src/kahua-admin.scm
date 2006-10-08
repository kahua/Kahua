;; Administrative script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-admin.scm,v 1.7 2006/10/08 07:13:27 bizenn Exp $

(use srfi-1)
(use gauche.net)
(use gauche.parseopt)
(use gauche.parameter)
(use util.list)
(use kahua)
(use file.util)
(use gauche.process)

(define last-command (make-parameter ""))
(define *redo-command* ":")

;; Deal with supervisor command -----------------------------------
(define (spvr-command-processor)
  (with-error-handler
      (lambda (e)
        (print "ERROR: " (ref e 'message))
        spvr-command-processor)
    (lambda ()
      (format #t "spvr> ")
      (flush)
      (let1 line (read-line)
        (if (equal? line *redo-command*)
	    (set! line (last-command)) ;; use last command
	    (last-command line))       ;; set last command
        (if (eof-object? line)
          (exit 0)
          (let1 cmd (call-with-input-string line port->sexp-list)
            (if (null? cmd)
              spvr-command-processor
              (dispatch-spvr-command cmd))))))))

(define (dispatch-spvr-command cmd)
  (case (car cmd)
    ((help)
     (let ((spvr-help (send-command #f cmd))
           (admin-help '(connect cvs
                                 adduser deluser lsuser moduser
                                 plugin)))
       (write (append spvr-help admin-help)) (newline)
       spvr-command-processor))
    ((connect)
     (if (= (length cmd) 2)
       (connect-worker (cadr cmd))
       (begin (display "Usage: connect <worker-number>\n")
              spvr-command-processor)))
    ((update)
     (if (> (length cmd) 1)
       (apply update-command (cdr cmd))
       (begin (display 
               "Usage: update <worker-type/number> (files...)\n")
              spvr-command-processor)))

    ;; user management
    ((adduser)
     (if (>= (length cmd) 3)
       (let ((name (symbol->string (cadr cmd)))

             (pw (symbol->string (caddr cmd)))
             (roles (cdddr cmd)))
         (kahua-add-developer name pw roles)
         (write 'done) (newline)
         spvr-command-processor)
       (begin
         (display "Usage: adduser <name> <password> <roles>...\n")
         spvr-command-processor)))
    ((deluser)
     (if (= (length cmd) 2)
       (let1 name (symbol->string (cadr cmd))
         (kahua-delete-developer name)
         (write 'done) (newline)
         spvr-command-processor)
       (begin (display "Usage: deluser <name>\n")
              spvr-command-processor)))
    ((moduser)
     (if (and (= (length cmd) 4) (eq? (cadr cmd) 'password))
       (begin
         (apply kahua-change-developer-password
                (map symbol->string (cddr cmd)))
         (write 'done) (newline)
         spvr-command-processor)
       (begin
         (display "Usage: moduser password <name> <new>\n")
         spvr-command-processor)))
    ((lsuser)
     (write (kahua-list-developer)) (newline)
     spvr-command-processor)

    ;; plugin management
    ((plugin)
     (plugin-command (cdr cmd))
     spvr-command-processor)
    (else
     (let1 reply (send-command #f cmd)
       (case (car cmd)
         ((ls run kill) (ls-result reply))
         (else (write reply) (newline)))
       spvr-command-processor)
     )))

(define (ls-result reply)
  (define now (sys-time))
  (define (show-time time)
    (sys-strftime "%b %e %H:%M" (sys-localtime time)))
  
  (format #t "wno   pid type         since        wid\n")
  (dolist (w reply)
    (format #t "~3d ~5d ~12a ~10a ~a\n"
            (get-keyword :worker-count w)
            (get-keyword :worker-pid w)
            (get-keyword :worker-type w)
            (show-time (get-keyword :start-time w))
            (get-keyword :worker-id w)))
  )

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
        spvr-command-processor))))

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
           ((memq expr '(disconnect bye)) spvr-command-processor)
           (else
            ;; NB: the first two elts of reply is error-output and std-output
            (let1 reply (send-command wid expr)
              (display (car reply)) (display (cadr reply))
              (for-each (lambda (r) (display r) (newline)) (cddr reply))
              worker-processor))
           ))
        ))
    ))


(define (update-worker-files wtype . files)
  (let* ((workers (send-command #f '(ls)))
         (the-worker (find (lambda (w)
                             (eqv? (get-keyword :worker-type w) wtype))
                           workers)))
    (if the-worker
	(begin
	  (display (caddr (send-command (get-keyword :worker-id the-worker) 
				      '(update-server))))
	  (newline))
        (format #t "No such worker: ~a\n" wtype)))
  spvr-command-processor)

(define (update-command t/c . files)
  (let* ((workers (send-command #f '(ls)))
	 (targets (filter
		   (lambda (w)
		     (or (eqv? (get-keyword :worker-type w) t/c)
			 (eqv? (get-keyword :worker-count w) t/c)))
		   workers)))
    (if (null? targets)
	  (format #t "No such worker: ~a\n" t/c)
	  (for-each (lambda (w)
		      (let* ((wid   (get-keyword :worker-id w))
			     (wtype (get-keyword :worker-type w))
			     (fs    (map (cut symbol->string <>) files))
			     (ans   (send-command wid
						  `(update-server ,@fs))))
			(if (equal? (caddr ans) "#f")
			    (begin
			      (format #t "update failed: ~a(~a)\n" wtype wid)
			      (format #t "~a\n" (car ans)))
			    (format #t "update: ~a(~a)\n" wtype wid))))
		    targets))
    spvr-command-processor))

;; Deal with plugin command ------------------------------------

(define (plugin-command cmd)
  (define (show-plugin wid)
    (let* ((reply (send-command wid '(all-plugins)))
           (plugins (sort
                    (read-from-string (caddr reply))
                    (lambda (x y) (string<? (car x) (car y))))))
      (format #t "name                 version\n")
      (dolist (p plugins)
              (format #t "~20a ~a\n"
                      (car p)
                      (cdr p)))
      ))
  (let* ((workers (or (send-command #f '(ls)) (error "No worker found")))
         (wid (get-keyword :worker-id
                           (if (null? cmd)
                               (car workers)
                               (or (find (lambda (w)
                                           (eqv? (get-keyword :worker-count w)
                                                 (car (reverse cmd))))
                                         workers)
                                   (error "No such worker" (car cmd)))))))
    (cond ((<= (length cmd) 1)
           ;; show plugin list
           (show-plugin wid))
          ((and (= (length cmd) 2)
                (equal? (car cmd) 'reload))
           ;; reload plugins
           (send-command wid '(initialize-plugins))
           (format #t "plugin reloaded: ~a\n" (cadr cmd))
           (show-plugin wid))
          (else (error "unknown plugin command" cmd)))
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

;; Entry -------------------------------------------------------
(define (main args)
  (let-args (cdr args)
      ((site "S=s")
       (conf-file "c=s")
       (gosh      "gosh=s")  ;; wrapper script adds this.  ignore.
       (help      "h|help" => (cut usage))
       . args)
    (set-signal-handler! SIGINT  (lambda _ (exit 0)))
    (set-signal-handler! SIGTERM (lambda _ (exit 0)))
    (kahua-common-init site conf-file)
    (cond
     ((null? args) ;; interactive mode
      (let loop ((command-processor spvr-command-processor))
        (loop (command-processor))))
     (else         ;; batch mode
      (dispatch-spvr-command
       (call-with-input-string (string-join args " ") port->sexp-list))
      (exit 0)))))

(define (usage)
  (print "kahua-admin [-S=site] [-c=conf-file] [command ...]")
  (exit 0))

;; Local variables:
;; mode: scheme
;; end:
