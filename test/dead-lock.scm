;; -*- coding: utf-8 ; mode: scheme -*-
;; test supervisor's dead-lock

(use srfi-1)
(use gauche.test)
(use gauche.process)
(use gauche.threads)
(use gauche.net)
(use file.util)
(use text.tree)
(use sxml.ssax)
(use sxml.sxpath)
(use kahua.config)
(use kahua.gsid)

(test-start "dead-lock")

(define *site* "_site")

(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(make-directory* #`",|*site*|/app/many-as")
(copy-file "many-as.kahua" #`",|*site*|/app/many-as/many-as.kahua")
(copy-file "../plugins/allow-module.scm" #`",|*site*|/plugins/allow-module.scm")
(define *spvr* #f)
(define *gsid* #f)

(kahua-common-init *site* #f)

;; some utilities
(define (send-message out header body)
  (write header out) (newline out)
  (write body out)   (newline out)
  (flush out))

(define (receive-message in)
  (let* ((header (read in))
         (body   (read in)))
    (values header body)))

(define (send&receive header body receiver)
  (call-with-client-socket
      (make-client-socket (supervisor-sockaddr (kahua-sockbase)))
    (lambda (in out)
      (send-message out header body)
      (call-with-values (cut receive-message in) receiver))))

;; prepare app-servers file
(with-output-to-file #`",|*site*|/app-servers"
  (lambda ()
    (write '((many-as :run-by-default 0)))))

;;-----------------------------------------------------------
(test-section "starting spvr")

(test* "start" #t
       (let* ((p (run-process "../src/kahua-spvr" "--test" "-S" *site* "-i"
                              :input :pipe :output :pipe))
              )
         (set! *spvr* p)
         (sys-sleep 2) ;; give the spvr time to set up...
	 (let1 socket-path #`",|*site*|/socket/kahua"
	   (and (file-exists? socket-path)
		(or (eq? (file-type socket-path) 'socket)
		    (eq? (file-type socket-path) 'fifo))))))

;;-----------------------------------------------------------
(test-section "starting worker")

(test* "many-as start" #t
       (let* ((out (process-input *spvr*))
              (in  (process-output *spvr*))
              )
         (read in) ;; prompt
         (write '(begin (run-worker *spvr* 'many-as) #t) out)
         (newline out)
         (flush out)
         (read in))) ;; result

;;-----------------------------------------------------------
(test-section "many-as.kahua")

(define (get-as body)
  ((sxpath '(// p))
   (call-with-input-string (tree->string body)
     (cut ssax:xml->sxml <> '()))))

;(sys-sleep 60) ;; Uncomment, if you want time to run strace

(test* "show response length" 1000000
       (string-length 
	(cadar
	 (send&receive '(("x-kahua-worker" "many-as"))
		       '()
		       (lambda (header body)
			 (receive (sgsid cgsid) (get-gsid-from-header header)
			   #f)
			 (get-as body))))))


(test* "show response length" 1000000
       (string-length
	(cadar
	 (begin
	   (for-each 
	    thread-start!
	    (list-tabulate 
	     10
	     (lambda (_)
	       (make-thread
		(lambda ()
		  (send&receive '(("x-kahua-worker" "many-as"))
				'()
				(lambda (header body)
				  (receive (sgsid cgsid) (get-gsid-from-header header)
				    #f)
				  (get-as body))))))))
	   (send&receive '(("x-kahua-worker" "many-as"))
			 '()
			 (lambda (header body)
			   (receive (sgsid cgsid) (get-gsid-from-header header)
			     #f)
			   (get-as body)))))))

(test* "shutdown" '()
       (begin
         (process-send-signal *spvr* SIGTERM)
	 (process-wait *spvr*)
         (directory-list #`",|*site*|/socket" :children? #t)))

(test-end)
