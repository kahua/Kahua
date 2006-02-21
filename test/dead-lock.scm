;; -*- coding: euc-jp ; mode: scheme -*-
;; test supervisor's dead-lock
;; $Id: dead-lock.scm,v 1.2 2006/02/21 14:24:33 nobsun Exp $

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

(sys-system "rm -rf _tmp _work")
(for-each make-directory*
          '("_tmp" 
            "_work/checkout/many-as"
            "_work/plugins"))

(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")
(copy-file "./many-as.kahua"      "./_work/checkout/many-as/many-as.kahua")

(define *config* "./test.conf")
(define *spvr* #f)
(define *gsid* #f)

(kahua-init *config*)

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
(with-output-to-file "_work/app-servers"
  (lambda ()
    (write '((many-as :run-by-default 0)))))

;;-----------------------------------------------------------
(test-section "starting spvr")

(test* "start" #t
       (let* ((p (run-process "../src/kahua-spvr" "--test"
                              "-c" *config* "-i"
                              :input :pipe :output :pipe))
              )
         (set! *spvr* p)
         (sys-sleep 2) ;; give the spvr time to set up...
         (and (file-exists? "_tmp/kahua")
	      (or (eq? (file-type "_tmp/kahua") 'socket)
                  (eq? (file-type "_tmp/kahua") 'fifo)))))

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
         (sys-sleep 5) ;; give the spvr time to shutdown ...
         (directory-list "_tmp" :children? #t)))

(test-end)
