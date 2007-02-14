;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Provides worker<->client protocol.
;;
;;  Copyright (c) 2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: worker.scm,v 1.4.2.1 2007/01/12 08:32:28 bizenn Exp $
(define-module kahua.protocol.worker
  (use util.list)
  (use gauche.net)
  (use gauche.logger)
  (use kahua.gsid)
  (use kahua.config)
  (use kahua.util)
  (export talk-to-worker
	  add-kahua-header!
	  kahua-worker-header
	  <kahua-worker-not-found>
	  <kahua-worker-not-respond>
	  <kahua-worker-unknown-error>
	  <kahua-spvr-session-expired>
	  kahua-worker-not-found?
	  kahua-worker-not-respond?
	  kahua-worker-unknown-error?
	  kahua-spvr-session-expired?)
  )

(select-module kahua.protocol.worker)

(define-condition-type <kahua-worker-unknown-error> <kahua-error> kahua-worker-unknown-error?)
(define-condition-type <kahua-worker-not-found> <kahua-error> kahua-worker-not-found?)
(define-condition-type <kahua-worker-not-respond> <kahua-error> kahua-worker-not-respond?)
(define-condition-type <kahua-spvr-session-expired> <kahua-error> kahua-spvr-session-expired?)

(define (add-kahua-header! header . args)
  (let loop ((h header)
	     (pairs args))
    (if (null? pairs)
	h
	(let ((key (car pairs))
	      (value (cadr pairs)))
	  (if value
	      (loop (assoc-set! h key (list value)) (cddr pairs))
	      (loop h (cddr pairs)))))))

(define (kahua-worker-header worker path-info . args)
  (let-keywords* args ((server-uri #f)
		       (worker-uri #f)
		       (metavariables #f)
		       (sgsid #f)
		       (cgsid #f)
		       (remote-addr #f)
		       (bridge #f))
    (reverse!
     (add-kahua-header! '()
			"x-kahua-worker"        worker
			"x-kahua-path-info"     path-info
			"x-kahua-sgsid"         sgsid
			"x-kahua-cgsid"         cgsid
			"x-kahua-worker-uri"    worker-uri
			"x-kahua-server-uri"    server-uri
			"x-kahua-bridge"        bridge
			"x-kahua-remote-addr"   remote-addr
			"x-kahua-metavariables" metavariables
			))))

;; Too ugly. FIXME!!
(define (check-kahua-status kheader kbody)
  (or (and-let* ((kahua-status (assoc-ref kheader "x-kahua-status")))
	(if (null? kahua-status)
	    (error <kahua-worker-unknown-error> "Unknown worker error")
	    (case (string->symbol (car kahua-status))
	      ((OK)         #t)
	      ((SPVR-ERROR)
	       (if (null? (cdr kahua-status))
		   (error <kahua-worker-unknown-error> "Unknown worker error")
		   (case (cadr kahua-status)
		     ((<kahua-worker-not-found>) (error <kahua-worker-not-found> "Worker not found"))
		     ((<kahua-worker-not-respond>) (error <kahua-worker-not-respond> "Worker not respond"))
		     (else (error <kahua-worker-unknown-error> "Unknown worker error")))))
	      (else
	       (log-format "Unknown x-kahua-status: ~s" kahua-status)
	       (error <kahua-worker-unknown-error> "Unknown worker error")))))
      #t))

(define (make-socket-to-worker cgsid)
  (make-client-socket (worker-id->sockaddr (and cgsid (gsid->worker-id cgsid)) (kahua-sockbase))))

(define (talk-to-worker cgsid header params)
  (guard (e ((not (kahua-error? e))
	     (cond ((slot-exists? e 'message)
		    (log-format "Error: ~a ~s" (class-name (class-of e)) (slot-ref e 'message)))
		   (else (log-format "Error: ~s" e)))
	     (error <kahua-worker-not-found> "Worker not found")))
    (call-with-client-socket (make-socket-to-worker cgsid)
      (lambda (w-in w-out)
	(log-format "C->W header: ~s" header)
	(log-format "C->W params: ~s" params)
	(write header w-out)
	(write params w-out)
	(flush w-out)
	(let* ((w-header (read w-in))
	       (w-body   (read w-in)))
	  (log-format "C<-W header: ~s" w-header)
	  (check-kahua-status w-header w-body)
	  (values w-header w-body))))))

(provide "kahua/protocol/worker")
