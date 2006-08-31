;; -*- mode: scheme; coding: utf-8 -*-
;;
;; Provides worker<->client protocol.
;;
;;  Copyright (c) 2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: worker.scm,v 1.2 2006/08/31 04:46:58 bizenn Exp $
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
	  <kahua-worker-error>)
  )

(select-module kahua.protocol.worker)

(define-condition-type <kahua-worker-error> <kahua-error> #f)
(define-condition-type <kahua-worker-not-found> <kahua-error> #f)

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

(define (check-kahua-status kheader kbody)
  (or (and-let* ((kahua-status (assoc-ref-car kheader "x-kahua-status")))
	(case (string->symbol kahua-status)
	  ((OK)         #t)
	  ((SPVR-ERROR) (raise (make-condition <kahua-worker-not-found> 'message "Worker not found")))
	  (else         (raise (make-condition <kahua-worker-error> 'message "Worker error")))))
      #t))

(define (make-socket-to-worker cgsid)
  (make-client-socket (worker-id->sockaddr (and cgsid (gsid->worker-id cgsid)) (kahua-sockbase))))

(define (talk-to-worker cgsid header params)
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
	(values w-header w-body)))))

(provide "kahua/protocol/worker")
