;;
;; generic framework to test worker scripts
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: worker.scm,v 1.4 2004/02/09 06:48:56 shiro Exp $

;; A convenience module to test worker scripts.
;; You can spawn a worker script as a subprocess and communicate with it.

(define-module kahua.test.worker
  (use srfi-2)
  (use gauche.test)
  (use gauche.process)
  (use gauche.net)
  (use text.tree)
  (use rfc.uri)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use util.list)
  (use kahua.config)
  (use kahua.gsid)
  (use kahua.test.xml)
  (export run-worker worker-running?
          call-worker call-worker/gsid call-worker/gsid->sxml
          reset-gsid shutdown-worker with-worker
          make-match&pick make-match&pick)
  )
(select-module kahua.test.worker)

(define-class <worker-subprocess> ()
  ((worker-id      :init-keyword :worker-id)
   (worker-process :init-keyword :worker-process)
   (state-sid      :init-value #f)
   (cont-sid       :init-value #f)
   (path-info      :init-value #f)
   ))

(define (run-worker command)
  (let* ((p  (apply run-process (append command '(:output :pipe))))
         (id (read-line (process-output p))))
    (make <worker-subprocess>
      :worker-id id :worker-process p)))

(define-method worker-running? ((worker <worker-subprocess>))
  (string? (ref worker 'worker-id)))

(define-method call-worker ((worker <worker-subprocess>) header body proc)
  (let* ((worker-id (ref worker 'worker-id))
         (sock (make-client-socket
                (worker-id->sockaddr worker-id (kahua-sockbase)))))
    (call-with-client-socket sock
      (lambda (in out)
        (write header out) (newline out)
        (write body out)   (newline out)
        (let* ((header (read in))
               (body   (read in)))
          (proc header body))))))

(define-method call-worker/gsid ((worker <worker-subprocess>) header body proc)
  (call-worker worker
               (add-gsid-to-header (append
                                    (cond-list ((ref worker 'path-info)
                                                `("x-kahua-path-info"
                                                  ,(ref worker 'path-info))))
                                    header)
                                   (ref worker 'state-sid)
                                   (ref worker 'cont-sid))
               body
               (lambda (header body)
                 (set!-values ((ref worker 'state-sid)
                               (ref worker 'cont-sid))
                              (get-gsid-from-header header))
                 (proc header body))))

(define-method call-worker/gsid->sxml
  ((worker <worker-subprocess>) header body . maybe-sxpath)
  (call-worker/gsid worker header body
                    (lambda (h b)
                      (let1 r (call-with-input-string (tree->string b)
                                (cut ssax:xml->sxml <> '()))
                        (cond ((get-optional maybe-sxpath #f)
                               => (lambda (path)
                                    (cons '*TOP* ((sxpath path) r))))
                              (else r))))))

(define-method reset-gsid ((worker <worker-subprocess>))
  (set! (ref worker 'state-sid) #f)
  (set! (ref worker 'cont-sid) #f))

(define-method shutdown-worker ((worker <worker-subprocess>))
  (and-let* ((p (ref worker 'worker-process)))
    (set! (ref worker 'worker-process) #f)
    (set! (ref worker 'worker-id) #f)
    (process-send-signal p SIGINT)
    (process-wait p)))

(define-syntax with-worker
  (syntax-rules ()
    ((with-worker (w command) body ...)
     (let ((w (run-worker command)))
       (with-error-handler
           (lambda (e) (shutdown-worker w) (raise e))
         (lambda () body ... (shutdown-worker w)))))))

;; Returns a procedure that can be used as the fourth arg of test*
;; to match the resulting xml, as well as to save the session id
;; matched to a pattern variable ?&.

(define-method make-match&pick ((worker <worker-subprocess>))
  (define (pick matches)
    (and-let* ((p (assoc-ref matches '?&)))
      ;; cut parameter from url
      (cond ((#/\?x-kahua-cgsid=([^#&]*)/ p)
             => (lambda (m) (uri-decode-string (m 1))))
            ((#/kahua.cgi\/(.+?)\/(.+)/ p)
             => (lambda (m) (uri-decode-string (m 2)))) ;; path_info
            ((#/kahua.cgi\/(.+?)/ p) #f)                    ;; to top
            (else p))))
  (define (save p)
    (receive (cgsid xtra-path) (string-scan (or p "") "/" 'both)
      (set! (ref worker 'cont-sid) (or cgsid p))
      (set! (ref worker 'path-info)
            (and xtra-path
                 (list* "dummy" "dummy" (string-split xtra-path "/"))))))
  (lambda (pattern input)
    (if (and (pair? input)
             (symbol? (car input)))
      (test-sxml-match? pattern input (compose save pick))
      (test-xml-match? pattern input (compose save pick)))))

(provide "kahua/test/worker")
