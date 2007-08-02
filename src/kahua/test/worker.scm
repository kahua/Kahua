;;
;; generic framework to test worker scripts
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

;; A convenience module to test worker scripts.
;; You can spawn a worker script as a subprocess and communicate with it.

(define-module kahua.test.worker
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use gauche.test)
  (use gauche.process)
  (use gauche.net)
  (use text.tree)
  (use rfc.uri)
  (use www.cgi)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use util.list)
  (use kahua.config)
  (use kahua.gsid)
  (use kahua.test.xml)
  (export run-worker worker-running?
          call-worker call-worker/gsid call-worker/gsid->sxml
          reset-gsid shutdown-worker with-worker set-gsid
          make-match&pick header->sxml test/send&pick)
  )
(select-module kahua.test.worker)

(define-class <worker-subprocess> ()
  ((worker-id      :init-keyword :worker-id)
   (worker-process :init-keyword :worker-process)
   (sessions       :init-value (make-hash-table 'string=?))
   (state-sid      :init-value #f)
   (cont-sid       :init-value #f)
   (path-info      :init-value #f)
   (query          :init-value '())
   ))

(define-class <session> ()
  ((state-sid      :init-value #f)
   (cont-sid       :init-value #f)
   (path-info      :init-value #f)
   (query          :init-value '())))


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
	(flush out)
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
               (append body (ref worker 'query))
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

(define-method set-gsid ((worker <worker-subprocess>) (id <string>))
  (test* (format "set-gsid to: ~s" id) #t
         (let1 session (ref (ref worker 'sessions) id)
           (for-each (lambda (slot)
                       (slot-set! worker slot (ref session slot)))
                     (map slot-definition-name (class-slots <session>)))
           #t)))

(define-method set-gsid ((worker <worker-subprocess>) (id <symbol>))
  (set-gsid worker (symbol->string id)))

(define-method shutdown-worker ((worker <worker-subprocess>))
  (and-let* ((p (ref worker 'worker-process)))
    (set! (ref worker 'worker-process) #f)
    (set! (ref worker 'worker-id) #f)
    (process-send-signal p SIGTERM)
    (process-wait p)))

(define-syntax with-worker
  (syntax-rules ()
    ((_ (w command) body ...)
     (let1 w (run-worker command)
       (unwind-protect
	(begin body ...)
	(shutdown-worker w))))))

;; Returns a procedure that can be used as the fourth arg of test*
;; to match the resulting xml, as well as to save the session id
;; matched to a pattern variable ?&.

(define-method make-match&pick ((worker <worker-subprocess>))
  ;; matches -> ((name . (path . param)) ...)
  (define (pick matches)
    ;; match -> (path . param)
    (define (get-path&param p)
      ;; cut parameter from url
      (cond ((#/\?x-kahua-cgsid=([^#&]*)/ p)
             => (lambda (m) (cons (uri-decode-string (m 1)) "")))
            ((#/kahua.cgi\/(.+?)\/([^\?]+)\??(.*)/ p)
             => (lambda (m) (cons (uri-decode-string (m 2)) (m 3)))) ;; path_info
            ((#/kahua.cgi\/(.+?)/ p) (cons #f ""))                    ;; to top
            (else (cons p ""))))

    (filter-map (lambda (match-pair)
                  (let1 key (x->string (car match-pair))
                    (and (string-prefix? "?&" key)
                         (cons (string-drop key 2)
                               (get-path&param (cdr match-pair))))))
                matches))

  (define (save ps)
    (define (save-session path&query session)
      (let ((p (car path&query))
            (query (cdr path&query)))
        (receive (cgsid xtra-path) (string-scan (or p "") "/" 'both)
	  (set! (ref session 'state-sid) (ref worker 'state-sid))
          (set! (ref session 'cont-sid) (or cgsid p))
          (set! (ref session 'path-info)
              (and xtra-path
                   (filter (compose not string-null?)
                           (list* "dummy" "dummy" (string-split xtra-path #[/])))))
          (set! (ref session 'query)
              (cgi-parse-parameters :query-string query)))
        session))

    (save-session (or (assoc-ref ps "") '(#f . "")) worker)
    (for-each (lambda (p)
                (let1 key (car p)
                  (unless (equal? key "")
                    (hash-table-put! (ref worker 'sessions)
                                     key
                                     (save-session (cdr p) (make <session>))))))
              ps))

  (lambda (pattern input)
    (if (and (pair? input)
             (symbol? (car input)))
      (test-sxml-match? pattern input (compose save pick))
      (test-xml-match? pattern input (compose save pick)))))

;; check 'http header' and save 'continuation session id' to worker.
;;
;; (test* "redirect header test"
;;        '(*TOP* (!contain (Status "302 Found")
;;                            (Location ?&)))
;;        (call-worker/gsid
;;         w
;;         '()
;;         '(("login-name" "shibata") ("passwd" "hogehoge"))
;;         header->sxml)
;;        (make-match&pick w))
;;
;; == (test/send&pick "redirect header test" w
;;                 '(("login-name" "shibata") ("passwd" "hogehoge")))


;; '(("Status" "302 Found") ("Location" "http://localho..")) (html..)
;; => '(*TOP* (Status "302 Found") (Location "http://localho.."))
(define (header->sxml h b)
  (define (url-fragment-cutoff pair)
    (cond ((and (eq? (car pair) 'Location)
	     (string-scan (cadr pair) #\# 'before))
	   => (lambda (url)
		(list (car pair) url)))
	  (else pair)))
  (cons '*TOP*
        (map (lambda (item)
               (url-fragment-cutoff
		(cons (string->symbol (car item))
		      (cdr item))))
             h)))

(define (test/send&pick label w send-data)
  (test* label
         '(*TOP* (!contain (Status "302 Found")
                           (Location ?&)))
         (call-worker/gsid
          w
          '()
          send-data
          header->sxml)
         (make-match&pick w)))


(provide "kahua/test/worker")
