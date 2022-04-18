;; -*- coding: utf-8 ; mode: scheme -*-
;; test supervisor scripts.
;; this test isn't for modules, but for actual scripts.

(use gauche.test)
(use gauche.process)
(use gauche.net)
(use file.util)
(use text.tree)
(use sxml.ssax)
(use sxml.sxpath)
(use kahua.config)
(use kahua.gsid)
(use kahua.test.util)

(test-start "supervisor script")

(define *site* "_site")
(define-constant *prompt* "kahua> ")

(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(for-each make-directory*
          (list #`",|*site*|/app/lister"
                #`",|*site*|/app/greeting"
                #`",|*site*|/app/hello"
                #`",|*site*|/app/ss1"
                #`",|*site*|/app/ss2"))
(copy-file "../plugins/allow-module.scm" #`",|*site*|/plugins/allow-module.scm")

(for-each (apply$ (lambda (m d)
                    (copy-file #`",|m|.kahua" #`",|*site*|/app/,|d|/,|d|.kahua")))
          '(("lister" "lister")
            ("greeting" "greeting")
            ("hello-world" "hello")
            ("sharedstate" "ss1")
            ("sharedstate" "ss2")))

(define *spvr* #f)
(define *gsid* #f)
(define *app-servers* #`",|*site*|/app-servers")

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
(with-output-to-file *app-servers*
  (lambda ()
    (write '((hello       :run-by-default 1)
             (greeting    :run-by-default 0)
             (lister      :run-by-default 0)))))

;;-----------------------------------------------------------
(test-section "basic functionality")

(test* "start" #t
       (receive (p prompt) (kahua:invoke&wait `("../src/kahua-spvr" "--test" "-S" ,*site* "-i") :prompt *prompt*)
         (set! *spvr* p)
         (let1 path #`",|*site*|/socket/kahua"
           (and (string=? *prompt* (string-incomplete->complete prompt))
                (file-exists? path)
                (or (eq? (file-type path) 'socket)
                    (eq? (file-type path) 'fifo))))))

(test* "listener" #t
       (let ((out (process-input *spvr*))
             (in  (process-output *spvr*)))
         (write '(is-a? *spvr* <kahua-spvr>) out)
         (newline out)
         (flush out)
         (read in))) ;; result

;;-----------------------------------------------------------
(test* "greeting service via kahua-server (start)" #t
       (let* ((out (process-input *spvr*))
              (in  (process-output *spvr*))
              )
         (read in) ;; prompt
         (write '(begin (run-worker *spvr* 'greeting) #t) out)
         (newline out)
         (flush out)
         (read in))) ;; result

(test* "hello-world session initiation" #f ;; non-sense
       (send&receive
        '(("x-kahua-worker" "greeting"))
        "greeting"
        (lambda (header body)
          (receive (stat-gsid cont-gsid) (get-gsid-from-header header)
            (set! *gsid* (list stat-gsid cont-gsid))
            (and (string? stat-gsid)
                 (string? cont-gsid)
                 body)))))

;;-----------------------------------------------------------
(test-section "spvr protocol")

(test* "ls" '(hello greeting)
       (send&receive
        '(("x-kahua-worker" "spvr")) '(ls)
        (lambda (header body)
          (map (cut get-keyword :worker-type <> #f) body))))

(test* "types" '(hello greeting lister)
       (send&receive
        '(("x-kahua-worker" "spvr")) '(types)
        (lambda (header body) body)))

(test* "kill" '(greeting)
       (send&receive
        '(("x-kahua-worker" "spvr")) '(kill hello)
        (lambda (header body)
          (map (cut get-keyword :worker-type <> #f) body))))

(with-output-to-file *app-servers*
  (lambda ()
    (write '())))

(test* "reload" '()
       (send&receive
        '(("x-kahua-worker" "spvr")) '(reload)
        (lambda (header body) body)))

(test* "types after reload" '()
       (send&receive
        '(("x-kahua-worker" "spvr")) '(types)
        (lambda (header body) body)))

(with-output-to-file *app-servers*
  (lambda ()
    (write '(a b))))

(test* "reload malformed" #f
       (send&receive
        '(("x-kahua-worker" "spvr")) '(reload)
        (lambda (header body) body)))

(with-output-to-file *app-servers*
  (lambda ()
    (write '((lister :run-by-default 0)))))

(test* "reload again" '(lister)
       (send&receive
        '(("x-kahua-worker" "spvr")) '(reload)
        (lambda (header body) body)))

(with-output-to-file *app-servers*
  (lambda ()
    (write '((lister :run-by-default 2)))))

(test* "reload and check kicked" '(greeting lister lister)
       (begin
         (send&receive
          '(("x-kahua-worker" "spvr")) '(reload)
          (lambda (header body) body))
         (send&receive
          `(("x-kahua-worker" "spvr")) '(ls)
          (lambda (header body)
            (map (cut get-keyword :worker-type <> #f) body)))))

(test* "run" '(lister)
       (send&receive
        '(("x-kahua-worker" "spvr")) '(run lister)
        (lambda (header body)
          (map (cut get-keyword :worker-type <> #f) body))))

(test* "run" '(greeting lister lister lister)
       (send&receive
        '(("x-kahua-worker" "spvr")) '(ls)
        (lambda (header body)
          (map (cut get-keyword :worker-type <> #f) body))))

;;-----------------------------------------------------------
(test-section "sharing state")

(with-output-to-file *app-servers*
  (lambda ()
    (write '((ss1 :run-by-default 1)
             (ss2 :run-by-default 1)))))

(test* "start ss1&ss2" '(ss1 ss2)
       (begin
         (send&receive '(("x-kahua-worker" "spvr")) '(kill *) (lambda _ #f))
         (send&receive '(("x-kahua-worker" "spvr")) '(reload) (lambda _ #f))
         (send&receive '(("x-kahua-worker" "spvr")) '(ls)
                       (lambda (header body)
                         (map (cut get-keyword :worker-type <> #f) body)))))

(let ()
  (define *sgsid* #f)
  (define (get-ss-li body)
    ((sxpath '(// li))
     (call-with-input-string (tree->string body)
       (cut ssax:xml->sxml <> '()))))

  (test* "show ss1" ()
         (send&receive '(("x-kahua-worker" "ss1"))
                       '()
                       (lambda (header body)
                         (receive (sgsid cgsid) (get-gsid-from-header header)
                           (set! *sgsid* sgsid))
                         (get-ss-li body))))

  (test* "set ss1" '((li "foo=bar"))
         (send&receive `(("x-kahua-worker" "ss1")
                         ("x-kahua-cgsid" "set")
                         ("x-kahua-sgsid" ,*sgsid*)
                         ("x-kahua-path-info" ("kahua.cgi" "set" "foo" "bar")))
                       '()
                       (lambda (header body)
                         (get-ss-li body))))

  (test* "show ss2" '((li "foo=bar"))
         (send&receive `(("x-kahua-worker" "ss2")
                         ("x-kahua-sgsid" ,*sgsid*))
                       '()
                       (lambda (header body)
                         (get-ss-li body))))

  (test* "set ss2" '((li "foo=baz"))
         (send&receive `(("x-kahua-worker" "ss2")
                         ("x-kahua-cgsid" "set")
                         ("x-kahua-sgsid" ,*sgsid*)
                         ("x-kahua-path-info" ("kahua.cgi" "set" "foo" "baz")))
                       '()
                       (lambda (header body)
                         (get-ss-li body))))

  (test* "set/show ss1" '((li "boo=bee") (li "foo=baz"))
         (send&receive `(("x-kahua-worker" "ss1")
                         ("x-kahua-cgsid" "set")
                         ("x-kahua-sgsid" ,*sgsid*)
                         ("x-kahua-path-info" ("kahua.cgi" "set" "boo" "bee")))
                       '()
                       (lambda (header body)
                         (get-ss-li body))))

  (test* "show ss2" '((li "boo=bee") (li "foo=baz"))
         (send&receive `(("x-kahua-worker" "ss2")
                         ("x-kahua-sgsid" ,*sgsid*))
                       '()
                       (lambda (header body)
                         (get-ss-li body))))
  )

;; -----------------------------------------------------------
(test* "shutdown" '()
       (begin
         (process-send-signal *spvr* SIGTERM)
         (process-wait *spvr*)
         (directory-list #`",|*site*|/socket" :children? #t)))

(test-end)
