;; -*- coding: utf-8 ; mode: scheme -*-
;; test shell scripts.
;; this test isn't for modules, but for actual scripts.
;; kahua-shell のテスト

(use gauche.test)
(use gauche.process)
(use gauche.net)
(use file.util)
(use kahua.config)
(use kahua.gsid)

(test-start "kahua-shell script")

;;---------------------------------------------------------------
(test-section "initialization")

(define *site* "_site")

(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(for-each (apply$ (lambda (m d)
		    (let1 destdir #`",|*site*|/app/,|d|"
		      (make-directory* destdir)
		      (copy-file #`",|m|.kahua" #`",|destdir|/,|d|.kahua"))))
	  '(("lister" "lister")
	    ("greeting" "greeting")
	    ("hello-world" "hello")))
(copy-file "../plugins/allow-module.scm" #`",|*site*|/plugins/allow-module.scm")
(copy-file "testuser.conf" #`",|*site*|/etc/user.conf" :if-exists :supersede)
(define *app-servers* #`",|*site*|/app-servers")
(define *spvr*   #f)
(define *shell*  #f)

(kahua-common-init *site* #f)

;; prepare app-servers file
(with-output-to-file *app-servers*
  (lambda ()
    (write '((hello    :run-by-default 1)
             (greeting :run-by-default 0)
             (lister   :run-by-default 0)
             ))))

;;---------------------------------------------------------------
;; kahua-shell のテストを開始する。
(test-section "run scripts")

;; kahua-shell と通信する kahua-spvr を起動する。
(test* "start spvr" #t
       (let ((p (run-process "../src/kahua-spvr" "--test" "-S" *site*)))
         (set! *spvr* p)
	 (sys-sleep 3)
	 (let1 socket-path #`",|*site*|/socket/kahua"
	   (and (file-exists? socket-path)
		(or (eq? (file-type socket-path) 'socket)
		    (eq? (file-type socket-path) 'fifo))))))

;; kahua-shell を起動する。
(test* "start shell" "Welcome to Kahua."
       (let ((p (run-process 'env "-i" "../src/kahua-shell" "--test" "-S" *site* 
			     :input :pipe :output :pipe :error :pipe)))
	 (set! *shell* p)
	 (sys-sleep 3)
	 (let* ((out (process-input  *shell*))
		(in  (process-output *shell*)))
           (read-line in))
           ))

;;---------------------------------------------------------------
;; テストに必要なユーティリティを定義する。
(test-section "define utilities")

(define (shell-out)
  (process-input *shell*))

(define (shell-in)
  (let1 pt (process-output *shell*)
 ;; ad hoc patch for Gauche 0.8.1
    (begin ((setter port-buffering) pt :none) pt)))

(define (send msg)
  (let* ((out (shell-out)))
    (write msg out)
    (newline out)))

(define (recv)
  (read (shell-in)))

(define (send&recv msg)
  (let* ((out (shell-out))
	 (in  (shell-in)))
    (read in)      ;; read prompt
    (if (pair? msg)
	(for-each (lambda (e)
		    (write e out) (display " " out)) msg)
	(write msg out))   ;; write command
    (newline out)
    (flush out)
    (read in)))

(define (send&recv-str msg)
  (let* ((out (shell-out))
	 (in  (shell-in)))
    (read in)         ;; read prompt
    (if (pair? msg)
	(for-each (lambda (e)
		    (write e out) (display " " out)) msg)
	(write msg out))   ;; write command
    (newline out)
    (flush out)
    (sys-sleep 2)
    (let1 ret (read-block 1000 in)
	  (newline out)
	  (string-incomplete->complete ret))))


;;------------------------------------------------------------
;; シェルコマンドをテスト
(test-section "shell command test")

(sys-sleep 3)

;; 認証テスト。
;; アプリケーションサーバの選択プロンプトが出ることを確認する。
(test* "shell: login" "select wno> "
       (begin
         (recv)
         (send 'gandalf)
         (sys-sleep 1)
         (recv)
         (send 'friend)
         (sys-sleep 1)
         (read-line (shell-in))
         (sys-sleep 1)
         (read-line (shell-in))
         (sys-sleep 1)
	 ((setter port-buffering) (shell-in) :none)
         (string-incomplete->complete 
	  (read-block 1000 (shell-in)))
         ))

;; 認証されたら接続するアプリケーションサーバを選択してログインする。
;; プロンプトが接続先のアプリケーション名(hello)であることを確認する。
(test* "shell: select worker" #f
       (begin
         (sys-sleep 1)
         (send '0)
         (sys-sleep 1)
         (not
          (#/hello/
           (string-incomplete->complete (read-block 1000 (shell-in)))
           )))
         )

;; 接続先がアプリケーションであるか。
;; カレントモジュールが無名モジュール(サンドボックス)であることを確認する。
(test* "shell: evaluation" "#<module #>"
       (begin
         (sys-sleep 1)
         (send '(current-module))
         (sys-sleep 1)
         (car (string-split
               (string-incomplete->complete (read-block 1000 (shell-in)))
               "\n"))
         )
       )


;;------------------------------------------------------------
;; テストの終了処理。
(test-section "finalize")

;; kahua-spvr を終了する。
(process-send-signal *spvr* SIGTERM)
(process-wait *spvr*)

(test* "shutdown shell" #t
       (begin
	 (process-send-signal *shell* SIGTERM)
	 (process-wait *shell*)))

(test-end)

