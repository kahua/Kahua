;; -*- coding: utf-8 ; mode: scheme -*-
;; test admin scripts.
;; this test isn't for modules, but for actual scripts.
;; kahua-admin テスト

(use gauche.test)
(use gauche.process)
(use gauche.net)
(use file.util)
(use kahua.config)
(use kahua.gsid)
(use kahua.test.util)

(test-start "kahua-admin script")

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
(define *admin*  #f)

(kahua-common-init *site* #f)

;; prepare app-servers file
(with-output-to-file *app-servers*
  (lambda ()
    (write '((hello    :run-by-default 1)
             (greeting :run-by-default 0)
             (lister   :run-by-default 0)
             ))))

;;---------------------------------------------------------------
;; テストに必要な2つのスクリプトを起動する。
(test-section "run scripts")

;; kahua-spvr を起動する。
(test* "start spvr" #t
       (let ((p (kahua:invoke&wait `("../src/kahua-spvr" "--test" "-S" ,*site* "-i") :prompt "kahua> "))
	     (socket-path #`",|*site*|/socket/kahua"))
	 (and (file-exists? socket-path)
	      (or (eq? (file-type socket-path) 'socket)
		  (eq? (file-type socket-path) 'fifo)))))

;; kahua-admin を起動する。
(define-constant *admin-prompt* "spvr> ")
(test* "start admin" *admin-prompt*
       (receive (p prompt) (kahua:invoke&wait `("../src/kahua-admin" "--test" "-S" ,*site*) :prompt *admin-prompt*)
	 (set! *admin* p)
	 prompt)
       string=?)

;;---------------------------------------------------------------
;; テスト用のユーティリティを定義する。
(test-section "define utilities")

(define (admin-out)
  (process-input *admin*))
(define (admin-in)
  (process-output *admin*))

(define (send msg)
  (let* ((out (admin-out)))
    (write msg out)
    (newline out)))

(define (send&recv msg)
  (let* ((out (admin-out))
	 (in  (admin-in)))
    (read in)      ;; read prompt
    (if (pair? msg)
	(for-each (lambda (e)
		    (write e out) (display " " out)) msg)
	(write msg out))   ;; write command
    (newline out)
    (flush out)
    (read in)))

(define (send&recv-str msg)
  (let* ((out (admin-out))
	 (in  (admin-in)))
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

(newline (admin-out))


;;------------------------------------------------------------
;; kahua-admin の動作を確認する。
(test-section "spvr command test")

;; ls コマンドを実行。
;; hello アプリケーションが一覧にあることを確認する。
(test* "admin: ls" #f
       (not (#/wno\s+pid\s+type\s+since\s+wid.+hello/
	     (send&recv-str 'ls))))

;; help コマンドを実行。
;; コマンドのリストが表示されることを確認する。
(test* "admin: help" #t
       (let1 ans (send&recv 'help)
	     (and (list? ans)
		  (< 0 (length ans)))))

;; type コマンドを実行。
;; hello greeting lister の3つのアプリケーションが表示される
;; ことを確認する。
(test* "admin: types" '(hello greeting lister)
       (send&recv 'types))

;; run コマンドのテスト。1回目。
;; greeting が起動することを確認する。
(test* "admin: run greeting" #f
       (let1 ans (send&recv-str '(run greeting))
	     (not (#/greeting/ ans))))

;; run コマンドのテスト。2回目。
;; lister が起動することを確認する。
(test* "admin: run lister" #f
       (let1 ans (send&recv-str '(run lister))
	     (not (#/lister/ ans))))

;; kill コマンドのテスト。
;; greeting を終了できることを確認する。
(test* "admin: kill 1(greeting)" #f
       (let1 ans (send&recv-str '(kill 1))
	     (#/greeting/ ans)))

;; reload コマンドのテスト。
;; app-servers に登録されている3つのアプリケーションが
;; 表示されることを確認する。
(test* "admin: reload" '(hello greeting lister)
       (send&recv 'reload))

;; update コマンドのテスト
;; hello を更新できることを確認する。
(test* "admin: update" 'update:
       (send&recv '(update hello)))

;;------------------------------------------------------------
;; kahua-server に接続する connect コマンドをテストする。
(test-section "server connect test")

;; ワーカー番号 0 hello に接続できることを確認する。
(test* "admin: connect 0(hello)" #t
       (not (not (#/hello/ (send&recv-str '(connect 0))))))

;; 接続先が hello であることを確認する。
(test* "admin: connect: (kahua-worker-type)" "hello"
       (begin
	 (write '(kahua-worker-type) (admin-out))
	 (newline (admin-out))
	 (flush (admin-out))
	 (read (admin-in))))

;; hello から切断できることを確認する。
(test* "admin: connect: disconnect" #f
       (begin
	 (write 'disconnect (admin-out))
	 (newline (admin-out))
	 (flush (admin-out))
	 (sys-sleep 1)
	 (not (#/spvr>/ (string-incomplete->complete
			 (read-block 1000 (admin-in)))))))

(newline (admin-out))

;;------------------------------------------------------------
;; 開発者アカウントをテストする。
(test-section "developer account test")

;; lsuser コマンドを実行し、ユーザ gandalf 一人が一覧にいることを
;; 確認する。
(test* "admin: lsuser" '("gandalf")
       (send&recv '(lsuser)))

;; adduser コマンドを実行し、ユーザ bilbo を登録できることを
;; 確認する。
(test* "admin: adduser" 'done
       (send&recv '(adduser bilbo baggins)))

;; lsuser コマンドを実行し、ユーザ gandalf と bilbo が一覧にいることを
;; 確認する。
(test* "admin: lsuser" '("gandalf" "bilbo")
       (send&recv '(lsuser)))

;; deluser コマンドを実行し、ユーザ gandalf を削除できることを確認する。
(test* "admin: deluser" 'done
       (send&recv '(deluser gandalf)))

;; lsuser コマンドを実行し、ユーザ bilbo 一人が一覧にいることを確認する。
(test* "admin: lsuser" '("bilbo")
       (send&recv '(lsuser)))


;;------------------------------------------------------------
;; テスト終了処理
(test-section "finalize")

;; shutdown コマンドを実行し、kahua-spvr が終了できることを確認する。
(test* "shutdown spvr" '()
       (begin
	 (send&recv 'shutdown)
	 (call/cc (lambda (exit)
		    (dotimes (i 15)
		      (sys-sleep 1)
		      (when (null? (directory-list #`",|*site*|/socket" :children? #t))
			(exit '())))
		    #f))))

;; kahua-admin が終了することを確認する。
(test* "shutdown admin" #t
       (begin
	 (process-send-signal *admin* SIGTERM)
	 (process-wait *admin*)))

(test-end)

