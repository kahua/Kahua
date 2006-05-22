;; -*- coding: euc-jp ; mode: scheme -*-
;; test user exclusive mode.
;; ユーザ専用モードのテスト

;; $Id: usermode.scm,v 1.6.8.1 2006/05/22 09:00:53 bizenn Exp $

(use gauche.test)
(use gauche.process)
(use gauche.net)
(use file.util)
(use kahua.config)
(use kahua.gsid)

(test-start "user exclusive mode")

;;---------------------------------------------------------------
(test-section "initialization")

(sys-system "rm -rf _tmp _work _cvs _src user.conf")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_tmp/user" #o755)
(sys-mkdir "_tmp/user/gandalf" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/user" #o755)
(sys-mkdir "_work/user/gandalf" #o755)
(sys-mkdir "_work/user/gandalf/checkout" #o755)
(sys-mkdir "_work/user/gandalf/plugins" #o755)

;; prepre cvs repository
(define repository (sys-normalize-pathname "./_cvs" :absolute #t))

(sys-mkdir "_cvs" #o755)
(run-process "cvs" "-d" repository "init" :wait #t)

(sys-mkdir "_src" #o755)
(sys-mkdir "_src/hello"    #o755)
(sys-mkdir "_src/greeting" #o755)
(sys-mkdir "_src/lister"   #o755)

(copy-file "hello-world.kahua" "_src/hello/hello.kahua")
(copy-file "greeting.kahua"    "_src/greeting/greeting.kahua")
(copy-file "lister.kahua"      "_src/lister/lister.kahua")

(copy-file "../plugins/allow-module.scm"
           "_work/user/gandalf/plugins/allow-module.scm")
(copy-file "testcustom.conf"
           "_work/user/gandalf/custom.conf")


(sys-chdir "./_src")
(run-process "cvs" "-Q" "-d" repository "import" "-m test" "." "vt" "rt"
  :wait #t)

(sys-chdir "../")

(run-process "cvs" "-Q" "-d" repository "checkout" "-d"
             "_work/user/gandalf/checkout" 
	     "hello" "greeting" "lister" :wait #t)

;; copy user.conf
(copy-file "testuser.conf" "user.conf")

;; prepare app-servers file
(with-output-to-file "_work/user/gandalf/app-servers"
  (lambda ()
    (write '((hello    :run-by-default 1)
             (greeting :run-by-default 0)
             (lister   :run-by-default 0)
             ))))

(define *config* "./test.conf")
(define *spvr*   #f)
(define *admin*  #f)
(define *shell*  #f)

;;---------------------------------------------------------------
;; Test user exclusive mode.
;; Run three programs with -user option.
;; Load custom.conf from user directory then overwrite test.conf and
;; set working directory.
;;
;; ユーザ専用モードテスト
;; 3つのプログラムを -user オプション付きで実行。
;; ユーザディレクトリから custom.conf を読み込んで test.conf の設定を
;; 上書きし、ワーキングディレクトリをセットする。
;; 


;; テストに必要な3つのスクリプトをユーザ専用モードで起動する。
(test-section "run scripts in user exclusive mode.")

;; kahua-spvr をユーザ専用モードで起動する。
;; -user オプション付きで起動することを確認する。
(test* "start spvr" #t
       (let ((p (run-process "../src/kahua-spvr" "--test"
			     "-c" *config* "-user" "gandalf")))
	 (sys-sleep 3)
	 (and (file-exists? "_tmp/user/gandalf/kahua")
	      (or (eq? (file-type "_tmp/user/gandalf/kahua") 'socket)
	          (eq? (file-type "_tmp/user/gandalf/kahua") 'fifo)))))

;; kahua-admin をユーザ専用モードで起動する。
;; -user オプション付きで起動することを確認する。
(test* "start admin" 'spvr>
       (let ((p (run-process "../src/kahua-admin" "--test"
			     "-c" *config* "-user" "gandalf"
			     :input :pipe :output :pipe :error :pipe)))
	 (set! *admin* p)
	 (sys-sleep 1)
	 (let* ((out (process-input  *admin*))
		(in  (process-output *admin*)))
	   (read in))))

;; kahua-admin をユーザ専用モードで起動する。
;; -user オプション付きで起動することを確認する。
(test* "start shell" "Welcome to Kahua."
       (let ((p (run-process 'env "-i" "../src/kahua-shell" "--test"
			     "-c" *config* "-user" "gandalf"
			     :input :pipe :output :pipe :error :pipe)))
	 (set! *shell* p)
	 (sys-sleep 3)
	 (let* ((out (process-input  *shell*))
		(in  (process-output *shell*)))
           (read-line in))
           ))

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

(define (shell-out)
  (process-input *shell*))

(define (shell-in)
  (let1 pt (process-output *shell*)
;; ad hoc patch for Gauche 0.8.1
    (begin ((setter port-buffering) pt :none) pt)))

(define (send-shell msg)
  (let* ((out (shell-out)))
    (write msg out)
    (newline out)))


;;------------------------------------------------------------
;; ユーザ専用モードでの kahua-admin の動作を確認する。
(test-section "kahua-admin with -user option")

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
;; app-server に登録されている3つのアプリケーションが
;; 表示されることを確認する。
(test* "admin: reload" '(hello greeting lister)
       (send&recv 'reload))

;; update コマンドのテスト
;; hello を更新できることを確認する。
(test* "admin: update" 'update:
       (send&recv '(update hello)))


;;------------------------------------------------------------
;; ユーザ専用モードでの kahua-shell の動作を確認する。
(test-section "kahua-shell with -user option")


;; 認証テスト。
;; ユーザ認証をパスし、ログインできることを確認する。
(test* "shell: login" "select wno> "
       (begin
         (read (shell-in))
         (send-shell 'friend)
         (sys-sleep 1)
         (read-line (shell-in))
         (sys-sleep 1)
         (read-line (shell-in))
         (sys-sleep 1)
         (read-line (shell-in))
         (sys-sleep 1)
         (string-incomplete->complete (read-block 1000 (shell-in)))
         ))

;; ワーカー(起動中のアプリケーション)の一覧から hello を選んで
;; アプリケーション環境に入れることを確認する。
(test* "shell: select worker" #f
       (begin
         (sys-sleep 1)
         (send-shell '0)
         (sys-sleep 1)
         (not
          (#/hello/
           (string-incomplete->complete (read-block 1000 (shell-in)))
           )))
         )

;; 接続先がアプリケーション環境(無名モジュール)であることを確認する。
(test* "shell: evaluation" "#<module #>"
       (begin
         (sys-sleep 1)
         (send-shell '(current-module))
         (sys-sleep 1)
         (car (string-split
               (string-incomplete->complete (read-block 1000 (shell-in)))
               "\n"))
         )
       )

;;------------------------------------------------------------
;; テスト終了処理
;; テスト用に起動したプロセスを終了する。
(test-section "finalize")


;; kahua-shell が終了することを確認する。
(test* "shutdown shell" #t
       (begin
	 (process-send-signal *shell* SIGTERM)
         (sys-sleep 1) ;; give the spvr time to shutdown ...
	 (process-wait *shell*)))

;; kahua-spvr が終了することを確認する。
(test* "shutdown spvr" '()
       (begin
	 (send&recv 'shutdown)
	 (call/cc (lambda (exit)
		    (dotimes (i 15)
		      (sys-sleep 1)
		      (when (null? (directory-list "_tmp/user/gandalf" :children? #t))
			(exit '())))
		    #f))))

;; kahua-admin が終了することを確認する。
(test* "shutdown admin" #t
       (begin
	 (process-send-signal *admin* SIGTERM)
         (sys-sleep 1) ;; give the spvr time to shutdown ...
	 (process-wait *admin*)))

(test-end)

