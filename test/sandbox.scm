;; -*- coding: utf-8 ; mode: scheme -*-
;; test sandbox module.
;; Kahua.sandbox モジュールのテスト

(use gauche.test)
(use kahua.plugin)
(use kahua.config)

(test-start "sandbox test")

;; TODO: This should not be required.
(define *site* "_site")
(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(kahua-common-init *site* #f)

(define *sandbox* #f)

;;---------------------------------------------------------------
;; ロードテスト
;; kahua.sandbox がロードでき、またそのインターフェイスに齟齬が
;; ないことを確認する。
(use kahua.sandbox)
(test-module 'kahua.sandbox)


;; サンドボックスモジュールを作成し、それが無名モジュールで
;; あることを確認する。
(test* "make sandbox module" (write-to-string (make-module #f))
       (let ((m (make-sandbox-module)))
         (set! *sandbox* m)
         (x->string m)))

;; 安全な手続きのテストその1。
;; サンドボックスで許可されている手続きが car が使えることを確認する。
(test* "check available binding, car"
       '1
       (eval '(car '(1 2 3)) *sandbox*))

;; 安全な手続きのテストその2。
;; サンドボックスで許可されている手続きが define が使えることを確認する。
(test* "check available binding, define"
       'square
       (eval '(define (square n) (* n n)) *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその1。
;; open-input-file を評価するとエラーになることを確認する。
(test* "check disablebinding, open-input-file"
       *test-error*
       (eval '(open-input-file "/etc/passwd") *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその2。
;; open-output-file を評価するとエラーになることを確認する。
(test* "check disablebinding, open-output-file"
       *test-error*
       (eval '(open-output-file "evil") *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその3。
;; call-with-input-file を評価するとエラーになることを確認する。
(test* "check disablebinding, call-with-input-file"
       *test-error*
       (eval '(call-with-input-file "/etc/passwd" read-line) *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその4。
;; call-with-output-file を評価するとエラーになることを確認する。
(test* "check disablebinding, call-with-output-file"
       *test-error*
       (eval '(call-with-output-file "evil"
                (lambda (in) (format in "#!/bin/sh\n")
                             (format in "killall gosh")))
             *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその5。
;; load を評価するとエラーになることを確認する。
(test* "check disablebinding, load"
       *test-error*
       (eval '(load "gauche/process") *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその6。
;; require を評価するとエラーになることを確認する。
(test* "check disablebinding, require"
       *test-error*
       (eval '(require "gauche/net") *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその7。
;; import を評価するとエラーになることを確認する。
(test* "check disablebinding, import"
       *test-error*
       (eval '(import kahua.sandbox) *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその8。
;; select-module を評価するとエラーになることを確認する。
(test* "check disablebinding, select-module"
       *test-error*
       (eval '(select-module user) *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその9。
;; with-module を評価するとエラーになることを確認する。
(test* "check disablebinding, with-module"
       *test-error*
       (eval '(with-module user (open-input-file "/etc/passwd"))
             *sandbox*))

;; ダミーの束縛をした危険な手続きのテストその10。
;; use でプラグインに登録されていないモジュールを評価すると
;; エラーになることを確認する。
(test* "check overrided, use"
       *test-error*
       (eval '(use file.util) *sandbox*))

(test-end)

