;; -*- coding: utf-8 ; mode: scheme -*-
;; test plugin module.
;; Kahua.plugin モジュールのテスト

(use gauche.test)
(use file.util)
(use kahua.sandbox)
(use kahua.config)


(test-start "plugin manager")

(define *site* "_site")

;; --------------------------------------------------------------
(test-section "initialization")
(sys-system #`"rm -rf ,|*site*|")
(kahua-site-create *site*)
(copy-file "../plugins/allow-module.scm" #`",|*site*|/plugins/allow-module.scm")
(copy-file "../plugins/sendmail.scm" #`",|*site*|/plugins/sendmail.scm")

(kahua-common-init *site* #f)

;;---------------------------------------------------------------
;; プラグインモジュールのテストを開始する。
(test-section "plugin")

;; ロードテスト
;; kahua.plugin がロードでき、またそのインターフェイスに齟齬が
;; ないことを確認する。
(use kahua.plugin)
(test-module 'kahua.plugin)

;; プラグインの初期化ができることを確認する。
(test* "initialize plugins" "#<undef>"
       (x->string (initialize-plugins (kahua-plugin-directory))))

;; プラグインが登録されたことを確認する。
(test* "are there plugins" #t
       (> (length (all-plugins)) 1))

;;---------------------------------------------------------------
;; サンドボックス内でのテスト
(test-section "in a sandbox")

(define *sandbox* (make-sandbox-module))

;; プラグイン srfi-1 をロードする前には xcons 手続きがないので、
;; テストに失敗することを確認する。
(test* "no plugin loads yet"
       *test-error*
       (eval '(xcons 1 2)  *sandbox*))

;; プラグイン srfi-1 をロードしたあとに xcons 手続きを使えることを
;; 確認する。
(test* "load srfi-1 plugin"
       '(2 . 1)
       (eval '(begin (use srfi-1) (xcons 1 2)) *sandbox*))

;; プラグイン srfi-1 の filter 手続きであることを確認する。
(test* "this is srfi-1's filter"
       (eval 'filter (find-module 'srfi-1))
       (eval 'filter *sandbox*))

;; プラグイン gauche.collection をロードしたあとに filter 手続きが
;; gauche.collectionのものであることを確認する。
(test* "replace filter to the gauche.collections's one"
       (eval 'filter (find-module 'gauche.collection))
       (eval '(begin (use gauche.collection) filter) *sandbox*))

;; プラグイン sendmail をロードする前には sendmail 手続きがないことを
;; 確認する。
(test* "sendmail does not exists"
       *test-error*
       (eval 'sendmail *sandbox*))

;; プラグイン sendmail をロードしたあとに sendmail 手続きがあることを
;; 確認する。
(test* "load sendmail plugin" #t
       (eval '(begin (use-plugin sendmail)
                     (global-variable-bound? (current-module) 'sendmail))
             *sandbox*))

(test-end)
