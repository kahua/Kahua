;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.developer
;; Kahua.developer モジュールのテスト

;; $Id: developer.scm,v 1.2 2005/07/04 05:09:21 nobsun Exp $

(use gauche.test)
(use srfi-1)
(use file.util)
(use kahua.config)

;;---------------------------------------------------------------
;; start test
;; 開発者アカウント操作のテストを開始する。
(test-start "developer")

;; テスト環境作成
(define conf-file (build-path (sys-getcwd) "user.conf"))
(define conf-lock-file (string-append conf-file ".lock"))


(slot-set! (kahua-config) 'userconf-file conf-file)

(sys-system #`"rm -rf ,conf-file ,conf-lock-file")
(sys-system #`"touch ,conf-file")


;; ロードテスト
;; kahua.developer がロードでき、またそのインターフェイスに齟齬が
;; ないことを確認する。
(use kahua.developer)
(test-module 'kahua.developer)

;; アカウント一覧表示１回目。
;; まだ一人も登録していないので、空リストになることを確認する。
(test* "kahua-list-developer" '()
       (kahua-list-developer))

;; アカウント 3人分を登録する。
;; 全て成功して #t を返すことを確認する。
(test* "kahua-add-developer" #t
       (every (cut eq? <> #t)
              (list (kahua-add-developer "yusei" "^Epc4q-D" '(manager))
                    (kahua-add-developer "admin" "yqX^Vj8q" '(manager))
                    (kahua-add-developer "guest" "N_HHmW6h" '()))))

;; アカウントを登録する。
;; パスワードが短いので登録を失敗しエラーが起きることを確認する。
(test* "kahua-add-developer bad password"
       *test-error*
       (kahua-add-developer "anonymous" "a" '()))

;; アカウント登録する。
;; 名前が短いので登録を失敗し、エラーが起きることを確認する。
(test* "kahua-add-developer bad name"
       *test-error*
       (kahua-add-developer "" "anonymous" '()))

;; アカウント一覧表示２回目。
;; 今度は登録が成功した3人の名前を返すことを確認する。
(test* "kahua-list-developer" '("yusei" "admin" "guest")
       (kahua-list-developer))

;; アカウントを削除する。
;; guest を削除、成功して #t を返すことを確認する。
(test* "kahua-delete-developer" #t
       (kahua-delete-developer "guest"))

;; アカウント一覧表示３回目。
;; guest を削除したので、残り2人の名前を返すことを確認する。
(test* "kahua-list-developer" '("yusei" "admin")
       (kahua-list-developer))

;; アカウントを登録する。
;; 削除したあとに、新しいアカウントを作成し、#t を返すことを確認する。
(test* "kahua-add-developer" #t
       (kahua-add-developer "anonymous" "anonymous" '()))

;; アカウント一覧表示４回目。
;; 追加したアカウントを含めて、3人の名前を返すことを確認する。
(test* "kahua-list-developer" '("yusei" "admin" "anonymous")
       (kahua-list-developer))

;; 認証テスト１回目。
;; 登録した名前とパスワードが一致するので #t を返すことを確認する。
(test* "kahua-check-developer" #t
       (kahua-check-developer "yusei" "^Epc4q-D"))

;; 認証テスト２回目。
;; 名前が間違っているので #f を返すことを確認する。
(test* "kahua-check-developer the name not found" #f
       (kahua-check-developer "yus" "^Epc4q-D"))

;; 認証テスト３回目。
;; パスワードが間違っているので #f を返すことを確認する。
(test* "kahua-check-developer incorrect password" #f
       (kahua-check-developer "yusei" "^Epc4"))

;; パスワード変更１回目。
;; 名前、新パスワードが正しいので #t を返すことを確認する。
(test* "kahua-change-developer-password" #t
       (kahua-change-developer-password "admin" "WpX^krRS"))

;; パスワード変更２回目。
;; 名前が間違っているので、エラーが起きることを確認する。
(test* "kahua-change-developer-password the name not found"
       *test-error*
       (kahua-change-developer-password "adnim" "WpX^krRS"))

;; パスワード変更３回目。
;; パスワードが短いので、エラーが起きることを確認する。
(test* "kahua-change-developer-password password too short"
       *test-error*
       (kahua-change-developer-password "adnim" "Wp"))

(sys-system #`"rm -rf ,conf-file ,conf-lock-file")

(test-end)
