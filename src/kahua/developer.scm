;; $id$

;; test kahua.developer

(use gauche.test)
(use srfi-1)
(use file.util)

(test-start "developer")
(use kahua.developer)
(test-module 'kahua.developer)


;; make test environment
(define conf-file (build-path (sys-getcwd) "user.conf"))
(define conf-lock-file (string-append conf-file ".lock"))

(use kahua.config)
(slot-set! (kahua-config) 'userconf-file conf-file)

(sys-system #`"rm -rf ,conf-file ,conf-lock-file")
(sys-system #`"touch ,conf-file")


;; start test
;; 開発者アカウント操作のテストを開始

;; アカウント一覧表示
;; まだ一人も登録していないので、空リストになる。
(test* "kahua-list-developer" '()
       (kahua-list-developer))

;; アカウント 3人分を登録
;; 全て成功して #t を返す。
(test* "kahua-add-developer" #t
       (every (cut eq? <> #t)
              (list (kahua-add-developer "yusei" "^Epc4q-D" '(manager))
                    (kahua-add-developer "admin" "yqX^Vj8q" '(manager))
                    (kahua-add-developer "guest" "N_HHmW6h" '()))))

;; アカウントを登録
;; パスワードが短いので登録失敗。エラーが起きる。
(test* "kahua-add-developer bad password"
       *test-error*
       (kahua-add-developer "anonymous" "a" '()))

;; アカウント登録
;; 名前が短いので登録失敗。エラーが起きる。
(test* "kahua-add-developer bad name"
       *test-error*
       (kahua-add-developer "" "anonymous" '()))

;; アカウント一覧表示 ２回目
;; 今度は登録が成功した3人の名前を返す。
(test* "kahua-list-developer" '("yusei" "admin" "guest")
       (kahua-list-developer))

;; アカウント削除
;; guest を削除、成功して真を返す。
(test* "kahua-delete-developer" #t
       (kahua-delete-developer "guest"))

;; アカウント一覧表示 ３回目
;; guest を削除したので、残り2人の名前を返す。
(test* "kahua-list-developer" '("yusei" "admin")
       (kahua-list-developer))

;; アカウント登録
;; 削除したあとに、新しいアカウントを作成。成功して真を返す。
(test* "kahua-add-developer" #t
       (kahua-add-developer "anonymous" "anonymous" '()))

;; アカウント一覧表示 ４回目
;; 追加したアカウントを含めて、3人の名前を返す。
(test* "kahua-list-developer" '("yusei" "admin" "anonymous")
       (kahua-list-developer))

;; 認証
;; 登録した名前とパスワードが一致するので真を返す。
(test* "kahua-check-developer" #t
       (kahua-check-developer "yusei" "^Epc4q-D"))

;; 認証２回目
;; 名前が間違っているので偽を返す。
(test* "kahua-check-developer the name not found"
       *test-error*
       (kahua-check-developer "yus" "^Epc4q-D"))

;; 認証３回目
;; パスワードが間違っているので偽を返す。
(test* "kahua-check-developer incorrect password" #f
       (kahua-check-developer "yusei" "^Epc4"))

;; パスワード変更
;; 名前、パスワードが正しいので真を返す。
(test* "kahua-change-developer-password" #t
       (kahua-change-developer-password "admin" "WpX^krRS"))

;; パスワード変更２回目
;; 名前が間違っているので、エラーが起きる。
(test* "kahua-change-developer-password the name not found"
       *test-error*
       (kahua-change-developer-password "adnim" "WpX^krRS"))


(sys-system #`"rm -rf ,conf-file ,conf-lock-file")

(test-end)