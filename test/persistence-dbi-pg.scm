;; PostgreSQLバックエンドのテスト
;; $Id: persistence-dbi-pg.scm,v 1.1 2004/04/07 09:55:33 nobsun Exp $

;; Notes:
;;  * テストケース自体はpersistence.scmのものを使う。
;;  * postmasterが走っており、環境変数$USERのアカウントでパスワード無しで
;;    ログインでき、デフォルトデータベースが使えることを前提とする。

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"pg:,|*user*|::")

;; 前回のテストで作られたテーブルが残っていればそれをクリアしておく
(let* ((d (dbi-make-driver "pg"))
       (c (dbi-make-connection d *user* "" ""))
       (q (dbi-make-query c))
       (r (dbi-execute-query q "select table_name from kahua_db_classes"))
       (tables (and r (map (cut dbi-get-value <> 0) r))))
  (dolist (table tables)
    (dbi-execute-query q #`"drop table ,|table|"))
  (dbi-execute-query q "drop table kahua_db_idcount")
  (dbi-execute-query q "drop table kahua_db_classes")
  (dbi-close q)
  (dbi-close c)
  )

(load "./persistence.scm")
