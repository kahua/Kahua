;; -*- coding: euc-jp ; mode: scheme -*-
;; PostgreSQLバックエンドのテスト
;; $Id: persistence-dbi-pg.scm,v 1.3 2006/02/04 07:39:40 shibata Exp $

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
       ;; (q (dbi-make-query c))
       (r (dbi-do c "select table_name from kahua_db_classes"))
       (tables (and r (map (cut dbi-get-value <> 0) r))))
  (dolist (table tables)
    (dbi-do c #`"drop table ,|table|"))
  (dbi-do c "drop table kahua_db_idcount")
  (dbi-do c "drop table kahua_db_classes")
  (dbi-close c)
  )

(load "./persistence.scm")
