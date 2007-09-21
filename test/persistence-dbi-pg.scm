;; -*- coding: euc-jp ; mode: scheme -*-
;; PostgreSQLバックエンドのテスト
;; $Id: persistence-dbi-pg.scm,v 1.5 2006/10/24 06:14:53 bizenn Exp $

;; Notes:
;;  * テストケース自体はpersistence.scmのものを使う。
;;  * postmasterが走っており、環境変数$USERのアカウントでパスワード無しで
;;    ログインでき、デフォルトデータベースが使えることを前提とする。

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"postgresql:,|*user*|::")

;; 前回のテストで作られたテーブルが残っていればそれをクリアしておく
(load "./persistence-dbi.scm")
(cleanup-db "pg" *user* "" "")

(load "./persistence.scm")
