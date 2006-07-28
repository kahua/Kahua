;; -*- coding: euc-jp ; mode: scheme -*-
;; PostgreSQLバックエンドのテスト
;; $Id: persistence-dbi-pg.scm,v 1.4 2006/07/28 13:09:49 bizenn Exp $

;; Notes:
;;  * テストケース自体はpersistence.scmのものを使う。
;;  * postmasterが走っており、環境変数$USERのアカウントでパスワード無しで
;;    ログインでき、デフォルトデータベースが使えることを前提とする。

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"pg:,|*user*|::")

;; 前回のテストで作られたテーブルが残っていればそれをクリアしておく
(load "./persistence-dbi.scm")
(cleanup-db "pg" *user* "" "")

(load "./persistence.scm")
