;; -*- coding: utf-8 ; mode: scheme -*-
;; PostgreSQLバックエンドのテスト
;; $Id$

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
