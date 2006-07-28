;; -*- coding: euc-jp ; mode: scheme -*-
;; MySQLバックエンドのテスト
;; $Id: persistence-dbi-mysql.scm,v 1.4 2006/07/28 13:09:49 bizenn Exp $

;; Notes:
;;  * テストケース自体はpersistence.scmのものを使う。
;;  * mysqldが走っており、環境変数$USERのアカウントでパスワード無しで
;;    ログインでき、'test'データベースが使えることを前提とする。

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"mysql:,|*user*|::db=test")

;; 前回のテストで作られたテーブルが残っていればそれをクリアしておく
(load "./persistence-dbi.scm")
(cleanup-db "mysql" *user* "" "db=test")

(load "./persistence.scm")
