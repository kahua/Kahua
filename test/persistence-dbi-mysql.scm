;; MySQLバックエンドのテスト
;; $Id: persistence-dbi-mysql.scm,v 1.1 2004/04/07 09:55:33 nobsun Exp $

;; Notes:
;;  * テストケース自体はpersistence.scmのものを使う。
;;  * mysqldが走っており、環境変数$USERのアカウントでパスワード無しで
;;    ログインでき、'test'データベースが使えることを前提とする。

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"mysql:,|*user*|::db=test")

;; 前回のテストで作られたテーブルが残っていればそれをクリアしておく
(cleanup-db "mysql" *user* "" "db=test")

(load "./persistence.scm")
