;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.persistence with dbi
;; DBIバックエンドを用いたkahua.persistenceモジュールのテスト

;; $Id: persistence-dbi.scm,v 1.4 2006/07/28 13:09:49 bizenn Exp $

;; Clear the data remaining from the other test
(define (cleanup-db dbtype user pass options)

  (define (safe-query c sql)
    (guard (e ((<dbi-exception> e) '())
	      (else (raise e)))
      (dbi-do c sql '(:pass-through #t))))

  (guard (e ((<dbi-exception> e) (error #`"DBI error: ,(ref e 'message)"))
	    (else (raise e)))
    (let* ((d (dbi-make-driver dbtype))
	   (c (dbi-make-connection d user pass options))
	   (r (safe-query c "select table_name from kahua_db_classes"))
	   (tables (and r (map (cut dbi-get-value <> 0) r))))
      (dolist (table tables)
	(safe-query c #`"drop table ,|table|"))
      (safe-query c "drop table kahua_db_idcount")       ; for MySQL
      (safe-query c "drop table kahua_db_classcount")	 ; for MySQL
      (safe-query c "drop sequence kahua_db_idcount")    ; for PostgreSQL
      (safe-query c "drop sequence kahua_db_classcount") ; for PostgreSQL
      (safe-query c "drop generator kahua_db_idcount")   ; for Firebird
      (safe-query c "drop generator kahua_db_classcount"); for Firebird
      (safe-query c "drop table kahua_db_classes")
      (dbi-close c)))
  )
