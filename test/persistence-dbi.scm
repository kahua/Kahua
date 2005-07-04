;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.persistence with dbi
;; DBIバックエンドを用いたkahua.persistenceモジュールのテスト

;; $Id: persistence-dbi.scm,v 1.2 2005/07/04 05:09:21 nobsun Exp $

;; Clear the data remaining from the other test
(define (cleanup-db dbtype user pass options)

  (define (safe-query q sql)
    (with-error-handler
        (lambda (e)
          (if (is-a? e <dbi-exception>) '() (raise e)))
      (lambda () (dbi-execute-query q sql))))
  
  (with-error-handler
      (lambda (e)
        (if (is-a? e <dbi-exception>)
          (error #`"DBI error: ,(ref e 'message)")
          (raise e)))
    (lambda ()
      (let* ((d (dbi-make-driver dbtype))
             (c (dbi-make-connection d user pass options))
             (q (dbi-make-query c))
             (r (safe-query q "select table_name from kahua_db_classes"))
             (tables (and r (map (cut dbi-get-value <> 0) r))))
        (dolist (table tables)
          (safe-query q #`"drop table ,|table|"))
        (safe-query q "drop table kahua_db_idcount")
        (safe-query q "drop table kahua_db_classes")
        (dbi-close q)
        (dbi-close c))))
  )

(load "./persistence-dbi-mysql.scm")
(load "./persistence-dbi-pg.scm")
