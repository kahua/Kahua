;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.persistence with dbi
;; DBIバックエンドを用いたkahua.persistenceモジュールのテスト

;; $Id: persistence-dbi.scm,v 1.3 2006/02/04 07:39:40 shibata Exp $

;; Clear the data remaining from the other test
(define (cleanup-db dbtype user pass options)

  (define (safe-query c sql)
    (with-error-handler
        (lambda (e)
          (if (is-a? e <dbi-exception>) '() (raise e)))
      (lambda () (dbi-do c sql '(:pass-through #t)))))
  
  (with-error-handler
      (lambda (e)
        (if (is-a? e <dbi-exception>)
          (error #`"DBI error: ,(ref e 'message)")
          (raise e)))
    (lambda ()
      (let* ((d (dbi-make-driver dbtype))
             (c (dbi-make-connection d user pass options))
             (r (safe-query c "select table_name from kahua_db_classes"))
             (tables (and r (map (cut dbi-get-value <> 0) r))))
        (dolist (table tables)
          (safe-query c #`"drop table ,|table|"))
        (safe-query c "drop table kahua_db_idcount")
        (safe-query c "drop table kahua_db_classes")
        (dbi-close c))))
  )

(load "./persistence-dbi-mysql.scm")
(load "./persistence-dbi-pg.scm")
