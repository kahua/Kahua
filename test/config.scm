;; -*- coding: utf-8 ; mode: scheme -*-
;; test kahua.config
;; Kahua.config モジュールのテスト

(use gauche.test)

(sys-system "rm -rf _work")
(sys-mkdir "_work" #o755)

;;---------------------------------------------------------------
;; テスト開始
(test-start "kahua.config")
(use kahua.config)

(define *config* #f)

;; ロードテスト
;; kahua.config がロードでき、またそのインターフェイスに齟齬が
;; ないことを確認する。
(test-module 'kahua.config)

(test-section "Initialize with Site Bundle")
(test* "kahua-site-init: cannot read kahua.conf" *test-error* (kahua-site-init "./_work"))
(sys-mkdir "./_work/etc" #o755)
(call-with-output-file "./_work/etc/kahua.conf" (cut write '(make <kahua-config>) <>))
(test* "kahua-site-init: can read kahua.conf, as new config" *config* (kahua-site-init "./_work") (compose not eq?))
(set! *config* (kahua-config))
(for-each (lambda (e)
	    (apply (lambda (name accessor path)
		     (test* name (sys-normalize-pathname path :absolute #t)
			    (accessor) string=?))
		   e))
	  `(("kahua-site-root" ,kahua-site-root "_work")
	    ("kahua-working-directory" ,kahua-working-directory "_work")
	    ("kahua-app-servers" ,kahua-app-servers "_work/app-servers")
	    ("kahua-application-directory" ,kahua-application-directory "_work/app")
	    ("kahua-plugin-directory" ,kahua-plugin-directory "_work/plugins")
	    ("kahua-database-directory" ,kahua-database-directory "_work/database")
	    ("kahua-temporary-directory" ,kahua-temporary-directory "_work/tmp")
	    ("kahua-log-directory" ,kahua-log-directory "_work/logs")
	    ("kahua-cgilog-directory" ,kahua-cgilog-directory "_work/cgilogs")
	    ("kahua-run-directory" ,kahua-run-directory "_work/run")
	    ("kahua-etc-directory" ,kahua-etc-directory "_work/etc")
	    ("kahua-userconf-file" ,kahua-userconf-file "_work/etc/user.conf")
	    ("kahua-config-file" ,kahua-config-file "_work/etc/kahua.conf")))
(test* "kahua-sockbase" #`"unix:,(sys-normalize-pathname \"_work/socket\" :absolute #t)"
       (kahua-sockbase) string=?)
(test* "kahua-default-database-name" "db" (kahua-default-database-name) string=?)
(test* "kahua-timeout-mins" 60 (kahua-timeout-mins) =)
(test* "kahua-auto-restart" #f (kahua-auto-restart) eq?)
(test* "kahua-secure-sandbox" #t (kahua-secure-sandbox) eq?)
(test* "kahua-spvr-concurrency" 10 (kahua-spvr-concurrency) =)

(kahua-site-create "_site")
(test* "kahua-site-create" #t (file-is-directory? "_site") eq?)
(test* "kahua-site-init again" *config* (kahua-site-init "_site") (compose not eq?))
(for-each (lambda (e)
	    (apply (lambda (name accessor path)
		     (test* name (sys-normalize-pathname path :absolute #t)
			    (accessor) string=?))
		   e))
	  `(("kahua-site-root" ,kahua-site-root "_site")
	    ("kahua-working-directory" ,kahua-working-directory "_site")
	    ("kahua-app-servers" ,kahua-app-servers "_site/app-servers")
	    ("kahua-application-directory" ,kahua-application-directory "_site/app")
	    ("kahua-plugin-directory" ,kahua-plugin-directory "_site/plugins")
	    ("kahua-database-directory" ,kahua-database-directory "_site/database")
	    ("kahua-temporary-directory" ,kahua-temporary-directory "_site/tmp")
	    ("kahua-log-directory" ,kahua-log-directory "_site/logs")
	    ("kahua-cgilog-directory" ,kahua-cgilog-directory "_site/cgilogs")
	    ("kahua-run-directory" ,kahua-run-directory "_site/run")
	    ("kahua-etc-directory" ,kahua-etc-directory "_site/etc")
	    ("kahua-userconf-file" ,kahua-userconf-file "_site/etc/user.conf")
	    ("kahua-config-file" ,kahua-config-file "_site/etc/kahua.conf")))
(test* "kahua-sockbase" #`"unix:,(sys-normalize-pathname \"_site/socket\" :absolute #t)"
       (kahua-sockbase) string=?)
(test* "kahua-default-database-name" "db" (kahua-default-database-name) string=?)
(test* "kahua-timeout-mins" 60 (kahua-timeout-mins) =)
(test* "kahua-auto-restart" #f (kahua-auto-restart) eq?)
(test* "kahua-secure-sandbox" #t (kahua-secure-sandbox) eq?)
(test* "kahua-spvr-concurrency" 10 (kahua-spvr-concurrency) =)

(sys-system "rm -rf _work _site")

(test-end)

