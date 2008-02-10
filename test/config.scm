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

(test-section "Old Fashion Configuration")
(test* "loading config" #t
       (is-a? (let1 c (kahua-init "./test.conf")
		(set! *config* c)
		c) <kahua-config>))

(test* "sockbase" "unix:_tmp"
       (kahua-sockbase))

(test* "set! sockbase" "unix:foo"
       (begin (set! (kahua-sockbase) "unix:foo")
              (kahua-sockbase)))

(test* "log path" "_work/logs/foo.log"
       (kahua-logpath "foo.log"))

(test* "config file" "./test.conf"
       (kahua-config-file))xs

(test* "error-document-alist" '((404 . "_work/404.html") (503 . "_work/503.html"))
       (kahua-error-document-alist))

(test* "error-document 404" "_work/404.html"
       (kahua-error-document 404))

(test* "error-document 500" #f
       (kahua-error-document 500))

(test* "error-document 503" "_work/503.html"
       (kahua-error-document 503))

(test-section "Initialize with Site Bundle")
(test* "kahua-site-init" *config* (kahua-site-init "./_work") eq?)
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
(test* "kahua-site-init again" *config* (kahua-site-init "_site") eq?)
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

