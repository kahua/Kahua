;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.config
;; Kahua.config モジュールのテスト

;; $Id: config.scm,v 1.2 2005/07/04 05:09:21 nobsun Exp $

(use gauche.test)

(sys-system "rm -rf _work")
(sys-mkdir "_work" #o755)

;;---------------------------------------------------------------
;; テスト開始
(test-start "kahua.config")
(use kahua.config)

;; ロードテスト
;; kahua.config がロードでき、またそのインターフェイスに齟齬が
;; ないことを確認する。
(test-module 'kahua.config)

(test* "loading config" #t
       (is-a? (kahua-init "./test.conf") <kahua-config>))

(test* "sockbase" "unix:_tmp"
       (kahua-sockbase))

(test* "set! sockbase" "unix:foo"
       (begin (set! (kahua-sockbase) "unix:foo")
              (kahua-sockbase)))

(test* "log path" "_work/logs/foo.log"
       (kahua-logpath "foo.log"))

(test* "config file" "./test.conf"
       (kahua-config-file))

(sys-system "rm -rf _work")

(test-end)

