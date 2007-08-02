;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Extended File System Database.
;;

(define *dbname* "efs:_tmpefs")
(sys-system "rm -rf _tmpefs")

(load "./persistence.scm")
