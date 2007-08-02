;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Old File System Database.
;; !! This is very obsolete. !!
;;

(define *dbname* "fs:_tmp")
(sys-system "rm -rf _tmp")

(load "./persistence.scm")
