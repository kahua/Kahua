;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Old File System Database.
;; !! This is very obsolete. !!
;;
;; $Id: persistence-fs.scm,v 1.1 2006/12/02 07:11:36 bizenn Exp $

(define *dbname* "fs:_tmp")
(sys-system "rm -rf _tmp")

(load "./persistence.scm")
