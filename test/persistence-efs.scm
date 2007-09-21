;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Extended File System Database.
;;
;; $Id: persistence-efs.scm,v 1.1 2006/10/24 06:14:53 bizenn Exp $

(define *dbname* "efs:_tmpefs")
(sys-system "rm -rf _tmpefs")

(load "./persistence.scm")
