;;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots (efs)
;;
;; $Id: index-slots-efs.scm,v 1.1 2006/11/27 07:18:37 bizenn Exp $

(define *dbname* "efs:_tmpefs")
(sys-system "rm -rf _tmpefs*")

(load "./index-slots.scm")
