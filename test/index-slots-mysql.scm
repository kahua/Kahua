;;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots (MySQL)
;;
;; $Id: index-slots-mysql.scm,v 1.1 2006/11/27 07:18:37 bizenn Exp $

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"mysql:,|*user*|::db=test")

(load "./persistence-dbi.scm")
(cleanup-db "mysql" *user* "" "db=test")

(load "./index-slots.scm")
