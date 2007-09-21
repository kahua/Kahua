;;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots (PostgreSQL)
;;
;; $Id: index-slots-pg.scm,v 1.1 2006/11/28 03:52:57 bizenn Exp $

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"postgresql:,|*user*|::")

(load "./persistence-dbi.scm")
(cleanup-db "pg" *user* "" "")

(load "./index-slots.scm")
