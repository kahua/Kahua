;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots (PostgreSQL)
;;

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"postgresql:,|*user*|::")

(load "./persistence-dbi.scm")
(cleanup-db "pg" *user* "" "")

(load "./index-slots.scm")
