;; -*- mode: scheme; coding: utf-8 -*-
;; Tests for Index Slots (MySQL)
;;

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"mysql:,|*user*|::db=test")

(load "./persistence-dbi.scm")
(cleanup-db "mysql" *user* "" "db=test")

(load "./index-slots.scm")
