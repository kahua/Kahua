;; Provides dbm interface on top of Kahua db
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: dbm.scm,v 1.1 2004/01/14 00:50:54 shiro Exp $

;; This module implements dbm protocol on top of kahua's persistent objects.

(define-module kahua.dbm
  (use srfi-2)
  (use kahua.persistence)
  (use gauche.sequence)
  (extend dbm)
  (export <kahua-dbm> <kahua-dbm-record>))
(select-module kahua.dbm)

;; dbm expects a record consists of a string key and a string value.
;; In Kahua, each 'dbm database' is mapped to a class which subclasses
;; <kahua-dbm-record>

(define-class <kahua-dbm-record-meta> (<kahua-persistent-meta>)
  ())

(define-class <kahua-dbm-record> (<kahua-persistent-base>)
  ((key      :allocation :persistent :init-value "" :init-keyword :key)
   (value    :allocation :persistent :init-value "" :init-keyword :value)
   (deleted? :allocation :persistent :init-value #f))
  :metaclass <kahua-dbm-record-meta>)

(define-method key-of ((self <kahua-dbm-record>))
  (ref self 'key))

;; dbm protocol implementation
;; :path argument takes the _class_ object

(define-class <kahua-dbm-meta> (<dbm-meta>) ())

(define-class <kahua-dbm> (<dbm>)
  ((closed? :init-value #f))
  :metaclass <kahua-dbm-meta>)

(define (kahua-dbm-class dbm) (ref dbm 'path))

(define-method dbm-open ((self <kahua-dbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (error "path must be set to open kahua-dbm database"))
  (unless (is-a? (ref self 'path) <kahua-dbm-record-meta>)
    (error "path of kahua-dbm must be a class object that inherits <kahua-dbm-record>, but got:" (ref self 'path)))
  self)

(define-method dbm-close ((self <kahua-dbm>))
  (set! (ref self 'closed?) #t)
  #t)

(define-method dbm-closed? ((self <kahua-dbm>))
  (ref self 'closed?))

(define-method dbm-put! ((self <kahua-dbm>) key value)
  (next-method)
  (let* ((k (%dbm-k2s self key))
         (v (%dbm-v2s self value))
         (class (kahua-dbm-class self))
         (obj (find-kahua-instance class k)))
    (if obj
      (begin
        (set! (ref obj 'value) v)
        (set! (ref obj 'deleted?) #f))
      (make class :key k :value v))
    #t))

(define-method dbm-get ((self <kahua-dbm>) key . args)
  (next-method)
  (let* ((k (%dbm-k2s self key))
         (class (kahua-dbm-class self))
         (obj (find-kahua-instance class k)))
    (if (and obj (not (ref obj 'deleted?)))
      (%dbm-s2v self (ref obj 'value))
      (get-optional args
                    (errorf "kahua-dbm: no data for key ~s in database ~s"
                            key class)))))

(define-method dbm-exists? ((self <kahua-dbm>) key)
  (next-method)
  (and-let* ((obj (find-kahua-instance (kahua-dbm-class self)
                                       (%dbm-k2s self key))))
    (not (ref obj 'deleted?))))

(define-method dbm-delete! ((self <kahua-dbm>) key)
  (next-method)
  (and-let* ((obj (find-kahua-instance (kahua-dbm-class self)
                                       (%dbm-k2s self key))))
    (set! (ref obj 'deleted?) #t)
    #t))

(define-method dbm-fold ((self <kahua-dbm>) proc seed)
  (next-method)
  (fold (lambda (obj seed)
          (if (ref obj 'deleted?)
            seed
            (proc (%dbm-s2k self (ref obj 'key))
                  (%dbm-s2v self (ref obj 'value))
                  seed)))
        seed (make-kahua-collection (kahua-dbm-class self))))

(define-method dbm-db-exists? ((class <kahua-dbm-meta>) name)
  (next-method)
  (is-a? name <kahua-dbm-record-meta>))

(define-method dbm-db-remove ((class <kahua-dbm-meta>) name)
  (next-method)
  (errorf "removing kahua-dbm database isn't supported yet"))

(provide "kahua/dbm")


