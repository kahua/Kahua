;; -*- coding: euc-jp ; mode: scheme -*-
;; test kahua.partcont
;; $Id: partcont.scm,v 1.2 2005/07/04 05:09:21 nobsun Exp $

(use gauche.test)

(use kahua.partcont)
(test-module 'kahua.partcont)

(test-start "kahua.partcont")

(test* "reset/pc" 4
       (+ 1 (reset/pc 3)))

(test* "reset/pc, let/pc" 5
       (+ 1 (reset/pc (* 2 (let/pc k 4)))))

(test* "reset/pc, let/pc" 9
       (+ 1 (reset/pc (* 2 (let/pc k (k 4))))))

(test* "reset/pc, let/pc" 17
       (+ 1 (reset/pc (* 2 (let/pc k (k (k 4)))))))
       

(test-end)
