;; -*- coding: utf-8 ; mode: scheme -*-
;; test kahua.partcont

(use gauche.test)

(use kahua.partcont)
(test-module 'kahua.partcont)

(test-start "kahua.partcont")

(test* "reset" 4
       (+ 1 (reset 3)))

(test* "reset, let/pc" 5
       (+ 1 (reset (* 2 (let/pc k 4)))))

(test* "reset, let/pc" 9
       (+ 1 (reset (* 2 (let/pc k (k 4))))))

(test* "reset, let/pc" 17
       (+ 1 (reset (* 2 (let/pc k (k (k 4)))))))


(test-end)
