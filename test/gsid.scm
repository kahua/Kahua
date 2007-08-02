;; -*- coding: utf-8 ; mode: scheme -*-
;; test gsid module

(use gauche.test)
(use gauche.net)

(test-start "gsid")
(use kahua.gsid)
(test-module 'kahua.gsid)

(test* "make-gsid" "1-keeaumoku:1312:1442432-a2VlYXVtb2t1"
       (make-gsid "keeaumoku:1312:1442432" "a2VlYXVtb2t1"))

(test* "decompose-gsid" '("keeaumoku:1312:1442432" "a2VlYXVtb2t1")
       (receive r (decompose-gsid "1-keeaumoku:1312:1442432-a2VlYXVtb2t1") r))

(test* "decompose-gsid(proc)" '(#f "abc")
       (receive r (decompose-gsid "abc") r))

(test* "get-gsid-from-header" '(#f #f)
       (receive r (get-gsid-from-header '()) r))
(test* "get-gsid-from-header" '("abc" "def")
       (receive r
           (get-gsid-from-header '(("timestamp" "45934895834")
                                   ("x-kahua-cgsid" "def")
                                   ("status" "haleakala")
                                   ("x-kahua-sgsid" "abc")))
         r))
(test* "get-gsid-from-header" '("abc" #f)
       (receive r
           (get-gsid-from-header '(("timestamp" "45934895834")
                                   ("status" "haleakala")
                                   ("x-kahua-sgsid" "abc")))
         r))

(test* "add-gsid-to-header" '(("x-kahua-sgsid" "abc")
                              ("x-kahua-cgsid" "def"))
       (add-gsid-to-header '() "abc" "def"))
(test* "add-gsid-to-header" '(("x-kahua-sgsid" "abc")
                              ("x-kahua-cgsid" "def")
                              ("timestamp" "45934895834")
                              ("status" "haleakala"))
       (add-gsid-to-header '(("timestamp" "45934895834")
                             ("x-kahua-cgsid" "zzz")
                             ("status" "haleakala")
                             ("x-kahua-sgsid" "yyy"))
                           "abc" "def"))

(test* "worker-id->sockaddr"
       '(#t "/tmp/kahua/keeeaumoku:1312:1442432")
       (let ((a (worker-id->sockaddr "keeeaumoku:1312:1442432")))
         (list (is-a? a <sockaddr-un>)
               (sockaddr-name a))))

(test* "worker-id->sockaddr"
       '(#t "/home/shiro/keeeaumoku:1312:1442432")
       (let ((a (worker-id->sockaddr "keeeaumoku:1312:1442432"
                                     "unix:/home/shiro")))
         (list (is-a? a <sockaddr-un>)
               (sockaddr-name a))))

(test* "worker-id->sockaddr" *test-error* ;; this will eventually be supported
       (let ((a (worker-id->sockaddr "keeeaumoku:1312:1442432"
                                     "inet:localhost:1234")))
         (list (is-a? a <sockaddr-in>)
               (sockaddr-name a))))

(test* "make-worker-id" #t
       (not (#/^aaa:\d:\d+$/ (make-worker-id "aaa"))))

(test-end)
