;; -*- coding: euc-jp ; mode: scheme -*-
;; $Id: user.scm,v 1.4 2005/12/13 17:33:35 cut-sea Exp $

;; test kahua.user

(use gauche.test)
(use srfi-2)
(use rfc.md5)
(use kahua.persistence)
(use file.util)

(test-start "user")
(use srfi-1)
(use kahua.user)
(test-module 'kahua.user)

(define *dbname* (build-path (sys-getcwd) "_tmp"))
(sys-system #`"rm -rf ,*dbname*")

(test* "kahua-check-user (in the empty db)" #f
       (with-db (db *dbname*)
         (kahua-check-user "shiro" "shiro")))

(test* "kahua-add-user" #t
       (with-db (db *dbname*)
         (every (cut is-a? <> <kahua-user>)
                (list (kahua-add-user "shiro"  "manapua")
                      (kahua-add-user "nobsun" "punahou")
                      (kahua-add-user "admin"  "kamehameha")))))

(test* "kahua-find-user" "shiro"
       (with-db (db *dbname*)
         (and-let* ((u (kahua-find-user "shiro")))
           (ref u 'login-name))))

(test* "kahua-find-user" #f
       (with-db (db *dbname*)
         (and-let* ((u (kahua-find-user "shirok")))
           (ref u 'login-name))))

(test* "kahua-add-user (dup)" #f
       (with-db (db *dbname*)
         (kahua-add-user "nobsun" "makapuu")))

(test* "kahua-add-user (non-dup)" #t
       (with-db (db *dbname*)
         (not (not (kahua-add-user "guest" "molokai")))))
         
(test* "kahua-check-user" "shiro"
       (with-db (db *dbname*)
         (and-let* ((u (kahua-check-user "shiro" "manapua")))
           (ref u 'login-name))))

(test* "kahua-check-user" #f
       (with-db (db *dbname*)
         (and-let* ((u (kahua-check-user "shiro" "makapuu")))
           (ref u 'login-name))))

(test* "kahua-check-user" #f
       (with-db (db *dbname*)
         (and-let* ((u (kahua-check-user "shirok" "makapuu")))
           (ref u 'login-name))))

(test* "kahua-user-password-change" "shiro"
       (with-db (db *dbname*)
		(let1 user (find-kahua-instance <kahua-user> "shiro")
		      (kahua-user-password-change user "manapua" "manahune")
		      (and-let* ((u (kahua-check-user "shiro" "manahune")))
				(ref u 'login-name)))))
		      
(test* "kahua-user-password-change-force" "shiro"
       (with-db (db *dbname*)
		(let1 user (find-kahua-instance <kahua-user> "shiro")
		      (kahua-user-password-change-force user "urashima")
		      (and-let* ((u (kahua-check-user "shiro" "urashima")))
				(ref u 'login-name)))))
		      
;;
;; app subclass of <kahua-user>.
;;
(define-class <test-user> (<kahua-user>)
  ((fname  :allocation :persistent :init-keyword :fname
	   :init-value "" :accessor fname-of)
   (lname  :allocation :persistent :init-keyword :lname
	   :init-value "" :accessor lname-of)))


(test* "check-user (in the empty db)" #f
       (with-db (db *dbname*)
         (check-user <test-user> "cut-sea" "cutsea")))

(test* "add-user" #t
       (with-db (db *dbname*)
         (every (cut is-a? <> <test-user>)
                (list (add-user <test-user> "shiro"  "manapua")
                      (add-user <test-user> "nobsun" "punahou")
                      (add-user <test-user> "cut-sea"  "kamosimakuri-bu")))))

(test* "find-user" "cut-sea"
       (with-db (db *dbname*)
         (and-let* ((u (find-user <test-user> "cut-sea")))
           (ref u 'login-name))))

(test* "find-user" #f
       (with-db (db *dbname*)
         (and-let* ((u (find-user <test-user> "shirok")))
           (ref u 'login-name))))

(test* "add-user (dup)" #f
       (with-db (db *dbname*)
         (add-user <test-user> "nobsun" "makapuu")))

(test* "add-user (non-dup)" #t
       (with-db (db *dbname*)
         (not (not (add-user <test-user> "guest" "molokai")))))
         
(test* "check-user" "cut-sea"
       (with-db (db *dbname*)
         (and-let* ((u (check-user <test-user> "cut-sea" "kamosimakuri-bu")))
           (ref u 'login-name))))

(test* "check-user" #f
       (with-db (db *dbname*)
         (and-let* ((u (check-user <test-user> "shiro" "makapuu")))
           (ref u 'login-name))))

(test* "check-user" #f
       (with-db (db *dbname*)
         (and-let* ((u (check-user <test-user> "shirok" "makapuu")))
           (ref u 'login-name))))

(test* "user-password-change" "cut-sea"
       (with-db (db *dbname*)
		(let1 user (find-kahua-instance <test-user> "cut-sea")
		      (user-password-change user "kamosimakuri-bu" "kamosiJapan")
		      (and-let* ((u (check-user <test-user> "cut-sea" "kamosiJapan")))
				(ref u 'login-name)))))
		      
(test* "user-password-change-force" "cut-sea"
       (with-db (db *dbname*)
		(let1 user (find-kahua-instance <test-user> "cut-sea")
		      (user-password-change-force user "kamosuzo!")
		      (and-let* ((u (check-user <test-user> "cut-sea" "kamosuzo!")))
				(ref u 'login-name)))))
		      
		      

(test-end)
