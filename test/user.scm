;; -*- coding: euc-jp ; mode: scheme -*-
;; $Id: user.scm,v 1.7.2.1 2007/07/05 05:14:56 bizenn Exp $

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

(define *dbpath* (build-path (sys-getcwd) "_tmpefs"))
(sys-system #`"rm -rf ,*dbpath*")
(define *dbname* #`"efs:,|*dbpath*|")

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
(define-class <my-kahua-user> (<kahua-user>) ())

(test* "kahua-check-user (in the empty db)" #f
       (with-db (db *dbname*)
         (kahua-check-user "shiro" "shiro")))

(test* "kahua-add-user" #t
       (with-db (db *dbname*)
         (every (cut is-a? <> <my-kahua-user>)
                (list (kahua-add-user "shiro"  "manapua")
                      (kahua-add-user "nobsun" "punahou")
                      (kahua-add-user "admin"  "kamehameha")))))

(test* "kahua-find-user" "shiro"
       (with-db (db *dbname*)
         (and-let* ((u (kahua-find-user "shiro")))
           (ref u 'login-name))))

(test* "kahua-user?" #t
       (with-db (db *dbname*)
	 (every kahua-user?
		(list (kahua-find-user "shiro")
		      (kahua-find-user "nobsun")
		      (kahua-find-user "admin")))))

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
		(let1 user (kahua-find-user "shiro")
		      (kahua-user-password-change user "manapua" "manahune")
		      (and-let* ((u (kahua-check-user "shiro" "manahune")))
				(ref u 'login-name)))))

(test* "kahua-user-password-change-force" "shiro"
       (with-db (db *dbname*)
		(let1 user (kahua-find-user "shiro")
		      (kahua-user-password-change-force user "urashima")
		      (and-let* ((u (kahua-check-user "shiro" "urashima")))
				(ref u 'login-name)))))

(test* "remove-kahua-instance: before sync" #t
       (with-db (db *dbname*)
	 (let1 user (kahua-find-user "shiro")
	   (remove-kahua-instance user)
	   (removed? user))))

(test* "remove-kahua-instance: after sync" #f (with-db (db *dbname*) (kahua-find-user "shiro")))

(test-end)
