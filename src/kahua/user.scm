;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(define-module kahua.user
  (use kahua.persistence)
  (use rfc.md5)
  (use srfi-1)
  (use srfi-13)
  (use srfi-27)
  (use gauche.collection)
  (export <kahua-user> kahua-user? kahua-add-user kahua-check-user kahua-find-user
          kahua-user-password-change kahua-user-password-change-force
          kahua-user-has-role? kahua-user-add-role! kahua-user-drop-role!
          dbpath-of inactive? active? name-of roles-of kahua-current-user-class
          ))
(select-module kahua.user)

(define-class <kahua-user-meta> (<class>)
  ((%user-class :allocation :class)))

(define-class <kaua-user-mixin> () () :metaclass <kahua-user-meta>)

(define-method initialize ((class <kahua-user-meta>) initargs)
  (next-method)
  (slot-set! class '%user-class class))

(define-class <kahua-user> (<kahua-persistent-base> <kaua-user-mixin>)
  ((login-name    :allocation :persistent :accessor name-of
                  :init-keyword :login-name :index :unique)
   (password-hash :allocation :persistent
                  :init-keyword :password-hash :init-value #f)
   (role-alist    :allocation :persistent :accessor roles-of
                  :init-keyword :role-alist :init-value '())
   (inactive      :allocation :persistent :accessor inactive?
                  :init-keyword :inactive :init-value #f)
   ))

(define-method find-kahua-instance ((user-class <kahua-user-meta>)
                                    (login-name <string>)
                                    . args)
  (apply find-kahua-instance user-class 'login-name login-name))

(define-method dbpath-of ((user <kahua-user>))
  (path-of (ref user '%kahua-persistent-base::db)))

(define (kahua-current-user-class)
  (ref <kahua-user> '%user-class))

(define (kahua-user? obj)
  (eq? (kahua-current-user-class) (class-of obj)))

(define (kahua-find-user login-name)
  (find-kahua-instance (kahua-current-user-class) 'login-name login-name))

(define (kahua-add-user login-name password . kargs)
  (if (kahua-find-user login-name)
    #f
    (apply make (kahua-current-user-class)
           :login-name login-name
           :password-hash (crypt-passwd password)
           kargs)))

(define (kahua-check-user login-name password)
  (find (lambda (user)
          (and (equal? (ref user 'login-name) login-name)
               (active? user)
               (match-passwd password (ref user 'password-hash))))
        (make-kahua-collection (kahua-current-user-class))))

(define-method active? ((user <kahua-user>))
  (not (inactive? user)))

;; user must be <kahua-user>.  caller must sync db.
(define (kahua-user-password-change user old-password new-password)
  (and (is-a? user (kahua-current-user-class))
       (kahua-check-user (ref user 'login-name) old-password)
       (kahua-user-password-change-force user new-password)
       #t))

;; user must be <kahua-user>.  caller must sync db.
(define (kahua-user-password-change-force user new-password)
  (and (is-a? user (kahua-current-user-class))
       (begin (set! (ref user 'password-hash) (crypt-passwd new-password))
              #t)))


;; user may be <kahua-user> object or #f
;; role-alist's spec isn't fixed yet.  for now, we assume it's a list of
;; symbols.
;; ROLES arg is also a list of symbols.  Later we might add move syntax.
(define (kahua-user-has-role? user roles)
  (and (is-a? user (kahua-current-user-class))
       (list? roles)
       (find (lambda (urole) (find (cut eq? <> urole) roles))
             (ref user 'role-alist))))

(define (kahua-user-add-role! user role)
  (when (and user (symbol? role))
    (unless (memq role (roles-of user))
      (push! (roles-of user) role))))

(define (kahua-user-drop-role! user role)
  (when (and user (symbol? role))
    (set! (roles-of user) (delete role (roles-of user) eq?))))

;; internal utility
(define (crypt-passwd passwd)
  (let* ((salt (format "~a~a"
                       (integer->char (+ (random-integer 94) 33))
                       (integer->char (+ (random-integer 94) 33))))
         (enc  (digest-hexify (md5-digest-string (string-append salt passwd))))
         )
    (string-append salt enc)))

(define (match-passwd passwd hashed)
  (let* ((salt (string-take hashed 2))
         (enc  (digest-hexify (md5-digest-string (string-append salt passwd))))
         )
    (string=? (string-drop hashed 2) enc)))

(provide "kahua/user")
