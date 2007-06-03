;; Kahua user (<kahua-user> instance) management tool
;;
;;  Copyright (c) 2004-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-user.scm,v 1.2 2007/06/03 13:46:36 bizenn Exp $

(use kahua.user)
(use kahua.persistence)
(use kahua.config)
(use file.util)
(use gauche.parseopt)
(use gauche.termios)
(use gauche.collection)
(use gauche.parameter)

(define program-name (make-parameter #f))

(define (usage)
  (format (current-error-port)
	  "Usage: ~a [options] (add|del|ls|help) [args]
 Options:
    -S site-dir     site bundle directory's path.
    -c conf-file    configuration file's path.
    -D dbname       database name.

 Command \"add\" usage:
    add [-r role1,role2,...] <username> [<password>]

 Command \"del\" usage:
    del <username>

 Command \"ls\" usage:
    ls
   
" (program-name))
  (exit 70))

(define (main args)
  (set! (port-buffering (current-output-port)) :none)
  (program-name (sys-basename (car args)))
  (let-args (cdr args)
      ((gosh      "gosh=s")		; DUMMY, not used.
       (conf-file "c=s")
       (site-dir  "S=s")
       (dbpath "D=s")
       . rargs)
    (kahua-common-init site-dir conf-file)
    (let1 thunk
	(let-optionals* rargs ((command "help") . rargs)
	  (cond ((string=? command "add") (cut apply kahua-adduser rargs))
		((string=? command "del") (cut apply kahua-deluser rargs))
		((string=? command "ls")  (cut apply kahua-lsuser rargs))
		(else                     (usage))))
      (with-db (db (kahua-dbpath (or dbpath (kahua-default-database-name))))
	(thunk))))
  0)

(define (kahua-adduser . args)
  (define (parse-roles roles)
    (if roles
	(map string->symbol (string-split roles #[ ,]))
	'()))
  (let-args args ((roles "r=s") . args)
    (let-optionals* args ((user (usage)) (pass (get-password user)))
      (when user
	(if (kahua-add-user user pass :role-alist (parse-roles roles))
	    (format (current-error-port) "\nCreate user: ~a\n" user)
	    (format (current-error-port) "\nUser already exists: ~a\n" user))))))

(define (kahua-deluser . args)
  (cond ((get-optional args (usage))
	 => (lambda (uname)
	      (cond ((kahua-find-user uname)
		     => (lambda (u)
			 (remove-kahua-instance u)
			 (display "done\n")))
		    (else (format #t "No such user: ~a\n" uname)))))
	(else (usage))))

(define (kahua-lsuser . _)
  (for-each (lambda (u)
	      (format #t "~16a ~s\n" (slot-ref u 'login-name) (slot-ref u 'role-alist)))
	    (sort (coerce-to <list> (make-kahua-collection (kahua-current-user-class)))
		  (lambda (a b) (string<? (name-of a) (name-of b))))))

(define (prompt&read-line port prompt)
  (format #t "~a: " prompt)
  (read-line port))

(define (get-password uname)
  (with-noecho (current-input-port)
    (lambda (port)
      (let loop ()
	(let1 pw (prompt&read-line
		  port (format "Type ~s initial password" uname))
	  (unless (string? pw) (abort))
	  (let1 pw2 (prompt&read-line
		     port (format "\nType ~s initial password (again)" uname))
	    (unless (string? pw2) (abort))
	    (cond ((equal? pw pw2) pw)
		  (else
		   (display "\nDon't match, try again\n")
		   (loop)))))))))

(define (abort)
  (display "\naborted.\n")
  (exit 70))

(define (with-noecho port proc)
  (let* ((attr (sys-tcgetattr port))
         (lflag (slot-ref attr 'lflag)))
    (dynamic-wind
        (lambda ()
          (slot-set! attr 'lflag (logand lflag (lognot (logior ECHO ECHOE ECHOK ECHONL))))
          (sys-tcsetattr port TCSAFLUSH attr))
	(cut proc port)
        (lambda ()
          (slot-set! attr 'lflag lflag)
          (sys-tcsetattr port TCSANOW attr)))))
