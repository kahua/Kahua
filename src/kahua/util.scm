;;
;; kahua.util - miscellaneous utility collection
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: util.scm,v 1.3.8.3 2006/07/10 02:15:58 bizenn Exp $

;; This module contains generally useful routines, which don't belong to
;; a particular module.

(define-module kahua.util
  (use srfi-1)
  (use util.list)
  (use gauche.collection)
  (export kahua-error-string
	  with-sigmask
	  filter-map1))
(select-module kahua.util)

;; KAHUA-ERROR-STRING <error> [detail?]
;;  Returns a string representation of error.  If detail? is given,
;;  includes the stack trace.  Otherwise, just an error message.

(define (kahua-error-string e . maybe-detail?)
  (if (get-optional maybe-detail? #f)
    (call-with-output-string
      (cut with-error-to-port <> (cut report-error e)))
    (ref e 'message)))

(define (with-sigmask how mask thunk)
  (let1 old_sigset #f
    (dynamic-wind
	(lambda () (set! old_sigset (sys-sigmask how mask)))
	thunk
	(lambda () (sys-sigmask SIG_SETMASK old_sigset)))))

(define-method filter-map1 (f (c <collection>))
  (reverse!
   (fold (lambda (e res)
	   (let1 v (f e)
	     (if v (cons v res) res)))
	 '()
	 c)))

(provide "kahua/util")
