;;
;; kahua.util - miscellaneous utility collection
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: util.scm,v 1.3 2005/09/12 09:45:06 shiro Exp $

;; This module contains generally useful routines, which don't belong to
;; a particular module.

(define-module kahua.util
  (use srfi-1)
  (use util.list)
  (export kahua-error-string))
(select-module kahua.util)

;; KAHUA-ERROR-STRING <error> [detail?]
;;  Returns a string representation of error.  If detail? is given,
;;  includes the stack trace.  Otherwise, just an error message.

(define (kahua-error-string e . maybe-detail?)
  (if (get-optional maybe-detail? #f)
    (call-with-output-string
      (cut with-error-to-port <> (cut report-error e)))
    (ref e 'message)))

(provide "kahua/util")
