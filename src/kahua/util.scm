;;
;; kahua.util - miscellaneous utility collection
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: util.scm,v 1.1 2004/01/22 13:32:12 shiro Exp $

;; This module contains generally useful routines, which don't belong to
;; a particular module.

(define-module kahua.util
  (use srfi-1)
  (use util.list)
  (export kahua-error-string))
(select-module kahua.util)

;; KAHUA-FORMAT-ERROR <error> [detail?]
;;  Returns a string representation of error.  If detail? is given,
;;  includes the stack trace.  Otherwise, just an error message.

(define (kahua-error-string e . maybe-detail?)
  (if (get-optional maybe-detail? #f)
    (ref e 'message)
    (call-with-output-string
      (cut with-error-to-port <> (cut report-error e)))))

(provide "kahua/util")
