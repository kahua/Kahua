;; Manages global session ID
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: gsid.scm,v 1.3 2003/12/28 13:01:19 shiro Exp $

(define-module kahua.gsid
  (use gauche.uvector)
  (use gauche.net)
  (use file.util)
  (use srfi-1)
  (use srfi-27)
  (export make-gsid decompose-gsid gsid->worker-id
          worker-id->sockaddr make-worker-id
          supervisor-sockaddr
          get-gsid-from-header add-gsid-to-header)
  )
(select-module kahua.gsid)

;;; Global session ID (GSID) -------------------------------------------
;;
;; Session ID consists of two strings.  One is a continuation ID, which
;; corresponds to a continuation of the session.   It can be restartable,
;; that is, the same continuation ID can be reused to backtrack the
;; session as far as it hasn't explicitly invalidated.  In other word,
;; a continuation ID has an unlimited extent by default.
;;
;; The other is a state ID, which stands for a monadic state of
;; the session.  It carries a stateful information---therefore can't 
;; be backtracked.
;;
;; In a typical web session, the continuation ID is kept in parameters
;; in POST request or in URL, whereas the state ID is kept in cookies.
;;
;; Both ID consists of the following format:
;;
;;    1-HHHHH-BBBBBBB
;;
;; The first '1' desginates the GSID version.  Hs and Bs are for a header
;; and a body.  The format after the version number can be changed
;; in the later versions.  For version 1, the header just includes worker ID.
;; The body is up to the worker.  Hs and Bs shouldn't include a minus sign.

(define (make-gsid worker-id body)
  (format "1-~a-~a" worker-id body))

(define (decompose-gsid gsid)
  (if (string? gsid)
    (let1 l (string-split gsid #\-)
      (if (and (= (length l) 3)
               (equal? (car l) "1"))
          (values (cadr l) (caddr l))
        (values #f gsid)))
    (values #f #f)))

(define (gsid->worker-id gsid)
  (and gsid (receive (h b) (decompose-gsid gsid) h)))

;; Convenience routines.
;; Header is like (("name1" "value1") ("name2" "value2")) but may be
;; changed later.
(define (get-gsid-from-header header)
  (values (cond ((assoc "x-kahua-sgsid" header) => cadr) (else #f))
          (cond ((assoc "x-kahua-cgsid" header) => cadr) (else #f))))

(define (add-gsid-to-header header stat-gsid cont-gsid)
  (define (add header name id)
    (if id
      (cons (list name id)
            (remove (lambda (entry) (equal? (car entry) name)) header))
      header))
  (add (add header "x-kahua-cgsid" cont-gsid) "x-kahua-sgsid" stat-gsid))

;; NB: This should be configurable!
(define *kahua-sock-base* "unix:/tmp/kahua")

(define (worker-id->sockaddr worker-id . opts)
  (if worker-id
    (receive (proto param)
        (string-scan (or (get-optional opts #f) *kahua-sock-base*) ":" 'both)
      (cond
       ((equal? proto "unix")
        (make <sockaddr-un> :path (build-path param worker-id)))
       (else
        (error "unsupported socket base: "
               (get-optional opts *kahua-sock-base*)))))
    (apply supervisor-sockaddr opts)))

(define (make-worker-id worker-type)
  (format "~a:~a:~a" worker-type
          (number->string (sys-getpid) 36)
          (number->string (random-integer 10000000) 36)))

(define (supervisor-sockaddr . opts)
  ;; this may not be correct if we use inet domain socket.
  (apply worker-id->sockaddr "kahua" opts))

;; TODO: should be done in system's initialize routine?
(random-source-randomize! default-random-source)

(provide "kahua/gsid")
