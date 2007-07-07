;;
;; kahua.util - miscellaneous utility collection
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: util.scm,v 1.11 2007/07/07 22:34:36 bizenn Exp $

;; This module contains generally useful routines, which don't belong to
;; a particular module.

(define-module kahua.util
  (use srfi-1)
  (use srfi-19)
  (use util.list)
  (use file.util)
  (use gauche.collection)
  (use gauche.sequence)
  (use gauche.parseopt)
  (use gauche.charconv)
  (use kahua.config)
  (export kahua-error-string
	  <kahua-error>
	  kahua-error?
	  with-sigmask
	  filter-map1
	  ref-car
	  assq-ref-car
	  assoc-ref-car
	  http-date->date
	  time->rfc1123-string
	  date->rfc1123-string
	  setuidgid!
	  write-pid-file
	  read-pid-file
	  check-pid
	  make-filter-pipeline
	  kahua:call-with-output-file
	  ))
(select-module kahua.util)

(define-condition-type <kahua-error> <error> kahua-error?)
(define-condition-type <kahua-exception> <message-condition> kahua-exception?)

;; utility
(define (ref-car cmp lis item . maybe-default)
  (cond ((assoc item lis cmp) => cadr)
        (else (get-optional maybe-default #f))))
(define assq-ref-car  (pa$ ref-car eq?))
(define assoc-ref-car (pa$ ref-car equal?))

;; KAHUA-ERROR-STRING <error> [detail?]
;;  Returns a string representation of error.  If detail? is given,
;;  includes the stack trace.  Otherwise, just an error message.

(define (kahua-error-string e . maybe-detail?)
  (if (get-optional maybe-detail? #f)
    (call-with-output-string
      (cut with-error-to-port <> (cut report-error e)))
    (ref e 'message)))

(define (with-sigmask how mask thunk)
  (let1 old_sigset (sys-sigmask how mask)
    (unwind-protect (thunk) (sys-sigmask SIG_SETMASK old_sigset))))

(define-method filter-map1 (f (c <collection>))
  (reverse!
   (fold (lambda (e res)
	   (let1 v (f e)
	     (if v (cons v res) res)))
	 '()
	 c)))

;;
;; HTTP/1.1 Date string handling
;;

(define-constant days-of-week-abbrev
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define-constant days-of-week-full
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(define-constant month-abbrev
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;; Sun, 06 Nov 1994 08:49:37 GMT  <= RFC 822, updated by RFC 1123
(define-constant rfc1123-date-rx
  #/^(Sun|Mon|Tue|Wed|Thu|Fri|Sat), (\d\d) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) (\d\d\d\d) (\d\d):(\d\d):(\d\d) GMT$/)
;; Sunday, 06-Nov-94 08:49:37 GMT <= RFC 850, obsoleted by RFC 1036
(define-constant rfc850-date-rx
  #/^(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday), (\d\d)-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-(\d\d) (\d\d):(\d\d):(\d\d) GMT$/)
;; Sun Nov  6 08:49:37 1994       <= ANSI C's asctime() format
(define-constant asctime-date-rx
  #/^(Sun|Mon|Tue|Wed|Thu|Fri|Sat) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)  ?(\d?\d) (\d\d):(\d\d):(\d\d) (\d\d\d\d)/)

(define (http-date->date str)
  (define (yy->integer yy)
    (let ((y (x->integer yy)))
      (cond
       ((< y 70) (+ 2000 y))
       ((< y 100) (+ 1900 y))
       (else y))))
  (define (mon->integer mon)
    (+ (find-index (pa$ string=? mon) month-abbrev) 1))

  (rxmatch-case str
    (rfc1123-date-rx (#f #f date mon year hour min sec)
		     (make-date 0
				(x->integer sec)
				(x->integer min)
				(x->integer hour)
				(x->integer date)
				(mon->integer mon)
				(x->integer year)
				0))
    (rfc850-date-rx (#f #f date mon yy hour min sec)
		    (make-date 0
			       (x->integer sec)
			       (x->integer min)
			       (x->integer hour)
			       (x->integer date)
			       (mon->integer mon)
			       (yy->integer yy)
			       0))
    (asctime-date-rx (#f #f mon date hour min sec year)
		     (make-date 0
				(x->integer sec)
				(x->integer min)
				(x->integer hour)
				(x->integer date)
				(mon->integer mon)
				(x->integer year)
				0))
    (else #f)))

(define-method time->rfc1123-string ((time <number>))
  (sys-strftime "%a, %d %b %Y %H:%M:%S GMT" (sys-gmtime time)))
(define-method time->rfc1123-string ((time <time>))
  (time->rfc1123-string (time->seconds time)))
(define (date->rfc1123-string date)
  (time->rfc1123-string (date->time-utc date)))

(define (setuidgid! user:group)
  (when user:group
    (and-let* ((m (#/([^\:]+)(?::([^\:]+))?/ user:group))
	       (pw (sys-getpwnam (m 1)))
	       (uid (ref pw 'uid))
	       (gid (or (and-let* ((g (m 2)))
			  (sys-group-name->gid g))
			(ref pw 'gid))))
      (sys-setgid gid)
      (sys-setuid uid))))

(define (check-pid pid)
  (guard (e (else #f))
    (sys-kill pid 0)
    #t))

(define (read-pid-file path)
  (with-input-from-file path read :if-does-not-exist #f))

(define (write-pid-file path)
  (and-let* ((pid (read-pid-file path)))
    (when (check-pid pid)
      (error <kahua-error> :message (format "Process #~d on PID file ~s" pid path)))
    (sys-unlink path))
  (with-output-to-file path (cut write (sys-getpid)) :if-exists :error))

(define (make-filter-pipeline filter-list)
  (let1 filtered-filters (filter identity filter-list)
    (lambda (obj)
      (let/cc break
	(fold (lambda (f obj)
		(if obj
		    (and (f obj) obj)
		    (break #f)))
	      obj
	      filtered-filters)))))

(define (kahua:call-with-output-file outfile proc . kargs)
  (let-keywords kargs ((backup-file #f)
		       (tmpbase     #f)
		       (perm        #f)
		       (encoding (gauche-character-encoding))
		       . kargs)
    (let1 outdir (sys-dirname outfile)
      (make-directory* outdir)
      (receive (out tmpfile)
	  (sys-mkstemp (build-path outdir (or tmpbase "kahua-tmp-")))
	(let1 out (wrap-with-output-conversion out encoding)
	  (when (and backup-file (file-is-regular? outfile))
	    (sys-link outfile backup-file))
	  (guard (e (else
		     (sys-unlink tmpfile)
		     (unless (port-closed? out)
		       (close-output-port out))
		     (raise e)))
	    (proc out outfile)
	    (close-output-port out)
	    (when perm
	      (sys-chmod tmpfile perm))
	    (sys-rename tmpfile outfile)))))))

(provide "kahua/util")
