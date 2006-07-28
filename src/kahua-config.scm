;; package maintainance shell script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-config.scm,v 1.2 2006/07/28 13:09:43 bizenn Exp $
(use gauche.parseopt)
(use kahua.config)

;
; main
;
(define (main args)
  (let-args (cdr args)
      ((conf-file "c=s")
       (gosh      "gosh=s")
       (help      "h|help"))
    (if help (usage conf-file))
    (kahua-init conf-file)
    (let* ((conf (kahua-config))
	   (klass (class-of conf))
	   (slots (map car (ref klass 'slots)))
	   (lens (map (lambda (s)
			(string-length (x->string s)))
		      slots))
	   (max-len (apply max lens)))
      (cond ((null? (cdddr args))
	     (for-each (lambda (slot len)
			 (format #t "~a:" slot)
			 (display (make-string (- (+ max-len 4) len) #\sp))
			 (format #t "~a~%" (ref conf slot)))
		       slots lens))
	    (else (let1 slot (string->symbol (cadddr args))
		    (format #t "~a~%" (ref conf slot))))))))

(define (usage conf-file)
  (kahua-init conf-file)
  (let1 conf (kahua-config)
    (format #t "kahua-config [option]~%")
    (for-each (lambda (slot)
		(format #t "            ~a~%" slot))
	      (map car (ref (class-of conf) 'slots))))
  (exit 0))

