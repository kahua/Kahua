;; package maintainance shell script
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua-config.scm,v 1.1.2.1 2006/07/16 13:13:30 cut-sea Exp $
(use gauche.parseopt)
(use kahua.config)

;
; main
;
(define (main args)
  (let-args (cdr args)
      ((conf-file "c=s")
       (gosh      "gosh=s"))
    (kahua-init conf-file)
    (let* ((conf (kahua-config))
	   (klass (class-of conf))
	   (slots (map car (ref klass 'slots)))
	   (lens (map (lambda (s)
			(string-length (x->string s)))
		      slots))
	   (max-len (apply max lens)))
      (for-each (lambda (slot len)
		  (format #t "~a:" slot)
		  (display (make-string (- (+ max-len 4) len) #\sp))
		  (format #t "~a~%" (ref conf slot)))
		slots lens))))

(define (usage)
  (print "kahua-config")
  (exit 0))

