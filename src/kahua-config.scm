;; package maintainance shell script
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
(use gauche.parseopt)
(use kahua.config)

;
; main
;
(define (main args)
  (let-args (cdr args)
      ((site "S=s")
       (conf-file "c=s")
       (gosh      "gosh=s")
       (help      "h|help" => usage) . args)
    (kahua-common-init site conf-file)
    (let* ((conf (kahua-config))
	   (klass (class-of conf))
	   (slots (map car (ref klass 'slots)))
	   (lens (map (lambda (s)
			(string-length (x->string s)))
		      slots))
	   (max-len (apply max lens)))
      (cond ((null? args)
	     (for-each (lambda (slot len)
			 (format #t "~a:" slot)
			 (display (make-string (- (+ max-len 4) len) #\sp))
			 (format #t "~a~%" (ref conf slot)))
		       slots lens))
	    (else
	     (let1 slot (string->symbol (car args))
		    (format #t "~a~%" (ref conf slot))))))))

(define (usage conf-file)
  (let1 conf (kahua-config)
    (format #t "kahua-config [option]~%")
    (for-each (lambda (slot)
		(format #t "            ~a~%" slot))
	      (map car (ref (class-of conf) 'slots))))
  (exit 0))

