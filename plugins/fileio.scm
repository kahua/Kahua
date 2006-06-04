;;; upload and download plugin

(define-plugin "fileio"
  (version "0.5")
  (export open-uploaded-file
	  call-with-uploaded-file
	  with-input-from-uploaded-file
	  get-original-name
	  save-uploaded-file
	  send-downloaded-file)
  (depend #f))

(define-export (open-uploaded-file spec)
  (let1 tmpf (car spec)
    (open-input-file tmpf)))

(define-export (call-with-uploaded-file spec proc)
  (let1 in (open-uploaded-file spec)
    (with-error-handler
      (lambda (e) (close-input-port in) (raise e))
      (lambda ()
	(begin0
	  (proc in)
	  (close-input-port in))))))

(define-export (with-input-from-uploaded-file spec thunk)
  (call-with-uploaded-file spec (cut with-input-from-port <> thunk)))
       
(define-export (get-original-name spec)
  (cadr spec))

(define-export (save-uploaded-file spec path)
  (sys-rename (car spec) path))

(define-export (send-downloaded-file file fname)
  `((file (,file ,fname))))

(with-module kahua.server
  (use srfi-14)
  (use rfc.uri)
  (use rfc.base64)
  (use rfc.quoted-printable)
  (use gauche.charconv)
  (use gauche.sequence)

  (define (raw-word word charset)
    (if (ces-equivalent? charset (gauche-character-encoding) #t)
	word
	(ces-convert word (gauche-character-encoding) charset)))

  (define (in-char-set? str cs)
    (call/cc (lambda (k)
	       (for-each (lambda (c)
			   (or (char-set-contains? cs c)
			       (k #f)))
			 str)
	       #t)))

(define (mime-encode-word word . args)
  (define (%do-encoding word charset encoding proc)
    (with-output-to-string
      (lambda ()
	(format #t "=?~a?~s?~a?=" charset encoding (proc (raw-word word charset))))))
  (cond ((in-char-set? word char-set:printing) word)
	(else
	 (let-keywords* args
	     ((charset (gauche-character-encoding))
	      (encoding :b))
	   (case encoding
	     ((:b :B) (%do-encoding word charset 'B base64-encode-string))
	     ((:q :Q) (%do-encoding word charset 'Q quoted-printable-encode-string))
	     (else (errorf #`"Unsupported encoding: \"~s\"" encoding)))))))

  (define (rfc2231-encode-word word . args)
    (let-keywords* args
	((charset (gauche-character-encoding))
	 (language ""))
      (with-output-to-string
	(lambda ()
	  (format #t "~a'~a'~a" charset language (uri-encode-string (raw-word word charset)))))))

  (define (interp-file nodes context cont)
    (let* ((hdrs (assoc-ref-car context "extra-headers" '()))
	   (ua (kahua-meta-ref "HTTP_USER_AGENT"))
	   (fname (cadadr nodes))
	   (data (caadr nodes))
	   (cd-value (rxmatch-case ua
		       (#/Opera \d+\.\d+$/ (#f)	               ; Opera
		        (format "attachment; filename=~a" (raw-word fname "UTF-8")))
		       (#/compatible\; *MSIE *\d+\.\d+\;/ (#f) ; Microsoft Internet Explorer on Windows.
			(format "attachment; filename=~a" (uri-encode-string (raw-word fname "UTF-8"))))
		       (#/AppleWebKit\/\d+/ (#f)               ; Apple WebKit base Browser(Safari, Shiira, etc..)
			(format "attachment; filename=~a" (raw-word fname "UTF-8")))
		       (#/Gecko\/\d+/ (#f)                     ; Mozilla Family (based on Gecko)
			(format "attachment; filename*=~a" (rfc2231-encode-word fname)))
		       (else		                       ; Unknown
			(format "attachment; filename=~a" (mime-encode-word fname)))))
	   (context (cons `("extra-headers"
			    ,(kahua-merge-headers
			      `(("content-type" "application/octet-stream")
				("Content-Disposition" ,cd-value))))
			  context)))
      (cont `(file ,data ,fname) context)))
  (add-interp! 'file interp-file))

;;; Local Variables:
;;; mode: scheme
;;; coding: utf-8-unix
;;; End:
