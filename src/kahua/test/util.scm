;;
;; Utilities to implement test script.
;;
;;  Copyright (c) 2008 Kahua Project, All rights reserved.

(define-module kahua.test.util
  (use gauche.process)
  (export kahua:invoke&wait))

(select-module kahua.test.util)

(define (kahua:invoke&wait cmd&args . args)
  (let-keywords* args ((prompt #f)
		       (reader (if (string? prompt)
				   (lambda (in) (read-block (string-size prompt) in))
				   read-line)))
    (let* ((p (run-process cmd&args :input :pipe :output :pipe))
	   (r (reader (process-output p))))
      (values p (string-incomplete->complete r)))))

(provide "kahua/test/util")
