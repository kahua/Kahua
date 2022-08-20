#!/usr/local/bin/gosh

(use rfc.http)

(define (help args)
  (print #`"Usage: ,(car args) host logfile"))

(define (print-status host uri)
  (receive (status header body)
      (http-get host uri)
    (format #`"~d ~a" status uri)))

(define (main args)
  (cond [(< (length+ args) 3) (help args)]
        [else
         (let [(host (cadr args))
               (logfile (caddr args))]
           (call-with-input-file logfile
             (lambda (in)
               (for-each
                (lambda (line)
                  (let ((words (string-split line " ")))
                    (print-status host (list-ref words 6))))
                (port->string-list in)))))]))
