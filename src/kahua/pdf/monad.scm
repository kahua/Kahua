;; Monad

(define-module kahua.pdf.monad
  (use srfi-1)
  (export <monad> 
          monad-of
          let-zf*
          )
)

(select-module kahua.pdf.monad)

(define-class <monad> ()
  ((monad :init-keyword :monad :accessor monad-of)))

(define-macro (let-zf* binds body)
  (expand-let-zf binds body))
               
(define (expand-let-zf binds body)
  (if (null? binds)
      body
      (let ((bind (car binds))
            (rest (cdr binds)))
        (if (null? (cdr bind))
            `(>> ,(car bind) ,(expand-let-zf rest body))
            `(>>= ,(cadr bind) (lambda (,(car bind)) 
                                ,(expand-let-zf rest body)))))))


(provide "kahua/pdf/monad")