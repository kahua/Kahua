;; Kahua sandbox
;;
;; this module is based on banyan/sandbox.scm
;; written by Shiro Kawai (shiro@acm.org).
;;
;;  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
;;
;; $Id: sandbox.scm,v 1.2 2004/02/25 11:15:49 tahara Exp $

(define-module kahua.sandbox
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-14)
  (export make-sandbox-module
          export-module export-module-except
          disable-bindings)
  )

(select-module kahua.sandbox)

(define-syntax export-module
  (syntax-rules ()
    ((_ module) #f)
    ((_ module name . names)
     (begin
       (define name (with-module module name))
       (export name)
       (export-module module . names)))
    ))

(define-macro (export-module-except module . names)
  (define (fold-bindings module proc knil)
    (let* ((mod     (find-module module))
           (exports (module-exports mod)))
      (if (pair? exports)
        (fold (lambda (name knil)
                (if (memq name names) knil (cons (proc name) knil)))
              knil
              exports)
        (hash-table-fold (module-table mod)
                         (lambda (name val knil)
                           (if (memq name names)
                             knil
                             (cons (proc name) knil)))
                         knil)
        )))
  `(begin
     ,@(fold-bindings module
                      (lambda (symbol)
                        `(define ,symbol (with-module ,module ,symbol)))
                      '())))

(define-syntax disable-bindings
  (syntax-rules ()
    ((_) #f)
    ((_ name . names)
     (begin
       (define-macro (name . args)
         (errorf "~a can't be used within sandbox module"
                 (unwrap-syntax 'name)))
       (disable-bindings . names)))
    ))

(define (make-sandbox-module)
  (let ((m (make-module #f)))
    (eval
     '(begin
        (import kahua.sandbox)

        ;; this is a temporary setting for existing example applications.
        ;; includes non safe procedues.
        (use kahua.config)
        (use kahua.util)
        (use kahua.partcont)
        (use kahua.gsid)
        (use kahua.persistence)
        (use kahua.user)
        (use kahua.session)
        (use kahua.server)
        (use kahua.developer)
        (use kahua.elem)

        (export-module kahua.plugin load-plugin use-plugin)


        (disable-bindings open-input-file open-output-file
                          call-with-input-file call-with-output-file
                          with-input-from-file with-output-to-file
                          load transcript-on transcript-off
                          null-environment scheme-report-environment 
                          interaction-environment

                          import require

                          select-module
                          with-module define-module
                          define-in-module find-module)

        ;; override
        (define use use-plugin)
        
        )

     m)
    ;; Now, this resets module precedence list of m to null, voiding
    ;; all bindings but explicitly incorporated ones.
    ; (eval '(extend null) m)
    m))

(provide "kahua/sandbox")
