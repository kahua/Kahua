;; Kahua sandbox
;;
;; this module is based on banyan/sandbox.scm
;; written by Shiro Kawai (shiro@acm.org).
;;
;;  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
;;
;; $Id: sandbox.scm,v 1.4 2004/03/02 06:47:03 tahara Exp $

(define-module kahua.sandbox
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-14)
  (use kahua.plugin)
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

        ;; for class redefinition.
        ;; require is done at compile time but also clear
        ;; to need this module.
        ;; TODO: but why does autoload in sandbox module??
        (require "gauche/redefutil")

        (export-module kahua.plugin load-plugin use-plugin)


        (disable-bindings open-input-file open-output-file
                          call-with-input-file call-with-output-file
                          with-input-from-file with-output-to-file
                          load transcript-on transcript-off
                          null-environment scheme-report-environment 
                          interaction-environment

                          exit sys-exit sys-abort

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
