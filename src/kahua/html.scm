;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
(define-module kahua.html
  (use gauche.parameter)
  (use srfi-2)
  (use srfi-11)
  (use sxml.tools)
  (use text.tree)
  (use util.list)
  (use file.util)
  (use kahua.session)
  (use kahua.server)
  (use kahua.user)

  (export initialize-main-proc
          not-accessible?
	  add-element!
          define-element
	  main-proc
	  interp-html))
(select-module kahua.html)

;; Patch to fix 0.7.2 bug
(with-module sxml.tools (export sxml:string->xml))
;; End patch

;;
;; preliminary staff
;;

; (define main-proc (make-parameter (lambda (context)
; 				    (error "Not initialized!"))))
; (define (initialize-main-proc proc)
;   (main-proc proc))

(define (not-accessible? auxs context)
  (cond ((assq-ref auxs 'require-role)
         => (lambda (roles)
              (not (kahua-user-has-role? (kahua-current-user context) roles))))
        (else #f)))

;;
;; element management
;;

(define-values (add-element! get-element-handler)
  (let ((table (make-hash-table)))
    (values
     (lambda (tag handler)
       (hash-table-put! table tag handler))
     (lambda (tag)
       (hash-table-get table tag #f)))))

(define-syntax define-element
  (syntax-rules ()
    ((define-element name args . body)
     (add-element! 'name (lambda args . body)))))

(define (default-element-handler tag attrs content context cont)
  (handle-element-contents content context
                           (lambda (stree context)
                             (cont `("<" ,tag ,(map sxml:attr->xml attrs) ">"
                                     ,@stree
                                     "</" ,tag "\n>")
                                   context))))

(define (handle-element-contents contents context cont)
  (if (null? contents)
    (cont '() context)
    (interp-html (car contents) context
                 (lambda (stree context)
                   (handle-element-contents (cdr contents) context
                                            (lambda (stree2 context)
                                              (cont (cons stree stree2)
                                                    context)
                                              )))
                 )))
                                     
;; Interprete node
;;  interp-html :: Node -> Context -> (Stree -> Context -> Stree) -> Stree

(define (interp-html node context cont)
  (let ((name (sxml:element-name node)))
    (if (not name)
      (cont (if (string? node) (sxml:string->xml node) "")
            context)
      (let ((attrs (sxml:attr-list-u node))
            (auxs  (sxml:aux-list-u node))
            (contents (sxml:content node)))
        (if (not-accessible? auxs context)
          (cont "" context)
          (cond ((get-element-handler name)
                 => (cut <> attrs auxs contents context cont))
                (else 
                 (default-element-handler name attrs contents context cont)))
          )))
    ))

;; Element handlers
;;
;;  element-handler :: Attrs -> Auxs -> [Node] -> Context ->
;;                     (Node -> Context -> Stree) -> Stree

;;
;; a/cont element
;;
;; `(a/cont (@@ (cont ,closure)) contents)
;;

(define-element a/cont (attrs auxs contents context cont)
  (let* ((cont-closure (assq-ref auxs 'cont))
	 (id (session-cont-register (car cont-closure))))
    (interp-html 
     `(a (@ (href ,(format "~a?x-kahua-cgsid=~a" (kahua-bridge-name) id)))
         ,@contents)
     context
     cont)))
;;
;; form/cont
;; 
;; `(form/cont (@@ (cont ,closure)) contents)
;;

(define-element form/cont (attrs auxs contents context cont)
  (let* ((cont-closure (assq-ref auxs 'cont))
	 (id (session-cont-register (car cont-closure))))
    (interp-html
     `(form (@ (method "POST") 
               (action ,(kahua-bridge-name)))
            (input (@ (type "hidden")
                      (name "x-kahua-cgsid")
                      (value ,(x->string id))))
            ,@contents)
     context
     cont)))

(provide "kahua/html")

