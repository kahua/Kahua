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
          define-entry
          main-proc
          interp-html
	  interp-html-rec
          ))
(select-module kahua.html)

;; Patch to fix 0.7.2 bug
(with-module sxml.tools (export sxml:string->xml))
;; End patch

;;==========================================================
;; Access control
;;

(define (not-accessible? auxs context)
  (cond ((assq-ref auxs 'require-role)
         => (lambda (roles)
              (not (kahua-user-has-role? (kahua-current-user context) roles))))
        (else #f)))

;;==========================================================
;; Entry management
;;

;; Define-entry creates a "well-known" or "permanent" continuation ID.
;; That means the user can always visit that node by using URL like
;; 'kahua.cgi/type/entry-name', where 'type' is an application type
;; and 'entry-name' is the name of the permanent entry.
;; Note that it is the same format with the transient continuation ID;
;; continuation IDs are distinguished by the name that matches #/^\d+-/.
;;
;; The entry name can be overloaded.  When "called", the definition
;; that matches the input signature is selected and invoked.

;; ... to be implemented ...
;(define-class <kahua-entry> ()
;  ((handlers :init-value '())))


(define (add-entry! name proc)
  (session-cont-register proc (symbol->string name)))

(define-syntax define-entry
  (syntax-rules ()
    ((define-entry (name . args) body . rest)
     (define name
       (let ((closure (lambda args body . rest)))
         (add-entry! 'name closure)
         closure)))
    ))

;;==========================================================
;;  Default SXML tree interpreter - generates HTML
;;

;; interp-html :: Node -> Context -> Stree
;;    where Stree is a list of string segments passed to tree->string.
(define (interp-html nodes context)
  (interp-html-rec (car nodes) context (lambda (s _) s)))

;; internal loop 
(define (interp-html-rec node context cont)
  (let ((name (sxml:element-name node)))
    (if (not name)
      (cont (if (string? node) 
              (sxml:string->xml node)
              "")
            context)
      (let ((attrs (sxml:attr-list-u node))
            (auxs  (sxml:aux-list-u node))
            (contents (sxml:content node)))
        (if (not-accessible? auxs context)
          (cont "" context)
          (cond ((get-element-handler name)
		 => (cut <> attrs auxs contents context
			 (lambda (nds cntx)
			   (handle-element-contents nds cntx cont))))
                (else 
                 (default-element-handler name attrs contents context cont)))
          )))
    ))

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
    (interp-html-rec (car contents) context
                     (lambda (stree context)
                       (handle-element-contents (cdr contents) context
                                                (lambda (stree2 context)
                                                  (cont (cons stree stree2)
                                                        context)
                                                  ))))))

;;==========================================================
;; Element management
;;

;; define-element allows the application programmer to define
;; a special SXML node type (element).  When the SXML
;; interpreter sees the node, its element handler is invoked,
;; which can translate the node into SXML.
;;
;;  element-handler :: Attrs -> Auxs -> [Node] -> Context ->
;;                     ([Node] -> Context -> a) -> a
;;
;;    where 'a' depends on the interpreter --- element handler is
;;    agnostic about it.

(define-syntax define-element
  (syntax-rules ()
    ((define-element name args . body)
     (add-element! 'name (lambda args . body)))))

(define-values (add-element! get-element-handler)
  (let ((table (make-hash-table)))
    (values
     (lambda (tag handler)
       (hash-table-put! table tag handler))
     (lambda (tag)
       (hash-table-get table tag #f)))))

;;-----------------------------------------------------------
;; Pre-defined element handlers
;;

;;
;; a/cont element
;;
;; `(a/cont (@@ (cont ,closure)) contents)
;;

(define-element a/cont (attrs auxs contents context cont)
  (let* ((cont-closure (car (assq-ref auxs 'cont '(#f))))
         (id (or (session-cont-register cont-closure) "")))
    (cont
     `((a (@ (href ,(format "~a/~a/~a"
                            (kahua-bridge-name) (kahua-worker-type) id)))
          ,@contents))
     context)))

;;
;; form/cont
;; 
;; `(form/cont (@@ (cont ,closure)) contents)
;;

(define-element form/cont (attrs auxs contents context cont)
  (let* ((cont-closure (assq-ref auxs 'cont))
         (id (session-cont-register (car cont-closure))))
    (cont
     `((form (@ (method "POST") 
                (action ,(format "~a/~a/~a"
                                 (kahua-bridge-name)
                                 (kahua-worker-type)
                                 id)))
             ,@contents))
     context)))

(provide "kahua/html")




