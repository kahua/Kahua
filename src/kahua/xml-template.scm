;; -*- mode: scheme; coding: utf-8 -*-
;;
;; kahua.template - Page Template Engine
;;
;;  Copyright (c) 2006-2007 Kahua Project, All rights reserved.
;;  See COPYING for terms and conditions of using this software.
;;

(define-module kahua.xml-template
  (use srfi-1)
  (use srfi-13)
  (use text.parse)
  (use sxml.ssax)
  (use sxml.tools)
  (use gauche.parameter)
  (use kahua.util)
  (use kahua.elem)
  (export kahua:make-xml-parser
	  <kahua:xml-template>
	  kahua:make-xml-template
	  kahua:xml-template->sxml
	  kahua:xml-template->node/
	  ))

(select-module kahua.xml-template)

(define-condition-type <kahua-xml-template-error> <kahua-error> #f)
(define (kahua-xml-template-error fmt . args)
  (apply errorf <kahua-xml-template-error> fmt args))

;; FIXME!! It should use the DOCTYPE declaration itself.

(define-constant *doctype-table*
  `(("http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"       . "-//W3C//DTD XHTML 1.0 Strict//EN")
    ("http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" . "-//W3C//DTD XHTML 1.0 Transitional//EN")
    ("http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"     . "-//W3C//DTD XHTML 1.0 Frameset//EN")
    ("http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"            . "-//W3C//DTD XHTML 1.1//EN")
    ))

(define (public-identifier uri)
  (and-let* ((id (assoc uri *doctype-table*)))
    (cdr id)))

(define-class <kahua:xml-template> ()
  ((sxml :init-keyword :sxml)
   (parser :init-keyword :parser)
   (path :init-keyword :path)))

(define (kahua:make-xml-template path . maybe-ns-alist)
  (let* ((ns-alist (get-optional maybe-ns-alist '((#f . "http://www.w3.org/1999/xhtml"))))
	 (parser (kahua:make-xml-parser ns-alist))
	 (sxml (call-with-input-file path (cut parser <> '()) :encoding 'UTF-8))) ; FIXME!!
    (make <kahua:xml-template>
      :sxml sxml :parser parser :path path)))

(define (keyword-list->alist klist)
  (define keyword->symbol (compose string->symbol keyword->string))
  (let loop ((klist klist)
	     (alist '()))
    (if (null? klist)
	(reverse! alist)
	(loop (cddr klist)
	      (acons (keyword->symbol (car klist)) (cadr klist) alist)))))

(define-method kahua:xml-template->sxml ((tmpl <kahua:xml-template>) . args)
  (let1 elem-alist (keyword-list->alist args)
    (define (xml-template->sxml-internal node accum)
      (cond ((and-let* ((id (sxml:attr node 'id))
			(p (assq (string->symbol id) elem-alist)))
	       (cdr p))
	     => (lambda (node)
		  (let1 node (cond ((procedure? node)
				    (rev-nodes (exec '() node)))
				   ((pair? node)
				    (let1 n (car node)
				      (cond ((eq? n 'node-set) (cdr node))
					    ((symbol? n) (list node))
					    (else        node))))
				   (else (kahua-xml-template-error "invalid node: ~s" node)))
		    (fold cons accum node))))
	    ((list? node) (cons (reverse (fold xml-template->sxml-internal '() node)) accum))
	    (else (cons node accum))))
    (list (reverse (fold xml-template->sxml-internal '() (slot-ref tmpl 'sxml))))))

(define-method kahua:xml-template->node/ ((tmpl <kahua:xml-template>) . args)
  (let1 sxml (apply kahua:xml-template->sxml tmpl args)
    (if (null? sxml)
	empty
	(update (cut cons (car (rev-nodes sxml)) <>)))))

;;
;; The parser take input port and seed(maybe a nil)
;; FIXME!!  This cannot handle namespace and DOCTYPE properly.
;;
(define (kahua:make-xml-parser ns-alist)
  (define (res-name->sxml res-name)
    (if (symbol? res-name)
	res-name
	(string->symbol #`",(car res-name):,(cdr res-name)")))
  (let ((namespaces (map (lambda (el)
			   (list* #f (car el)
				  (ssax:uri-string->symbol (cdr el))))
			 (or ns-alist '()))))
    (define (%new-level-seed elem-gi attributes namespaces expected-content seed)
      '())
    (define (%finish-element elem-gi attributes namespaces parent-seed seed)
      (receive (attrs id) (let loop ((attrs (attlist->alist attributes))
				     (accum '())
				     (id     #f))
			    (if (null? attrs)
				(values accum id)
				(let* ((attr (car attrs))
				       (name (res-name->sxml (car attr)))
				       (value (cdr attr))
				       (n+v (list name value)))
				  (if (eq? name 'id)
				      (loop (cdr attrs) (cons n+v accum) (string->symbol value))
				      (loop (cdr attrs) (cons n+v accum) #f)))))
	(let* ((seed (ssax:reverse-collect-str-drop-ws seed))
	       (node (cons (res-name->sxml elem-gi)
			   (if (null? attrs) seed (cons (cons '@ attrs) seed)))))
	  (cons node parent-seed))))
    (define (%char-data-handler string1 string2 seed)
      (if (string-null? string2)
	  (cons string1 seed)
	  (list* string2 string1 seed)))
    (define (%doctype port docname systemid internal-subset? seed)
      (when internal-subset?
	(ssax:warn port "Internal DTD subset is not currently handled ")
	(ssax:skip-internal-dtd port))
      (let1 publicid (public-identifier systemid)
	(values #f '() namespaces
		(if publicid
		    (cons (list '*DOCTYPE* docname publicid systemid) seed)
		    seed))))
    (define (%undecl-root elem-gi seed)
      (values #f '() namespaces seed))
    (define (%decl-root elem-gi seed)
      seed)
    (define (%default-pi-handler port pi-tag seed)
      (cons (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
	    seed))

    ;; main
    (let1 base-parser (ssax:make-parser DOCTYPE           %doctype
					DECL-ROOT         %decl-root
					UNDECL-ROOT       %undecl-root
					NEW-LEVEL-SEED    %new-level-seed
					FINISH-ELEMENT    %finish-element
					CHAR-DATA-HANDLER %char-data-handler
					PI                ((*DEFAULT* . %default-pi-handler)))
      (lambda (port seed)
	(let1 result (reverse (base-parser port seed))
	  (cond (ns-alist
		 (cons '*TOP*
		       (if (null? ns-alist)
			   result
			   (cons (list '@@ (cons '*NAMESPACES*
						 (map (lambda (ns) (list (car ns) (cdr ns))) ns-alist)))
				 result))))
		((and (pair? result) (pair? (car result))) (car result))
		(else result)))))))

(provide "kahua/xml-template")
