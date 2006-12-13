;; -*- mode: scheme; coding: utf-8 -*-
;;
;; kahua.template - Page Template Engine
;;
;;  Copyright (c) 2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software.
;;
;; $Id: xml-template.scm,v 1.3 2006/12/13 03:09:29 bizenn Exp $

(define-module kahua.xml-template
  (use srfi-1)
  (use srfi-13)
  (use text.parse)
  (use sxml.ssax)
  (use gauche.parameter)
  (use kahua.util)
  (use kahua.elem)
  (export kahua:make-xml-parser
	  <kahua:xml-template>
	  kahua:make-xml-template
	  kahua:get-xml-element-id
	  kahua:dump-xml-id-table
	  kahua:xml-template->sxml
	  ))

(select-module kahua.xml-template)

(define-condition-type <kahua-xml-template-error> <kahua-error> #f)
(define (kahua-xml-template-error fmt . args)
  (apply errorf <kahua-xml-template-error> fmt args))

;; FIXME!! It should use the DOCTYPE declaration itself.

(define-constant *doctype-table*
  `(("http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"       . "-//W3C//DTD XHTML 1.0 Strict//EN")
    ("http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" . "-//W3C//DTD XHTML 1.0 Transitional//EN")
    ("http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"     . "-//W3C//DTD XHTML 1.0 Frameset//EN")))

(define (public-identifier uri)
  (and-let* ((id (assoc uri *doctype-table*)))
    (cdr id)))

(define-class <kahua:xml-template> ()
  ((sxml :init-keyword :sxml)
   (parser :init-keyword :parser)
   (id-table :init-keyword :id-table)
   (path :init-keyword :path)))

(define (kahua:make-xml-template path . maybe-ns-alist)
  (let* ((ns-alist (get-optional maybe-ns-alist '((#f . "http://www.w3.org/1999/xhtml"))))
	 (id-table (make-hash-table 'eq?))
	 (parser (kahua:make-xml-parser ns-alist))
	 (sxml (call-with-input-file path (cut parser <> '() id-table))))
    (make <kahua:xml-template>
      :sxml sxml :parser parser :id-table id-table :path path)))

(define-method kahua:get-xml-element-id ((tmpl <kahua:xml-template>) elem)
  (hash-table-get (slot-ref tmpl 'id-table) elem #f))

(define-method kahua:dump-xml-id-table ((tmpl <kahua:xml-template>))
  (hash-table-map (slot-ref tmpl 'id-table) cons))

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
    (define (xml-template->sxml-internal node)
      (cond ((and-let* ((id (kahua:get-xml-element-id tmpl node))
			(p (assq id elem-alist)))
	       (cdr p))
	     => (lambda (node)
		  (cond ((procedure? node) (car (rev-nodes (exec '() node))))
			((pair? node)
			 (let1 e (car node)
			   (cond ((eq? e 'node-set) (cadr node))
				 ((symbol? e)       node)
				 (else              (car node)))))
			(else
			 (kahua-xml-template-error "invalid node: ~s" node)))))
	    ((list? node) (map xml-template->sxml-internal node))
	    (else node)))
    (list (xml-template->sxml-internal (slot-ref tmpl 'sxml)))))

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
			 ns-alist))
	(id-table (make-parameter #f)))
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
	  ;; register to ID table if specified.
	  (and (id-table) id (hash-table-put! (id-table) node id))
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
      (lambda (port seed . maybe-id-table)
	(parameterize ((id-table (get-optional maybe-id-table #f)))
	  (let1 result (reverse (base-parser port seed))
	    (cons '*TOP*
		  (if (null? ns-alist)
		      result
		      (cons (list '@@ (cons '*NAMESPACES*
					    (map (lambda (ns) (list (car ns) (cdr ns))) ns-alist)))
			    result)))))))
    ))

(provide "kahua/xml-template")
