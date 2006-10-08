;; Provides SXML tags implemented as functions
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: elem.scm,v 1.26 2006/10/08 01:36:16 bizenn Exp $

;; This module implements tags of SXML as functions

(define-module kahua.elem
  (use gauche.collection)
  (export >>=
	  >>
	  get
	  put
	  update
          rev-nodes
	  node-set
          node-set/
	  empty
	  exec
	  text/
	  font/ tt/ i/ b/ big/ small/ em/ strong/ dfn/ code/ samp/ kbd/ var/
	  cite/ abbr/ acronym/ sub/ sup/ span/ bdo/ br/ body/ address/ div/
	  a/ area/ link/ img/ hr/ p/ h1/ h2/ h3/ h4/ h5/ h6/
	  pre/ q/ blockquote/ ins/ del/ dl/ dt/ dd/ ol/ ul/ li/
	  form/ label/ input/ select/ optgroup/ option/ textarea/ fieldset/
	  legend/ button/ table/ caption/ thead/ tfoot/ tbody/ colgroup/
          col/ tr/ th/ td/ head/ title/ base/ meta/ style/ script/ noscript/
	  html/ pdf/ frameset/ frame/
	  @/
	  @@/
	  a/cont/
	  form/cont/
          frame/cont/
          extra-header/
	  map/
          with-ie/
	  &/

	  applet/ param/ object/ embed/ noembed/

	  node-list-to-node-set
	  node-set:
	  font: tt: i: b: big: small: em: strong: dfn: code: samp: kbd: var:
	  cite: abbr: acronym: sub: sup: span: bdo: br: body: address: div:
	  a: area: link: img: hr: p: h1: h2: h3: h4: h5: h6:
	  pre: q: blockquote: ins: del: dl: dt: dd: ol: ul: li:
	  form: label: input: select: optgroup: option: textarea: fieldset:
	  legend: button: table: caption: thead: tfoot: tbody: colgroup:
          col: tr: th: td: head: title: base: meta: style: script: noscript:
	  html: pdf: frameset: frame:
	  @:
	  @@:
	  a/cont:
	  form/cont:
          frame/cont:
          extra-header:
	  map:
	  with-ie:
	  &:

	  applet: param: object: embed: noembed:

          obj->string
          html:element?
	  make-no-escape-text-element
	  no-escape?
	  ))

(select-module kahua.elem)

(define-method obj->string ((self <integer>))
  (number->string self))

(define-method obj->string ((self <symbol>))
  (symbol->string self))

(define-method obj->string ((self <string>))
  self)

(define (html:element? obj)
  (not (null? (compute-applicable-methods obj->string (list obj)))))

;;
;; unescape elements
;;

(define-class <no-escape> ()
  ((src :init-keyword :src)))

(define (make-no-escape-text-element . src)
  (let1 src (apply string-append (map obj->string src))
    (make <no-escape> :src src)))

(define (no-escape? node)
  (is-a? node <no-escape>))

;; -------------------------------------------------------------------------
;; State thread : State -> State
;; This state thread is a spcial case of the state monad :
;;   State -> (a, State)

(define (>>= st f)
  (lambda (s)
    (let1 s1 (st s)
      ((f s1) s1))))
(define (>> st1 st2)
  (>>= st1 (lambda (_) st2)))
(define get (lambda (s) s))
(define (put s) (lambda (_) s))
(define (update f) 
  (>>= get (lambda (s) (put (f s)))))

(define (exec s0 st) (st s0))

(define (node-set sts)
  (if (null? sts)
      identity
      (let1 st (car sts)
	(if (or (html:element? st)
		(no-escape? st))
	    (>> (text/ st) (node-set (cdr sts)))
	    (>> st (node-set (cdr sts)))))))

(define (node-set/ . args)
  (node-set args))

(define empty (lambda (s) s))

;; Special tags

(define (text/ . args)
  (update (cut append args <>)))

(define (map/ proc arg1 . args)
  (node-set (apply map proc arg1 args)))

(define (&/ . args)
  (define (val->string val)
    (cond ((number? val) (format "#~x" val))
	  (else val)))
  (update (cut cons `(& ,@(exec '() (node-set (map val->string args)))) <>)))

;; SXML tag

(define-syntax @/
  (syntax-rules ()
    ((_ (name val1) ...)
     (update (cut cons `(@ (name ,val1) ...) <>)))
    ((_ (name val1 val2) ...)
     (update (cut cons `(@ (name ,val1 ,val2) ...) <>)))
    ((_ (name val1 val2 ...) ...)
     (update (cut cons `(@ (name ,val1 ,val2 ...) ...) <>)))))

(define-syntax @@/
  (syntax-rules ()
    ((_ (name val1) ...)
     (update (cut append <> `((@@ (name ,val1) ...)))))
    ((_ (name val1 val2) ...)
     (update (cut append <> `((@@ (name ,val1 ,val2) ...)))))
    ((_ (name val1 val2 ...) ...)
     (update (cut append <> `((@@ (name ,val1 ,val2 ...) ...)))))))

(define (rev-nodes node-set)
  (define (rev node)
    (cond ((html:element? node) (obj->string node))
	  ((no-escape? node) node)
	  ((or (eq? (car node) '@) (eq? (car node) '@@)) node)
	  (else (cons (car node) (rev-nodes (cdr node))))))
  (reverse! (map rev node-set)))

(define-macro (define-basic-element name)
  `(define-values (,(string->symbol (string-append (symbol->string name) "/"))
		   ,(string->symbol (string-append (symbol->string name) ":")))
     (values (lambda args (update (cut cons (cons (quote ,name) (exec '() (node-set args))) <>)))
	     (lambda args (cons (quote ,name) (flatten args))))))
(define-macro (define-elements . names)
  `(begin ,@(map (lambda (n) (list 'define-basic-element n)) names)))

(define-elements
  font tt b i small em strong dfn code samp kbd var cite abbr acronym sub span bdo
  br body address div a area link img hr p h1 h2 h3 h4 h5 h6 pre q blockquote ins del
  dl dt dd ol ul li form label input select optgroup option textarea fieldset legend
  button table caption thead tbody tfoot colgroup col tr th td head title base meta
  style script noscript html frameset frame applet param object embed noembed
  pdf a/cont form/cont frame/cont extra-header with-ie)
;(define-basic-element no-escape)

;;--------------------------------------------------------------------------

(define (flatten ls)
  (define (iter acc ls)
    (if (null? ls) 
	(reverse acc)
	(let ((hd (car ls))
	      (tl  (cdr ls)))
	  (cond ((html:element? hd) (iter (cons (obj->string hd) acc) tl))
		((null? hd) (iter acc tl))
		((eq? 'node-set (car hd)) 
		 (iter (append (reverse (cdr hd)) acc) tl))
		(else (iter (cons hd acc) tl))))))
  (iter '() ls))

(define (node-list-to-node-set ls) (cons 'node-set (flatten ls)))
(define (node-set: . arg) `(node-set ,@(flatten arg)))

(define-syntax @:
  (syntax-rules ()
    ((_ (name val1) ...)
     `(@ (name ,val1) ...))
    ((_ (name val1 val2) ...)
     `(@ (name ,val1 ,val2) ...))
    ((_ (name val1 val2 ...) ...)
     `(@ (name ,val1 ,val2 ...) ...))))

(define-syntax @@:
  (syntax-rules ()
    ((_ (name val1) ...)
     `(@@ (name ,val1) ...))
    ((_ (name val1 val2) ...)
     `(@@ (name ,val1 ,val2) ...))
    ((_ (name val1 val2 ...) ...)
     `(@@ (name ,val1 ,val2 ...) ...))))

(define (map: proc arg1 . args)
  (node-list-to-node-set (apply map proc arg1 args)))
(define (&: . arg)
  (define (val->string val)
    (cond ((number? val) (format "#~x" val))
	  (else val)))
  `(& ,@(flatten (map val->string arg))))

(provide "kahua/elem")
