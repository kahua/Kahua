;; Provides SXML tags implemented as functions
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: elem.scm,v 1.15 2006/03/01 16:45:28 cut-sea Exp $

;; This module implements tags of SXML as functions

(define-module kahua.elem
  (export >>=
	  >>
	  get
	  put
	  update
          rev-nodes
	  node-set
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
))

(select-module kahua.elem)
(use gauche.collection)

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
	(if (string? st)
	    (>> (text/ st) (node-set (cdr sts)))
	    (>> st (node-set (cdr sts)))))))

(define empty (lambda (s) s))

;; Special tags

(define (text/ . args)
  (update (cut append args <>)))

(define (a/cont/ . args)
  (update (cut cons `(a/cont ,@(exec '() (node-set args))) <>)))
  
(define (form/cont/ . args)
  (update (cut cons `(form/cont ,@(exec '() (node-set args))) <>)))

(define (frame/cont/ . args)
  (update (cut cons `(frame/cont ,@(exec '() (node-set args))) <>)))

(define (extra-header/ . args)
  (update (cut cons `(extra-header ,@(exec '() (node-set args))) <>)))

(define (map/ proc arg1 . args)
  (node-set (apply map proc arg1 args)))

(define (with-ie/ . args)
  (update (cut cons `(with-ie ,@(exec '() (node-set args))) <>)))

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
    (cond ((string? node) node)
	  ((or (eq? (car node) '@) (eq? (car node) '@@)) node)
	  (else (cons (car node) (rev-nodes (cdr node))))))
  (reverse (map rev node-set)))

(define (font/ . args) 
  (update (cut cons `(font ,@(exec '() (node-set args))) <>)))
(define (tt/ . args) 
  (update (cut cons `(tt ,@(exec '() (node-set args))) <>)))
(define (b/ . args)
  (update (cut cons `(b ,@(exec '() (node-set args))) <>)))
(define (i/ . args)
  (update (cut cons `(i ,@(exec '() (node-set args))) <>)))
(define (small/ . args)
  (update (cut cons `(small ,@(exec '() (node-set args))) <>)))
(define (em/ . args)
  (update (cut cons `(em ,@(exec '() (node-set args))) <>)))
(define (strong/ . args)
  (update (cut cons `(strong ,@(exec '() (node-set args))) <>)))
(define (dfn/ . args)
  (update (cut cons `(dfn ,@(exec '() (node-set args))) <>)))
(define (code/ . args)
  (update (cut cons `(code ,@(exec '() (node-set args))) <>)))
(define (samp/ . args)
  (update (cut cons `(samp ,@(exec '() (node-set args))) <>)))
(define (kbd/ . args)
  (update (cut cons `(kbd ,@(exec '() (node-set args))) <>)))
(define (var/ . args)
  (update (cut cons `(var ,@(exec '() (node-set args))) <>)))
(define (cite/ . args)
  (update (cut cons `(cite ,@(exec '() (node-set args))) <>)))
(define (abbr/ . args)
  (update (cut cons `(abbr ,@(exec '() (node-set args))) <>)))
(define (acronym/ . args)
  (update (cut cons `(acronym ,@(exec '() (node-set args))) <>)))
(define (sub/ . args)
  (update (cut cons `(sub ,@(exec '() (node-set args))) <>)))
(define (sup/ . args)
  (update (cut cons `(sup ,@(exec '() (node-set args))) <>)))
(define (span/ . args)
  (update (cut cons `(span ,@(exec '() (node-set args))) <>)))
(define (bdo/ . args)
  (update (cut cons `( ,@(exec '() (node-set args))) <>)))
(define (br/ . args)
  (update (cut cons `(br ,@(exec '() (node-set args))) <>)))
(define (body/ . args)
  (update (cut cons `(body ,@(exec '() (node-set args))) <>)))
(define (address/ . args)
  (update (cut cons `(address ,@(exec '() (node-set args))) <>)))
(define (div/ . args)
  (update (cut cons `(div ,@(exec '() (node-set args))) <>)))
(define (a/ . args)
  (update (cut cons `(a ,@(exec '() (node-set args))) <>)))
(define (area/ . args)
  (update (cut cons `(area ,@(exec '() (node-set args))) <>)))
(define (link/ . args)
  (update (cut cons `(link ,@(exec '() (node-set args))) <>)))
(define (img/ . args)
  (update (cut cons `(img ,@(exec '() (node-set args))) <>)))
(define (hr/ . args)
  (update (cut cons `(hr ,@(exec '() (node-set args))) <>)))
(define (p/ . args)
  (update (cut cons `(p ,@(exec '() (node-set args))) <>)))
(define (h1/ . args)
  (update (cut cons `(h1 ,@(exec '() (node-set args))) <>)))
(define (h2/ . args)
  (update (cut cons `(h2 ,@(exec '() (node-set args))) <>)))
(define (h3/ . args)
  (update (cut cons `(h3 ,@(exec '() (node-set args))) <>)))
(define (h4/ . args)
  (update (cut cons `(h4 ,@(exec '() (node-set args))) <>)))
(define (h5/ . args)
  (update (cut cons `(h5 ,@(exec '() (node-set args))) <>)))
(define (h6/ . args)
  (update (cut cons `(h6 ,@(exec '() (node-set args))) <>)))
(define (pre/ . args)
  (update (cut cons `(pre ,@(exec '() (node-set args))) <>)))
(define (q/ . args)
  (update (cut cons `(q ,@(exec '() (node-set args))) <>)))
(define (blockquote/ . args)
  (update (cut cons `(blockquote ,@(exec '() (node-set args))) <>)))
(define (ins/ . args)
  (update (cut cons `(ins ,@(exec '() (node-set args))) <>)))
(define (del/ . args)
  (update (cut cons `(del ,@(exec '() (node-set args))) <>)))
(define (dl/ . args)
  (update (cut cons `(dl ,@(exec '() (node-set args))) <>)))
(define (dt/ . args)
  (update (cut cons `(dt ,@(exec '() (node-set args))) <>)))
(define (dd/ . args)
  (update (cut cons `(dd ,@(exec '() (node-set args))) <>)))
(define (ol/ . args)
  (update (cut cons `(ol ,@(exec '() (node-set args))) <>)))
(define (ul/ . args)
  (update (cut cons `(ul ,@(exec '() (node-set args))) <>)))
(define (li/ . args)
  (update (cut cons `(li ,@(exec '() (node-set args))) <>)))
(define (form/ . args)
  (update (cut cons `(form ,@(exec '() (node-set args))) <>)))
(define (label/ . args)
  (update (cut cons `(label ,@(exec '() (node-set args))) <>)))
(define (input/ . args)
  (update (cut cons `(input ,@(exec '() (node-set args))) <>)))
(define (select/ . args)
  (update (cut cons `(select ,@(exec '() (node-set args))) <>)))
(define (optgroup/ . args)
  (update (cut cons `(optgroup ,@(exec '() (node-set args))) <>)))
(define (option/ . args)
  (update (cut cons `(option ,@(exec '() (node-set args))) <>)))
(define (textarea/ . args)
  (update (cut cons `(textarea ,@(exec '() (node-set args))) <>)))
(define (fieldset/ . args)
  (update (cut cons `(fieldset ,@(exec '() (node-set args))) <>)))
(define (legend/ . args)
  (update (cut cons `(legend ,@(exec '() (node-set args))) <>)))
(define (button/ . args)
  (update (cut cons `(button ,@(exec '() (node-set args))) <>)))
(define (table/ . args)
  (update (cut cons `(table ,@(exec '() (node-set args))) <>)))
(define (caption/ . args)
  (update (cut cons `(caption ,@(exec '() (node-set args))) <>)))
(define (thead/ . args)
  (update (cut cons `(thead ,@(exec '() (node-set args))) <>)))
(define (tbody/ . args)
  (update (cut cons `(tbody ,@(exec '() (node-set args))) <>)))
(define (tfoot/ . args)
  (update (cut cons `(tfoot ,@(exec '() (node-set args))) <>)))
(define (colgroup/ . args)
  (update (cut cons `(colgroup ,@(exec '() (node-set args))) <>)))
(define (col/ . args)
  (update (cut cons `(col ,@(exec '() (node-set args))) <>)))
(define (tr/ . args)
  (update (cut cons `(tr ,@(exec '() (node-set args))) <>)))
(define (th/ . args)
  (update (cut cons `(th ,@(exec '() (node-set args))) <>)))
(define (td/ . args)
  (update (cut cons `(td ,@(exec '() (node-set args))) <>)))
(define (head/ . args)
  (update (cut cons `(head ,@(exec '() (node-set args))) <>)))
(define (title/ . args)
  (update (cut cons `(title ,@(exec '() (node-set args))) <>)))
(define (base/ . args)
  (update (cut cons `(base ,@(exec '() (node-set args))) <>)))
(define (meta/ . args)
  (update (cut cons `(meta ,@(exec '() (node-set args))) <>)))
(define (style/ . args)
  (update (cut cons `(style ,@(exec '() (node-set args))) <>)))
(define (script/ . args)
  (update (cut cons `(script ,@(exec '() (node-set args))) <>)))
(define (noscript/ . args)
  (update (cut cons `(noscript ,@(exec '() (node-set args))) <>)))
(define (html/ . args)
  (update (cut cons `(html ,@(exec '() (node-set args))) <>)))
(define (pdf/ . args)
  (update (cut cons `(pdf ,@(exec '() (node-set args))) <>)))
(define (frameset/ . args)
  (update (cut cons `(frameset ,@(exec '() (node-set args))) <>)))
(define (frame/ . args)
  (update (cut cons `(frame ,@(exec '() (node-set args))) <>)))

;;--------------------------------------------------------------------------

(define (flatten ls)
  (define (iter acc ls)
    (if (null? ls) 
	(reverse acc)
	(let ((hd (car ls))
	      (tl  (cdr ls)))
	  (cond ((string? hd) (iter (cons hd acc) tl))
		((null? hd) (iter acc tl))
		((eq? 'node-set (car hd)) 
		 (iter (append (reverse (cdr hd)) acc) tl))
		(else (iter (cons hd acc) tl))))))
  (iter '() ls))

(define (node-list-to-node-set ls) (cons 'node-set (flatten ls)))
(define (node-set: . arg) `(node-set ,@(flatten arg)))
  
(define (font: . arg) `(font ,@(flatten arg)))
(define (tt: . arg) `(tt ,@(flatten arg)))
(define (b: . arg) `(b ,@(flatten arg)))
(define (i: . arg) `(i ,@(flatten arg)))
(define (small: . arg) `(small ,@(flatten arg)))
(define (em: . arg) `(em ,@(flatten arg)))
(define (strong: . arg) `(strong ,@(flatten arg)))
(define (dfn: . arg) `(dfn ,@(flatten arg)))
(define (code: . arg) `(code ,@(flatten arg)))
(define (samp: . arg) `(samp ,@(flatten arg)))
(define (kbd: . arg) `(kbd ,@(flatten arg)))
(define (var: . arg) `(var ,@(flatten arg)))
(define (cite: . arg) `(cite ,@(flatten arg)))
(define (abbr: . arg) `(abbr ,@(flatten arg)))
(define (acronym: . arg) `(acronym ,@(flatten arg)))
(define (sub: . arg) `(sub ,@(flatten arg)))
(define (sup: . arg) `(sup ,@(flatten arg)))
(define (span: . arg) `(span ,@(flatten arg)))
(define (bdo: . arg) `( ,@(flatten arg)))
(define (br: . arg) `(br ,@(flatten arg)))
(define (body: . arg) `(body ,@(flatten arg)))
(define (address: . arg) `(address ,@(flatten arg)))
(define (div: . arg) `(div ,@(flatten arg)))
(define (a: . arg) `(a ,@(flatten arg)))
(define (area: . arg) `(area ,@(flatten arg)))
(define (link: . arg) `(link ,@(flatten arg)))
(define (img: . arg) `(img ,@(flatten arg)))
(define (hr: . arg) `(hr ,@(flatten arg)))
(define (p: . arg) `(p ,@(flatten arg)))
(define (h1: . arg) `(h1 ,@(flatten arg)))
(define (h2: . arg) `(h2 ,@(flatten arg)))
(define (h3: . arg) `(h3 ,@(flatten arg)))
(define (h4: . arg) `(h4 ,@(flatten arg)))
(define (h5: . arg) `(h5 ,@(flatten arg)))
(define (h6: . arg) `(h6 ,@(flatten arg)))
(define (pre: . arg) `(pre ,@(flatten arg)))
(define (q: . arg) `(q ,@(flatten arg)))
(define (blockquote: . arg) `(blockquote ,@(flatten arg)))
(define (ins: . arg) `(ins ,@(flatten arg)))
(define (del: . arg) `(del ,@(flatten arg)))
(define (dl: . arg) `(dl ,@(flatten arg)))
(define (dt: . arg) `(dt ,@(flatten arg)))
(define (dd: . arg) `(dd ,@(flatten arg)))
(define (ol: . arg) `(ol ,@(flatten arg)))
(define (ul: . arg) `(ul ,@(flatten arg)))
(define (li: . arg) `(li ,@(flatten arg)))
(define (form: . arg) `(form ,@(flatten arg)))
(define (label: . arg) `(label ,@(flatten arg)))
(define (input: . arg) `(input ,@(flatten arg)))
(define (select: . arg) `(select ,@(flatten arg)))
(define (optgroup: . arg) `(optgroup ,@(flatten arg)))
(define (option: . arg) `(option ,@(flatten arg)))
(define (textarea: . arg) `(textarea ,@(flatten arg)))
(define (fieldset: . arg) `(fieldset ,@(flatten arg)))
(define (legend: . arg) `(legend ,@(flatten arg)))
(define (button: . arg) `(button ,@(flatten arg)))
(define (table: . arg) `(table ,@(flatten arg)))
(define (caption: . arg) `(caption ,@(flatten arg)))
(define (thead: . arg) `(thead ,@(flatten arg)))
(define (tbody: . arg) `(tbody ,@(flatten arg)))
(define (tfoot: . arg) `(tfoot ,@(flatten arg)))
(define (colgroup: . arg) `(colgroup ,@(flatten arg)))
(define (col: . arg) `(col ,@(flatten arg)))
(define (tr: . arg) `(tr ,@(flatten arg)))
(define (th: . arg) `(th ,@(flatten arg)))
(define (td: . arg) `(td ,@(flatten arg)))
(define (head: . arg) `(head ,@(flatten arg)))
(define (title: . arg) `(title ,@(flatten arg)))
(define (base: . arg) `(base ,@(flatten arg)))
(define (meta: . arg) `(meta ,@(flatten arg)))
(define (style: . arg) `(style ,@(flatten arg)))
(define (script: . arg) `(script ,@(flatten arg)))
(define (noscript: . arg) `(noscript ,@(flatten arg)))
(define (html: . arg) `(html ,@(flatten arg)))
(define (pdf: . arg) `(pdf ,@(flatten arg)))
(define (frameset: . arg) `(frameset ,@(flatten arg)))
(define (frame: . arg) `(frame ,@(flatten arg)))

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

(define (a/cont: . arg) `(a/cont ,@(flatten arg)))
(define (form/cont: . arg) `(form/cont ,@(flatten arg)))
(define (frame/cont: . arg) `(frame/cont ,@(flatten arg)))
(define (extra-header: . arg) `(extra-header ,@(flatten arg)))
(define (map: proc arg1 . args)
  (node-list-to-node-set (apply map proc arg1 args)))

(provide "kahua.elem")