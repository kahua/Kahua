;; Provides SXML tags implemented as functions
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: elem.scm,v 1.4 2004/02/17 10:08:10 nobsun Exp $

;; This module implements tags of SXML as functions

(define-module kahua.elem
  (export node-list-to-node-set
	  node-set:
	  tt: i: b: big: small: em: strong: dfn: code: samp: kbd: var:
	  cite: abbr: acronym: sub: sup: span: bdo: br: body: address: div:
	  a: area: link: img: hr: p: h1: h2: h3: h4: h5: h6:
	  pre: q: blockquote: ins: del: dl: dt: dd: ol: ul: li:
	  form: label: input: select: optgroup: option: textarea: fieldset:
	  legend: button: table: caption: thead: tfoot: tbody: colgroup:
          col: tr: th: td: head: title: base: meta: style: script: noscript:
	  html:
	  @:
	  @@:
	  a/cont:
	  form/cont:
))

(select-module kahua.elem)

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

(define (node-list-to-node-set ls) (cons 'node-set ls))
(define (node-set: . arg) `(node-set ,@(flatten arg)))
  
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
     
(provide "kahua.elem")


