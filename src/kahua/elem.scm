;; Provides SXML tags implemented as functions
;;
;;  Copyright (c) 2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: elem.scm,v 1.1 2004/02/14 10:21:43 nobsun Exp $

;; This module implements tags of SXML as functions

(define-module kahua.elem
  (export tt: i: b: big: small: em: strong: dfn: code: samp: kbd: var:
	  cite: abbr: acronym: sub: sup: span: bdo: br: body: address: div:
	  a: area: link: img: hr: p: h1: h2: h3: h4: h5: h6:
	  pre: q: blockquote: ins: del: dl: dt: dd: ol: ul: li:
	  form: label: input: select: optgroup: option: textarea: fieldset:
	  legend: button: table: caption: thead: tfoot: tbody: colgroup:
          col: tr: th: td: head: title: base: meta: style: script: noscript:
	  html:
	  @:
	  @@:
))

(select-module kahua.elem)

(define (tt: . arg) `(tt ,@arg))
(define (b: . arg) `(b ,@arg))
(define (i: . arg) `(i ,@arg))
(define (small: . arg) `(small ,@arg))
(define (em: . arg) `(em ,@arg))
(define (strong: . arg) `(strong ,@arg))
(define (dfn: . arg) `(dfn ,@arg))
(define (code: . arg) `(code ,@arg))
(define (samp: . arg) `(samp ,@arg))
(define (kbd: . arg) `(kbd ,@arg))
(define (var: . arg) `(var ,@arg))
(define (cite: . arg) `(cite ,@arg))
(define (abbr: . arg) `(abbr ,@arg))
(define (acronym: . arg) `(acronym ,@arg))
(define (sub: . arg) `(sub ,@arg))
(define (sup: . arg) `(sup ,@arg))
(define (span: . arg) `(span ,@arg))
(define (bdo: . arg) `( ,@arg))
(define (br: . arg) `(br ,@arg))
(define (body: . arg) `(body ,@arg))
(define (address: . arg) `(address ,@arg))
(define (div: . arg) `(div ,@arg))
(define (a: . arg) `(a ,@arg))
(define (area: . arg) `(area ,@arg))
(define (link: . arg) `(link ,@arg))
(define (img: . arg) `(img ,@arg))
(define (hr: . arg) `(hr ,@arg))
(define (p: . arg) `(p ,@arg))
(define (h1: . arg) `(h1 ,@arg))
(define (h2: . arg) `(h2 ,@arg))
(define (h3: . arg) `(h3 ,@arg))
(define (h4: . arg) `(h4 ,@arg))
(define (h5: . arg) `(h5 ,@arg))
(define (h6: . arg) `(h6 ,@arg))
(define (pre: . arg) `(pre ,@arg))
(define (q: . arg) `(q ,@arg))
(define (blockquote: . arg) `(blockquote ,@arg))
(define (ins: . arg) `(ins ,@arg))
(define (del: . arg) `(del ,@arg))
(define (dl: . arg) `(dl ,@arg))
(define (dt: . arg) `(dt ,@arg))
(define (dd: . arg) `(dd ,@arg))
(define (ol: . arg) `(ol ,@arg))
(define (ul: . arg) `(ul ,@arg))
(define (li: . arg) `(li ,@arg))
(define (form: . arg) `(form ,@arg))
(define (label: . arg) `(label ,@arg))
(define (input: . arg) `(input ,@arg))
(define (select: . arg) `(select ,@arg))
(define (optgroup: . arg) `(optgroup ,@arg))
(define (option: . arg) `(option ,@arg))
(define (textarea: . arg) `(textarea ,@arg))
(define (fieldset: . arg) `(fieldset ,@arg))
(define (legend: . arg) `(legend ,@arg))
(define (button: . arg) `(button ,@arg))
(define (table: . arg) `(table ,@arg))
(define (caption: . arg) `(caption ,@arg))
(define (thead: . arg) `(thead ,@arg))
(define (tbody: . arg) `(tbody ,@arg))
(define (tfoot: . arg) `(tfoot ,@arg))
(define (colgroup: . arg) `(colgroup ,@arg))
(define (col: . arg) `(col ,@arg))
(define (tr: . arg) `(tr ,@arg))
(define (th: . arg) `(th ,@arg))
(define (td: . arg) `(td ,@arg))
(define (head: . arg) `(head ,@arg))
(define (title: . arg) `(title ,@arg))
(define (base: . arg) `(base ,@arg))
(define (meta: . arg) `(meta ,@arg))
(define (style: . arg) `(style ,@arg))
(define (script: . arg) `(script ,@arg))
(define (noscript: . arg) `(noscript ,@arg))
(define (html: . arg) `(html ,@arg))

(define-syntax @:
  (syntax-rules ()
    ((_ a) `(@ a))
    ((_ a b) `(@ a b))
    ((_ a b ...) `(@ a b ...))))
     
(define-syntax @@:
  (syntax-rules ()
    ((_ a) `(@@ a))
    ((_ a b) `(@@ a b))
    ((_ a b ...) `(@@ a b ...))))
     
(provide "kahua.elem")


