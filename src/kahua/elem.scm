;; Provides SXML tags implemented as functions
;;
;;  Copyright (c) 2004-2008 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2004-2008 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; This module implements tags of SXML as functions

(define-module kahua.elem
  (use srfi-1)
  (use gauche.collection)
  (use util.match)
  (export unit
          >>=
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
	  html/ frameset/ frame/
	  @/
	  @@/
	  a/cont/
	  form/cont/
          frame/cont/
          extra-header/
	  map/
          with-ie/
	  &/
	  when/ unless/

	  applet/ param/ object/ embed/ noembed/

	  node-list->node-set
	  node-list-to-node-set
	  node-set:
	  font: tt: i: b: big: small: em: strong: dfn: code: samp: kbd: var:
	  cite: abbr: acronym: sub: sup: span: bdo: br: body: address: div:
	  a: area: link: img: hr: p: h1: h2: h3: h4: h5: h6:
	  pre: q: blockquote: ins: del: dl: dt: dd: ol: ul: li:
	  form: label: input: select: optgroup: option: textarea: fieldset:
	  legend: button: table: caption: thead: tfoot: tbody: colgroup:
          col: tr: th: td: head: title: base: meta: style: script: noscript:
	  html: frameset: frame:
	  @:
	  @@:
	  a/cont:
	  form/cont:
          frame/cont:
          extra-header:
	  map:
	  with-ie:
	  &:
	  when: unless:

	  applet: param: object: embed: noembed:

	  define-basic-element
	  define-elements

	  make-no-escape-text-element
	  no-escape?
	  ))

(select-module kahua.elem)

;;
;; unescape elements
;;

(define-class <no-escape> ()
  ((src :init-keyword :src)))

(define (make-no-escape-text-element . src)
  (let1 src (apply string-append (map x->string src))
    (make <no-escape> :src src)))

(define (no-escape? node)
  (is-a? node <no-escape>))

(define (entity->string val)
  (cond ((char? val) (format "#x~x" (char->ucs val))) ; character itself
	((integer? val) (format "#x~x" val))	      ; character code
	((or (string? val) (symbol? val)) val)	      ; character name
	(else (error "& node require string or symbol(character name), integer(character code) or character itself, but got " val))))

;; -------------------------------------------------------------------------
;; State thread : State -> State
;; This state thread is a spcial case of the state monad :
;;   State -> (a, State)

(define get identity)
(define (put s) (lambda (_) s))
(define (>>= st f)
  (lambda (s)
    (let1 s1 (st s)
      ((f s1) s1))))
(define (>> st1 st2)
  (>>= st1 (put st2)))
(define (update f)
  (>>= get (compose put f)))

(define (exec s0 st) (st s0))

(define (non-empty-list? xs) (and (list? xs) (not (null? xs)) xs))

(define (unit t)
  (receive (tg as) (car+cdr t)
    (apply (hash-table-get *element-table* tg) (attrs+children as))))

(define (attrs+children ss)
  (if (null? ss) '()
      (receive (a as) (car+cdr ss)
	(if (and (non-empty-list? a)
		 (or (eq? (car a) '@) (eq? (car a) '@@)))
	    (match a
	      (('@ . attrs) `(,|@/| ,@attrs))
              (('@@ . attrs) `(,|@/| ,@attrs)))
;;	    (cons a (attrs+children as))
	    (map unit+ ss)))))

(define (unit+ t)
  (if (string? t) (text/ t)
      (receive (tg as) (car+cdr t)
	(apply (hash-table-get *element-table* tg) (attrs+children as)))))
	  
(define (node-set sts)
  (if (null? sts)
      empty
      (let1 st (car sts)
	(cond ((procedure? st) (>> st (node-set (cdr sts))))
	      ((and (non-empty-list? st) (symbol? (car st))) (map/ unit sts))
	      (st (>> (text/ st) (node-set (cdr sts))))
	      (else (node-set (cdr sts)))))))

(define (node-set/ . args)
  (node-set args))

(define empty identity)

;; Special tags

(define (text/ . args)
  (update (cut fold cons <> args)))

(define (map/ proc arg1 . args)
  (node-set (apply map proc arg1 args)))

(define (&/ . args)
  (update (cut cons `(& ,@(exec '() (node-set (map entity->string args)))) <>)))

(define-syntax when/
  (syntax-rules ()
    ((_ expr . body)
     (cond (expr . body)
	   (else empty)))))

(define-syntax unless/
  (syntax-rules ()
    ((_ expr . body)
     (cond (expr empty)
	   (else . body)))))

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
    (cond ((pair? node)
	   (let1 name (car node)
	     (case name
	       ((@ @@) node)
	       (else (cons name (rev-nodes (cdr node)))))))
	  ((no-escape? node) node)
	  (else (x->string node))))
  (reverse (map rev node-set)))

(define-macro (define-basic-element name)
  (let ((name/ (string->symbol (string-append (symbol->string name) "/")))
	(name: (string->symbol (string-append (symbol->string name) ":"))))
    `(define-values (,name/ ,name:)
       (values (lambda args (update (cut cons (cons (quote ,name) (exec '() (node-set args))) <>)))
	       (lambda args (cons (quote ,name) (flatten args)))))))
(define-macro (define-elements . names)
  `(begin ,@(map (lambda (n) (list 'define-basic-element n)) names)))

(define *element-table* (make-hash-table 'eq?))
(define-macro (define-elements/reg . names)
  `(begin
     (define-elements ,@names)
     ,@(map (lambda (n)
	      (let1 n/ (string->symbol (string-append (symbol->string n) "/"))
		`(hash-table-put! ,*element-table* ',n ,n/)))
	    names)))

(define-elements/reg
  font tt b big i small em strong dfn code samp kbd var cite abbr acronym sub sup span bdo
  br body address div a area link img hr p h1 h2 h3 h4 h5 h6 pre q blockquote ins del
  dl dt dd ol ul li form label input select optgroup option textarea fieldset legend
  button table caption thead tbody tfoot colgroup col tr th td head title base meta
  style script noscript html frameset frame applet param object embed noembed
  pdf a/cont form/cont frame/cont extra-header with-ie)
;(define-basic-element no-escape)

;;--------------------------------------------------------------------------

(define (flatten ls)
  (reverse
   (fold (lambda (e r)
	   (cond ((null? e) r)
		 ((pair? e)
		  (case (car e)
		    ((node-set) (fold cons r (cdr e)))
		    (else (cons e r))))
		 ((no-escape? e) (cons e r))
		 (e (cons (x->string e) r))
		 (else r)))
	 '()
	 ls)))

(define (node-list->node-set ls) (cons 'node-set (flatten ls)))
(define node-list-to-node-set node-list->node-set) ; for backward compatibility
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
  `(& ,@(flatten (map entity->string arg))))

(define-syntax when:
  (syntax-rules ()
    ((_ expr . body)
     (cond (expr . body)
	   (else (node-set:))))))

(define-syntax unless:
  (syntax-rules ()
    ((_ expr . body)
     (cond (expr (node-set:))
	   (else . body)))))

(provide "kahua/elem")
