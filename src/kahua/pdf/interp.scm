;; interp-pdf :: SXML -> (State -> (#f, State))
(define-module kahua.pdf.interp
  (use srfi-1)
  (use srfi-11)
  (use util.list)
  (extend kahua.pdf.state)
  (export make-line
          flag-of-line
          tags-of-line
          string-of-line
          flag-on-line
          pop-tag-line
          push-tag-line
          add-string-line
          make-box
          indent-of-box
          lines-of-box
          add-line-box
          make-state
          numbering-of-state
          indent-of-state
          top-flag-of-state
          tags-of-state
          boxes-of-state
          numbering-inc-state
          numbering-clear-state
          numbering-init-state
          indent-inc-state
          set-indent-state
          top-flag-on-state
          top-flag-off-state
          push-tag-state
          pop-tag-state
          clear-boxes-state
          cons-box-state
          append-boxes-state
          set-boxes-state
          reverse-lines
          make-pdf-interp
          text
          inline-tag
          simple-block-tag
          lookup
          html-environment
          interp-html-pdf
	  ))

(select-module kahua.pdf.interp)

;; <pdf-line>
;;
(define (make-line flag tags string)
  (list flag tags string))
(define flag-of-line car)
(define tags-of-line cadr)
(define string-of-line caddr)

(define (flag-on-line line)
  (make-line
   #t
   (tags-of-line line)
   (string-of-line line)))
(define (pop-tag-line line) 
  (make-line
   (flag-of-line line)
   (cdr (tags-of-line line))
   (string-of-line line)))
(define (push-tag-line tag line) 
  (make-line
   (flag-of-line line)
   (cons tag (tags-of-line line))
   (string-of-line line)))
(define (add-string-line line str)
  (make-line
   (flag-of-line line)
   (tags-of-line line)
   (cons (string-of-line line) str)))

;; box
;;
(define (make-box numbering indent lines)
  (list numbering indent lines))
(define numbering-of-box car)
(define indent-of-box cadr)
(define lines-of-box caddr)

(define (add-line-box line box)
  (make-box (numbering-of-box box)
            (indent-of-box box)
            (cons line (lines-of-box box))))

;; state
;;
(define (make-state numbering indent top-flag tags boxes)
  (list numbering indent top-flag tags boxes))
(define numbering-of-state car)
(define indent-of-state cadr)
(define top-flag-of-state caddr)
(define tags-of-state cadddr)
(define (boxes-of-state state) (car (cddddr state)))

;; numbering update
(define (numbering-inc-state state)
  (make-state
    (+ (numbering-of-state state) 1) 
    (indent-of-state state)
    (top-flag-of-state state)
    (tags-of-state state)
    (boxes-of-state state)
    ))
(define (numbering-clear-state state)
  (make-state
    0
    (indent-of-state state)
    (top-flag-of-state state)
    (tags-of-state state)
    (boxes-of-state state)
    ))

(define (numbering-init-state state)
  (make-state
    1
    (indent-of-state state)
    (top-flag-of-state state)
    (tags-of-state state)
    (boxes-of-state state)
    ))

(define (indent-inc-state state)
  (make-state
    (numbering-of-state state)
    (+ (indent-of-state state) 1)
    (top-flag-of-state state)
    (tags-of-state state)
    (boxes-of-state state)
    ))

(define (set-indent-state indent state)
  (make-state
    (numbering-of-state state)
    indent
    (top-flag-of-state state)
    (tags-of-state state)
    (boxes-of-state state)
    ))

;; top flag update

(define (top-flag-on-state state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state)
    #t
    (tags-of-state state)
    (boxes-of-state state)
    ))
               
(define (top-flag-off-state state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state)
    #f
    (tags-of-state state)
    (boxes-of-state state)
    ))

;; tags update

(define (push-tag-state tag state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state)
    (top-flag-of-state state)
    (cons tag (tags-of-state state))
    (boxes-of-state state)
    ))

(define (pop-tag-state state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state)
    (top-flag-of-state state)
    (cdr (tags-of-state state))
    (boxes-of-state state)
    ))

;; boxes update

(define (clear-boxes-state state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state)
    (top-flag-of-state state)
    (tags-of-state state)
    '()
    ))

(define (cons-box-state box state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state)
    (top-flag-of-state state)
    (tags-of-state state)
    (cons box (boxes-of-state state))))

(define (append-boxes-state boxes state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state) 
    (top-flag-of-state state)
    (tags-of-state state)
    (append boxes (boxes-of-state state))))

(define (set-boxes-state boxes state)
  (make-state
    (numbering-of-state state)
    (indent-of-state state) 
    (top-flag-of-state state)
    (tags-of-state state)
    boxes))

;;
;; interp-pdf :: SXML -> Env -> M [PDFBlock]
;;

(define (reverse-lines box)
  (make-box (numbering-of-box box)
            (indent-of-box box)
            (reverse (lines-of-box box))))

(define (make-pdf-interp env)
  (define (interp node)
    (if (string? node)
      (text node)
      (let ((tag (car node))
	    (rest (cdr node)))
	(receive 
            (attrs contents)
            (span (lambda (n) 
                    (and (list? n)
                         (or (eq? (car n) '@)
                             (eq? (car n) '@@))))
                  rest)
          (let* ((tagattrs (cons tag attrs))
                 (f (lookup tag env)))
            (f tagattrs (map/state>> interp contents)))
          ))))
  interp)

;; Function :: Taginfo -> M [PDFBlock] -> M [PDFBlock]
;; M [PDFBlock] = State -> ([PDFBlock], State)

(define (text str)
  (>>= get/state
       (lambda (s)
	 (let ((num (numbering-of-state s))
               (ind (indent-of-state s))
               (top (top-flag-of-state s))
               (tgs (tags-of-state s))
               (bxs (boxes-of-state s)))
           (cond (top
                  (let* ((nln (make-line #t tgs str))
                         (nbx (make-box num ind (list nln))))
                    (>> (update/state (cut top-flag-off-state <>))
                        (update/state (cut cons-box-state nbx <>)))))
                 ((null? bxs)
                  (let* ((nln (make-line #t tgs str))
                         (nbx (make-box num ind (list nln))))
                    (update/state (cut cons-box-state nbx <>))))
                 (else
                  (let ((cbx (car bxs)))
                    (let* ((nln (make-line #f tgs str))
                           (nbx (add-line-box nln cbx))
                           (nbxs (cons nbx (cdr bxs))))
                      (update/state (cut set-boxes-state nbxs <>))))))))))

(define (inline-tag tagattrs args)
  (>>= get/state
       (lambda (s)
	 (let ((num (numbering-of-state s))
               (ind (indent-of-state s))
               (top (top-flag-of-state s))
               (tgs (tags-of-state s))
               (bxs (boxes-of-state s))
               (tag (car tagattrs)))
             (if top
                 (let* ((nbxs (boxes-of-state
                               (exec/state
                                (clear-boxes-state
                                 (push-tag-state tagattrs s))
                                args)))
                        (bx (car nbxs)) ;; nbxs should be singleton list
                        (ls (lines-of-box bx)))
                   (let1 nbx (make-box num ind ls)
                         (>> (update/state (cut cons-box-state nbx <>))
                             (update/state (cut top-flag-off-state <>)))))
                 (let* ((nbxs (boxes-of-state
                               (exec/state
                                (top-flag-off-state
                                 (push-tag-state tagattrs s))
                                args))))
                   (update/state (cut set-boxes-state nbxs <>))))))))

(define (simple-block-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (clear-boxes-state
                       (push-tag-state tagattrs s))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (blockquote-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (indent-inc-state
                       (clear-boxes-state
                        (push-tag-state tagattrs s)))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (pre-tag tagattrs args) 
  (>>= get/state
       (lambda (s)
         (let* ((nbxs (boxes-of-state 
                       (exec/state
                        (indent-inc-state
                         (clear-boxes-state
                          (push-tag-state tagattrs s)))
                        args)))
                (nbx (car nbxs))
                (nls (map flag-on-line (lines-of-box nbx)))
                (nbxs-bis (list (make-box (numbering-of-state s)
                                          (indent-of-box nbx) 
                                          nls))))
           (update/state (cut append-boxes-state nbxs-bis <>))))))

(define (br-tag tagattrs args)
  (update/state (cut top-flag-on-state <>)))

(define (ul-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (numbering-clear-state
                       (indent-inc-state
                        (clear-boxes-state
                         (push-tag-state tagattrs s))))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (ol-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (numbering-init-state
                       (indent-inc-state
                        (clear-boxes-state
                         (push-tag-state tagattrs s))))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (li-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let ((num (numbering-of-state s))
               (ind (indent-of-state s))
               (tgs (tags-of-state s))
               )
           (if (= num 0)
               (let* ((nbxs (boxes-of-state
                             (exec/state
                              (clear-boxes-state
                               (push-tag-state tagattrs s))   
                              (>> (text "¡¦") args)))))
                 (update/state (cut append-boxes-state nbxs <>)))
               (let* ((nbxs (boxes-of-state
                             (exec/state
                              (clear-boxes-state
                               (push-tag-state tagattrs s))
                              (>> (text (string-append (x->string num) "."))
                                  args)))))
                 (>> (update/state (cut append-boxes-state nbxs <>))
                     (update/state (cut numbering-inc-state <>)))))))))

(define (dl-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (numbering-clear-state
                       (indent-inc-state
                        (clear-boxes-state
                         (push-tag-state tagattrs s))))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (dt-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (numbering-clear-state
                       (clear-boxes-state
                        (push-tag-state tagattrs s)))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (dd-tag tagattrs args)
  (>>= get/state
       (lambda (s)
         (let1 nbxs (boxes-of-state 
                     (exec/state
                      (numbering-clear-state
                       (indent-inc-state
                        (clear-boxes-state
                         (push-tag-state tagattrs s))))
                      args))
           (update/state (cut append-boxes-state nbxs <>))))))

(define (ignore tagattr args) (unit/state #f))
		 
(define (lookup tag env)
  (assoc-ref env tag))

(define html-environment
  `((tt . ,inline-tag)
    (i . ,inline-tag)
    (b . ,inline-tag)
    (big . ,inline-tag)
    (small . ,inline-tag)
    (em . ,inline-tag)
    (strong . ,inline-tag)
    (dfn . ,inline-tag)
    (code . ,inline-tag)
    (samp . ,inline-tag)
    (kbd . ,inline-tag)
    (var . ,inline-tag)
    (cite . ,inline-tag)
    (abbr . ,inline-tag)
    (acronym . ,inline-tag)
    (sub . ,inline-tag)
    (sup . ,inline-tag)
    (span . ,inline-tag)
    (bdo . ,ignore)
    (br . ,br-tag)
    (body . ,simple-block-tag)
    (address . ,inline-tag)
    (div . ,simple-block-tag)
    (a . ,inline-tag)
    (area . ,ignore)
    (link . ,ignore)
    (img . ,ignore)
    (hr . ,ignore)
    (p . ,simple-block-tag)
    (h1 . ,simple-block-tag)
    (h2 . ,simple-block-tag)
    (h3 . ,simple-block-tag)
    (h4 . ,simple-block-tag)
    (h5 . ,simple-block-tag)
    (h6 . ,simple-block-tag)
    (pre . ,pre-tag)
    (q . ,inline-tag)
    (blockquote . ,blockquote-tag)
    (ins . ,ignore)
    (del . ,ignore)
    (dl . ,dl-tag)
    (dt . ,dt-tag)
    (dd . ,dd-tag)
    (ol . ,ol-tag)
    (ul . ,ul-tag)
    (li . ,li-tag)
    (form . ,simple-block-tag)
    (label . ,ignore)
    (input . ,ignore)
    (select . ,ignore)
    (optgroup . ,ignore)
    (option . ,ignore)
    (textarea . ,ignore)
    (fieldset . ,ignore)
    (legend . ,ignore)
    (button . ,ignore)
    (table . ,ignore)
    (caption . ,ignore)
    (thead . ,ignore)
    (tfoot . ,ignore)
    (tbody . ,ignore)
    (colgroup . ,ignore)
    (col . ,ignore)
    (tr . ,ignore)
    (th . ,ignore)
    (td . ,ignore)
    (head . ,ignore)
    (title . ,ignore)
    (base . ,ignore)
    (meta . ,ignore)
    (style . ,ignore)
    (script . ,ignore)
    (noscript . ,ignore)
    (html . ,simple-block-tag)
    (pdf . ,simple-block-tag)
    (extra-header . ,ignore)
))

(define interp-html-pdf (make-pdf-interp html-environment))

(provide "kahua/pdf/interp")
