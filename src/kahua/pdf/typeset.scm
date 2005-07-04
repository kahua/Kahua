;; -*- coding: euc-jp -*-
(define-module kahua.pdf.typeset
  (extend kahua.pdf.main)
  ;(extend kahua.pdf.interp)
  (use gauche.parameter)
  (use gauche.uvector)
  (use srfi-1)
  (use srfi-13)
  (export with-docdata-to-file with-docdata-to-port)
  )
(select-module kahua.pdf.typeset)

(define *typeset-top-margin* 72)
(define *typeset-left-margin* 72)
(define *typeset-indent-width* 24)
(define *typeset-line-space* 10)
(define *typeset-heading-space* 10)

(define *typeset-height* (make-parameter (- *default-height* (* *typeset-top-margin* 2))))
(define *typeset-width* (make-parameter (- *default-width* (* *typeset-left-margin* 2))))

(define *typeset-font-times* (make-parameter #f))
(define *typeset-font-helvetica* (make-parameter #f))
(define *typeset-font-courier* (make-parameter #f))
(define *typeset-font-mincho* (make-parameter #f))
(define *typeset-font-gothic* (make-parameter #f))

(define *typeset-boxwidth* (make-parameter #f))
(define *typeset-boxheight* (make-parameter #f))

(define *typeset-indent* (make-parameter 0))
(define *typeset-tag* (make-parameter #f))
(define *typeset-font* (make-parameter #f))
(define *typeset-currentfont* (make-parameter #f))

(define *typeset-document* (make-parameter #f))
(define *typeset-page* (make-parameter #f))
(define *typeset-page-height* (make-parameter #f))
(define *typeset-line* (make-parameter #f))
(define *typeset-line-height* (make-parameter #f))
(define *typeset-line-width* (make-parameter #f))


(define (typeset-initialize)
  (set! *typeset-top-margin* 72)
  (set! *typeset-left-margin* 72)
  (set! *typeset-indent-width* 24)
  (set! *typeset-line-space* 10)
  (set! *typeset-heading-space* 10)
  
  (set! *typeset-height* (make-parameter (- *default-height* (* *typeset-top-margin* 2))))
  (set! *typeset-width* (make-parameter (- *default-width* (* *typeset-left-margin* 2))))
  
  (set! *typeset-font-times* (make-parameter #f))
  (set! *typeset-font-helvetica* (make-parameter #f))
  (set! *typeset-font-courier* (make-parameter #f))
  (set! *typeset-font-mincho* (make-parameter #f))
  (set! *typeset-font-gothic* (make-parameter #f))
  
  (set! *typeset-boxwidth* (make-parameter #f))
  (set! *typeset-boxheight* (make-parameter #f))
  
  (set! *typeset-indent* (make-parameter 0))
  (set! *typeset-tag* (make-parameter #f))
  (set! *typeset-font* (make-parameter #f))
  (set! *typeset-currentfont* (make-parameter #f))
  
  (set! *typeset-document* (make-parameter #f))
  (set! *typeset-page* (make-parameter #f))
  (set! *typeset-page-height* (make-parameter #f))
  (set! *typeset-line* (make-parameter #f))
  (set! *typeset-line-height* (make-parameter #f))
  (set! *typeset-line-width* (make-parameter #f))
  )
  

;; font size
(define (typeset-fontwidth c)
  (let*
      ((code (char->integer c))
       (enfont (first (*typeset-font*)))
       (jafont (second (*typeset-font*))))
    (if (< code 256)
	(let* 
	    ((table
	      (cond ((eq? (first enfont) 'times)
		     (u16vector
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  333  408  500  500  833  778  222
		       333  333  500  564  250  333  250  278
		       500  500  500  500  500  500  500  500
		       500  500  278  278  564  564  564  444
		       921  722  667  667  722  611  556  722
		       722  333  389  722  611  889  722  722
		       556  722  667  556  611  722  722  944
		       722  722  611  333  278  333  469  500
		       333  444  500  444  500  444  333  500
		       500  278  278  500  278  778  500  500
		       500  500  333  389  278  500  500  722
		       500  500  444  480  200  480  541  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  333  500  500  167  500  500  500
		       500  180  444  500  333  333  556  556
		       250  500  500  500  250  250  453  350
		       333  444  444  500  1000 1000 250  444
		       250  333  333  333  333  333  333  333
		       333  250  333  333  250  333  333  333
		       1000 250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  889  250  276  250  250  250  250
		       611  722  889  310  250  250  250  250
		       250  667  250  250  250  278  250  250
		       278  500  722  500  250  250  250  250))
		     ((eq? (first enfont) 'times-bold)
		      (u16vector
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  389  555  500  500  833  778  222
		       333  333  500  570  250  333  250  278
		       500  500  500  500  500  500  500  500
		       500  500  333  333  570  570  570  500
		       832  667  667  667  722  667  667  722
		       778  389  500  667  611  889  722  722
		       611  722  667  556  611  722  667  889
		       667  611  611  333  278  333  570  500
		       333  500  500  444  500  444  333  500
		       556  278  278  500  278  778  556  500
		       500  500  389  389  278  556  444  667
		       500  444  389  348  220  348  570  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  389  500  500  167  500  500  500
		       500  278  500  500  333  333  556  556
		       250  500  500  500  250  250  500  350
		       333  500  500  500  1000 1000 250  500
		       250  333  333  333  333  333  333  333
		       333  250  333  333  250  333  333  333
		       1000 250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  944  250  266  250  250  250  250
		       611  722  944  300  250  250  250  250
		       250  722  250  250  250  278  250  250
		       278  500  722  500  250  250  250  250))
		    ((eq? (first enfont) 'times-italic)
		     (u16vector
		      250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  333  420  500  500  833  778  222
		       333  333  500  675  250  333  250  278
		       500  500  500  500  500  500  500  500
		       500  500  333  333  675  675  675  500
		       920  611  611  667  722  611  611  722
		       722  333  444  667  556  833  667  722
		       611  722  611  500  556  722  611  833
		       611  556  556  389  278  389  422  500
		       333  500  500  444  500  444  278  500
		       500  278  278  444  278  722  500  500
		       500  500  389  389  278  500  444  667
		       444  444  389  400  275  400  541  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  389  500  500  167  500  500  500
		       500  214  556  500  333  333  500  500
		       250  500  500  500  250  250  523  350
		       333  556  556  500  889  1000 250  500
		       250  333  333  333  333  333  333  333
		       333  250  333  333  250  333  333  333
		       889  250  250  250  250  250  250  250
		       250  250  250  250  250  250  250  250
		       250  889  250  276  250  250  250  250
		       556  722  944  310  250  250  250  250
		       250  667  250  250  250  278  250  250
		       278  500  667  500  250  250  250  250))
		    ((eq? (first enfont) 'courier)
		     (u16vector
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600
		       600  600  600  600  600  600  600  600))
		    ((eq? (first enfont) 'helvetica)
		     (u16vector
		      278  278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  278  355  556  556  889  667  221
		       333  333  389  584  278  333  278  278
		       556  556  556  556  556  556  556  556
		       556  556  278  278  584  584  584  556
		       1015 667  667  722  722  667  611  778
		       722  278  500  667  556  833  722  778
		       667  778  722  667  611  722  667  944
		       667  667  611  278  278  278  469  556
		       222  556  556  500  556  556  278  556
		       556  222  222  500  222  833  556  556
		       556  556  333  500  278  556  500  722
		       500  500  500  334  260  334  584  278
		       278  278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  333  556  556  167  556  556  556
		       556  191  333  556  333  333  500  500
		       278  556  556  556  278  278  537  350
		       222  333  333  556  1000 1000 278  611
		       278  333  333  333  333  333  333  333
		       333  278  333  333  278  333  333  333
		       1000 278  278  278  278  278  278  278
		       278  278  278  278  278  278  278  278
		       278  1000 278  370  278  278  278  278
		       556  778  1000 365  278  278  278  278
		       278  889  278  278  278  278  278  278
		       222  611  944  611  278  278  278  278))
		    (else
		     (u16vector)))))
	(/ (* (ref table code 1000) (second enfont)) 1000))
	(cond
	 ((eq? (first jafont) 'mincho)
	  (second jafont))
	 ((eq? (first jafont) 'gothic)
	  (second jafont))))))

(define (typeset-fontheight c)
  (let*
      ((code (char->integer c))
       (enfont (first (*typeset-font*)))
       (jafont (second (*typeset-font*))))
    (if (< code 256)
	(second enfont)
	(second jafont))))


;; charbox: ((height width (font size) tag) char)
;;          ((height width (font size) #f) char)
;;          ((height width #f #f) char)
;;
;;          (caar charbox) -> height
;;          (cadar charbox) -> width
;;          (caddar charbox) -> (font size)
;;          (cadddar charbox) -> tag
;;          (cadr charbox) -> char
;;
;; linebox: ((max-height indent-width #t) (charbox ...))
;;          ((max-height indent-width #f) (charbox ...))
;;
;;          (caar charbox) -> max-height
;;          (cadar charbox) -> indent-width
;;
;; pagebox: (linebox ...)


;; put charbox into linebox (and newline)
(define (typeset-addchar c)
  (*typeset-font* (typeset-font (*typeset-tag*)))
  (let* ((w (typeset-fontwidth c))
	 (h (typeset-fontheight c))
	 (font (if (< (char->integer c) 256)
		   (first (*typeset-font*))
		   (second (*typeset-font*)))))

    (when (equal? (car (*typeset-tag*)) `(h2)) (set! h (+ h *typeset-heading-space*)))

    (if (< (*typeset-boxwidth*) (+ (*typeset-line-width*) w))
	(typeset-newline-with-rule) #f)
    ;(if (< (*typeset-boxheight*) (+ (*typeset-page-height*) h))
    ;    (typeset-newpage-with-rule) #f)

    (if (< (*typeset-line-height*) h) (*typeset-line-height* h) #f)
    (if (equal? (*typeset-currentfont*) font)
	(*typeset-line* (cons `((,h ,w #f #f) ,c) (*typeset-line*)))
	(*typeset-line* (cons `((,h ,w ,font #f) ,c) (*typeset-line*))))
    (*typeset-currentfont* font)

    (*typeset-line-width* (+ (*typeset-line-width*) w))
))


;; newline
(define (typeset-newline)
  (let ((maxh (+ (*typeset-line-height*) *typeset-line-space*)))
    (if (< (*typeset-boxheight*) (+ (*typeset-page-height*) maxh))
      (typeset-newpage) #f)
    (*typeset-page-height* (+ (*typeset-page-height*) maxh))
    (*typeset-page* (cons `((,maxh ,(*typeset-indent*) #f) ,(reverse (*typeset-line*))) (*typeset-page*)))
    (*typeset-line-height* 0)
    (*typeset-line-width* 0)
    (*typeset-line* '())
    ))


;; newline with Japanese typeset rule
(define (typeset-newline-with-rule)
  (define (iter count)
    (let ((top-char (second (ref (*typeset-line*) count `(#f #\null))))
	  (last-char (second (ref (*typeset-line*) (+ count 1) `(#f #\null)))))
      (cond ((string-index ")。、，．！？」』）】〕｝〉》］ーぁぃぅぇぉゃゅょっァィゥェォャュョッヶ" top-char)
	     (iter (+ count 1)) )
	    ((string-index "(「『（【【〔｛〈《［" last-char)
	     (iter (+ count 1)) )
	    ((and (char-alphabetic? last-char) (char-alphabetic? top-char))
	     (iter (+ count 1)) )
	    (else
	     (let ((maxh (+ (*typeset-line-height*) *typeset-line-space*)))
	       (if (< (*typeset-boxheight*) (+ (*typeset-page-height*) maxh))
		   (typeset-newpage-with-rule) #f)
	       (*typeset-page* (cons `((,maxh ,(*typeset-indent*) #f) ,(reverse (drop (*typeset-line*) (+ count 1)))) (*typeset-page*)))
	       (*typeset-page-height* (+ (*typeset-page-height*) maxh))

	       (*typeset-line* (take (*typeset-line*) (+ count 1)))
	       (*typeset-line-height* 0)
	       (*typeset-line-width* 0)
	       (for-each
		(lambda (x)
		  (let ((h (caar x)) (w (cadar x)))
		    (if (< (*typeset-line-height*) h) (*typeset-line-height* h) #f)
		    (*typeset-line-width* (+ (*typeset-line-width*) w))
		    ))
		(*typeset-line*)
		)
	       )))))
  (iter 0) )


;; newpage
(define (typeset-newpage)
  (*typeset-document* (cons (reverse (*typeset-page*)) (*typeset-document*)))
  (*typeset-page-height* 0)
  (*typeset-page* '()))


;; newpage with typeset rule
(define (typeset-newpage-with-rule)
  (*typeset-document* (cons (reverse (*typeset-page*)) (*typeset-document*)))
  (*typeset-page-height* 0)
  (*typeset-page* '()))


;; from tag symbol to font symbol
(define (typeset-font tag)
  (let ((name (caar tag)))
    (cond
     ((eq? name 'h1) `((helvetica 24) (gothic 24)))
     ((eq? name 'h2) `((helvetica 16) (gothic 16)))
     ((eq? name 'b) `((helvetica 12) (gothic 12)))
     ((eq? name 'em) `((helvetica 14) (gothic 14)))
     ((eq? name 'strong) `((helvetica 14) (gothic 14)))
     ((eq? name 'tt) `((courier 12) (gothic 12)))
     ((eq? name 'pre) `((courier 12) (gothic 12)))
     (else `((times 12) (mincho 12)))
     )))


;; from font symbol to fontobj
(define (typeset-get-font f)
  (cond ((eq? f 'times) (font-name (*typeset-font-times*)))
	((eq? f 'helvetica) (font-name (*typeset-font-helvetica*)))
	((eq? f 'courier) (font-name (*typeset-font-courier*)))
	((eq? f 'mincho) (font-name (*typeset-font-mincho*)))
	((eq? f 'gothic) (font-name (*typeset-font-gothic*)))
	))


;; processing from interp-pdf value to pdf-file
;; TODO: 行ボックスのインデントレベルに応じて改行後インデント幅をつける
;; TODO: 行高さを最後の文字要素から取り出し、マージン幅を加えて改行する
;; TODO: 「タグ→フォント→fontobjの対応」「ページの初期出力」を引数で与える
;; TODO: draw-charを使わず、連続文字列TJを出力する
(define (pdf-proc par)
  (typeset-initialize)
  (*typeset-font* (typeset-font `((p))))
  (*typeset-boxheight* (*typeset-height*))
  (*typeset-document* '())
  (*typeset-page* '())
  (*typeset-page-height* 0)
  (*typeset-line* '())
  (*typeset-line-height* 0)
  (*typeset-line-width* 0)

  (*typeset-font-times* (build-font "Times-Roman"))
  (*typeset-font-helvetica* (build-font "Helvetica"))
  (*typeset-font-courier* (build-font "Courier"))
  (*typeset-font-mincho* (build-jfont "Ryumin-Light" "EUC-H"))
  (*typeset-font-gothic* (build-jfont "GothicBBB-Medium" "EUC-H"))

  ;; typesettting phase
  (for-each
   (lambda (elem)
     (let* (
	    (numbering (first elem))
	    (indent (second elem))
	    (lines (third elem))
	    )
       (for-each
	(lambda (x)
	  (let* (
		 (flag (first x))
		 (tag (second x))
		 (str (third x))
		 )
	    (if (and flag (*typeset-tag*)) (typeset-newline) #f)
	    (*typeset-indent* indent)
	    (*typeset-boxwidth* (- (*typeset-width*)
				   (* *typeset-indent-width* (*typeset-indent*))))
	    (*typeset-tag* tag)
	    (string-for-each
	     (lambda (c) (typeset-addchar c))
	     str )))
	lines )))
   par )
  (typeset-newline)
  (typeset-newpage)

  ;; output phase
  (let ((fontname 'times)
	(fontsize 12)
	(indent (* *typeset-indent-width* (*typeset-indent*))))
    (for-each
     (lambda (pbox)
       (with-page
        (lambda ()
          (move-to *typeset-left-margin* (- *default-height* *typeset-top-margin*))
          (line-to (+ *typeset-left-margin* (*typeset-width*)) (- *default-height* *typeset-top-margin*))
          (line-to (+ *typeset-left-margin* (*typeset-width*)) *typeset-top-margin*)
          (line-to *typeset-left-margin* *typeset-top-margin*)
          (line-to *typeset-left-margin* (- *default-height* *typeset-top-margin*))
          (stroke)
          (in-text-mode
           (move-text *typeset-left-margin* (- *default-height* *typeset-top-margin*))
           (for-each
            (lambda (lbox)
	    ;(format #t "indent: ~a~%body: ~a~%" (cadar lbox) (cadr lbox))
              (move-text 0 (- (caar lbox)))
              (move-text (* *typeset-indent-width* (cadar lbox)) 0)
              (for-each
               (lambda (cbox)
                 (let* ((font (caddar cbox))
                        (char (cadr cbox)) )
                   (when font
                     (set! fontname (typeset-get-font (car font)))
                     (set! fontsize (cadr font)) )
                   (draw-char fontname fontsize char) ))
	       (cadr lbox) )
              (move-text (- (* *typeset-indent-width* (cadar lbox))) 0)
              )
	    pbox)))))
     (reverse (*typeset-document*)) ))
  )

(define (with-docdata-to-file filename thunk)
  (reset-parameters)
  (*document* (build-doc))
  (pdf-proc (thunk))
  (write-document filename))

(define (with-docdata-to-port port thunk)
  (reset-parameters)
  (*document* (build-doc))
  (pdf-proc (thunk))
  (write-document-port port))


(provide "kahua/pdf/typeset")
