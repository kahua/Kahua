(define-module kahua.pdf.main
  (extend kahua.pdf.util)
  (use gauche.parameter)
  (use gauche.uvector)
  (use srfi-1)
  (use srfi-13)
  (export
           with-document
           with-document-to-file
           with-document-to-port
           with-page
           build-font
	   build-jfont
	   build-cidsysteminfo
           write-document
           write-document-port
           font-name
           unit-size
           page-width
           page-height
	   )
  )
(select-module kahua.pdf.main)

;; class definitions

(define-class <doc> ()
  (
   (catalog   :init-value #f
	      :init-keyword :catalog   
	      :accessor catalog-of)
   (root-page :init-value #f
	      :init-keyword :root-page 
	      :accessor root-page-of)
   (pages     :init-value #f
	      :init-keyword :pages
	      :accessor pages-of)
   (xref      :init-value #f
	      :init-keyword :xref
	      :accessor xref-of)
   (objects   :init-value #f
	      :init-keyword :objects
	      :accessor objects-of)
   (fonts     :init-value #f
	      :init-keyword :fonts
	      :accessor fonts-of)
   ))

(define-class <indirect-obj> ()
  (
   (obj-number :init-value #f
	       :init-keyword :obj-number
	       :accessor obj-number-of)
   (gen-number :init-value #f
	       :init-keyword :gen-number
	       :accessor gen-number-of)
   (content    ;;:init-value #f
	       :init-keyword :content
	       :accessor content-of)
   (name       :init-value #f
	       :init-keyword :name
	       :accessor name-of)
   ))

(define-class <dictionary> ()
  (
   (values :init-value #f
	   :init-keyword :values
	   :accessor values-of)
   ))
 
(define-class <pdf-stream> ()
  (
   (content :init-value #f
	    :init-keyword :content
	    :accessor content-of)
   ))
  
;; constants

(define *unit-size* 72)  ; default 72 points per inch
(define *default-width* 595)  ; in units, A4
(define *default-height* 842) ; in units, A4
  
;; handy parameters
  
(define *output* (make-parameter #f))
(define *document* (make-parameter #f))  
(define *page* (make-parameter #f))
(define *next-obj-number* (make-parameter 0))
(define *next-var-number* (make-parameter 100))
(define *page-width* (make-parameter *default-width*))
(define *page-height* (make-parameter *default-height*))
  
(define (get-next-obj-number)
  (*next-obj-number* (+ 1 (*next-obj-number*)))
  (*next-obj-number*))

(define (reset-parameters)
  (*output* #f)
  (*document* #f)
  (*page* #f)
  (*next-obj-number* 0)
  (*next-var-number* 100)
  (*page-height* *default-height*)
  (*page-width* *default-width*))
    
;; structure builder funcs
  
(define-syntax enforce-/
  (syntax-rules ()
    ((_ arg)
     (unless (char=? (string-ref arg 0) #\/)
	     (set! arg (string-append "/" arg))))
    ((_ arg1 arg2 ...)
     (begin
       (enforce-/ arg1)
       (enforce-/ arg2 ...)))))

(define (build-indirect-obj content)
  (let ((obj (make <indirect-obj> 
	       :obj-number (get-next-obj-number)
	       :gen-number 0 
	       :content content
	       :name "indirect-obj")))
    (if (*document*)
	(set! (objects-of (*document*)) (cons obj (objects-of (*document*)))))
    obj))

(define (build-dictionary values)
  (let ((obj (make <dictionary> :values values)))
    obj))

(define (build-pdf-stream content)
  (let ((obj (make <pdf-stream> :content content)))
    obj))

(define (build-font base-font)
  (enforce-/ base-font)
  (let ((obj (build-indirect-obj
	      (build-dictionary `(("/Type" . "/Font")
				  ("/Subtype" . "/Type1")
				  ("/BaseFont" . ,base-font)
				  ("/Encoding" . "/WinAnsiEncoding"))))))
    ;(set! (base-font-of obj) base-font)
    (set! (name-of obj) (gen-name "/CLF"))
    (set! (fonts-of (*document*)) (cons obj (fonts-of (*document*))))
    obj))


;; CHANGED by syd: Japanese font available
(define (build-cidsysteminfo)
  (let ((obj (build-indirect-obj
	      (build-dictionary
	       `(("/Registry" . "(Adobe)")
		 ("/Ordering" . "(Japan1)")
		 ("/Supplement" . 2))))))
  obj))

(define (build-jfont base-font encoding)
  (let1 *cidsysteminfo* (make-parameter #f)
    (enforce-/ base-font)
    (if (not (*cidsysteminfo*))
      (*cidsysteminfo* (build-cidsysteminfo)))
    (let* ((jfd (build-indirect-obj
                 (build-dictionary
                  (cond ((string=? base-font "/Ryumin-Light")
                         `(("/Type" . "/FontDescriptor")
                           ("/Ascent" . 723)
                           ("/Flags" . 6)
                           ("/Style" . "<</Panose <010502020300000000000000>>>")
                           ("/Descent" . -241)
                           ("/ItalicAngle" . 0)
                           ("/XHeight" . 450)
                           ("/CapHeight" . 709)
                           ("/StemV" . 69)
                           ("/FontBBox" . #(-170 -331 1024 903))
                           ("/FontName" . "/Ryumin-Light")))
                        ((string=? base-font "/GothicBBB-Medium")
                         `(("/Type" . "/FontDescriptor")
                           ("/Ascent" . 752)
                           ("/CapHeight" . 737)
                           ("/Descent" . -271)
                           ("/Flags" . 4)
                           ("/FontBBox" . #(-174 -268 1001 944))
                           ("/FontName" . "/GothicBBB-Medium")
                           ("/StemV" . 99)
                           ("/ItalicAngle" . 0)
                           ("/XHeight" . 553)
                           ("/Style" . "<</Panose <0801020b0500000000000000>>>")))
                        (else '())))))
           (df (build-dictionary `(("/Type" . "/Font")
                                   ("/BaseFont" . ,base-font)
                                   ("/Subtype" . "/CIDFontType0")
                                   ("/W" . #(231 389 500 631 #(500)))
                                   ("/DW" . 1000)
                                   ("/CIDSystemInfo" . ,(lambda () (get-obj-ref (*cidsysteminfo*))))
                                   ("/FontDescriptor" . ,(lambda () (get-obj-ref jfd))))))
           (obj (build-indirect-obj
                 (build-dictionary `(("/Type" . "/Font")
                                     ("/DescendantFonts" . #(,df))
                                     ("/Subtype" . "/Type0")
                                     ("/BaseFont" . ,(format #f "~a-~a" base-font encoding))
                                     ("/Encoding" . ,(format #f "/~a" encoding)))))))
                                        ;(set! (base-font-of obj) base-font)
      (set! (name-of obj) (gen-name "/CLF"))
      (set! (fonts-of (*document*)) (cons obj (fonts-of (*document*))))
      obj)))
  
(define (build-page width height content)
  (let* ((root-page (root-page-of (*document*)))
	 (res-obj (build-dictionary `(("/Xobject" . ,(build-dictionary '()))
				      ("/Font" . ,(lambda () (get-document-font-refs))))))
	 (obj (build-indirect-obj
	       (build-dictionary `(("/Type" . "/Page")
				   ("/Parent" . ,(lambda () (get-obj-ref root-page)))
				   ("/MediaBox" . #(0 0 ,width ,height))
				   ("/Resources" . ,res-obj)
				   ("/Contents" . ,(lambda () (get-obj-ref content))))))))
    obj))

(define (build-doc)
  (let* ((root-page (build-indirect-obj 
		     (build-dictionary `(("/Type" . "/Pages")
					 ("/Count" . ,(lambda () (page-count)))
					 ("/Kids" . ,(lambda () (page-refs)))))))
	 (catalog (build-indirect-obj
		   (build-dictionary `(("/Type" . "/Catalog")
				       ("/Pages" . ,(lambda () (get-obj-ref root-page))))))))
    (let ((obj (make <doc>
		 :catalog catalog
		 :root-page root-page
		 :pages '()
		 :xref '((0 65535 f))
		 :objects '()
		 :fonts '())))
      obj)))

  
;; writers
  
(define (write-obj obj)
  (cond ((is-a? obj <indirect-obj>)
	 (write-indirect-obj obj))
	((is-a? obj <dictionary>)
	 (write-dictionary obj))
	((is-a? obj <pdf-stream>)
	 (write-pdf-stream obj))
	((is-a? obj <procedure>)
	 (write-obj (obj)))
	((is-a? obj <vector>)
	 (format (*output*) "[ ")
	 (for-each
	  (lambda (x)
	    (write-obj x))
	  (vector->list obj))
	 (format (*output*) "] "))
	(else
	 (format (*output*) "~a " obj))))

(define (write-dictionary obj)
  (format (*output*) "<< ")
  (for-each
   (lambda (x)
     (write-obj (car x))
     (write-obj (cdr x))
     (format (*output*) "~%"))
   (values-of obj))
  (format (*output*) ">> "))

(define (write-indirect-obj obj)
  (let ((offset (port-tell (*output*))))
    (set! (xref-of (*document*)) (cons (list offset 0 'n) (xref-of (*document*))))
    (format (*output*) "~d ~d obj~%" (obj-number-of obj) (gen-number-of obj))
    (write-obj (content-of obj))
    (format (*output*) "~%endobj~%")))

(define (write-pdf-stream obj)
  (let ((content (content-of obj)))
    (format (*output*) "<< /Length ~d~%>>~%stream~%~a~%endstream~%"
	 (string-length content)
	 content)))

(define write-document-port
  (lambda (port)
    (*output* port)
    (format (*output*) "%PDF-1.3~%")
    (write-obj (root-page-of (*document*)))
    (write-obj (catalog-of (*document*)))
    (for-each
     (lambda (x)
       (write-obj x))
     (reverse (objects-of (*document*))))
    (let ((xref-offset (port-tell (*output*))))
      (format (*output*) "xref~%~d ~d~%" 0 (length (xref-of (*document*))))
      (for-each
       (lambda (x)
	 (format (*output*) "~10,'0d ~5,'0d ~a ~%" (car x) (cadr x) (caddr x)))
       (reverse (xref-of (*document*))))
      (format (*output*) "trailer ~%<< /Size ~d /Root ~a~%>>~%" 
	   (+ (length (objects-of (*document*))) 1)
	   (get-obj-ref (catalog-of (*document*))))
      (format (*output*) "startxref~%~d~%%%EOF~%" xref-offset))))

;; CHANGED by syd: overright -> supersede
(define write-document
  (lambda (file)
    (let ((port (open-output-file file :if-exists :supersede)))
      (write-document-port port)
      (close-output-port port))))

;; utilities

(define (add-page page)
  (set! (pages-of (*document*)) (cons page (pages-of (*document*)))))

(define (page-count)
  (if (*document*)
      (length (pages-of (*document*)))
      0))

(define (page-refs)
  (if (*document*)
      (list->vector (map get-obj-ref (reverse (pages-of (*document*)))))
      (list->vector '())))

(define (add-dictionary-item dict name value)
  (set! (values-of dict)
	(cons (cons name value) (values-of dict))))

(define (get-dictionary-value dict name)
  (cdr (assoc name (values-of dict))))

(define (set-dictionary-value dict name value)
  (set-cdr! (assoc name (values-of dict) value)))

(define (get-obj-ref obj)
  (cond ((is-a? obj <indirect-obj>)
	 (format #f "~d ~d R" 
	      (obj-number-of obj)
	      (gen-number-of obj)))
	((procedure? obj)
	 (get-obj-ref (obj)))
	(else
	 (error 'get-obj-ref "~s not an indirect-obj" obj))))

(define (get-font-ref obj)
  (if (is-a? obj <indirect-obj>)
      (build-dictionary `((,(name-of obj) . ,(get-obj-ref obj))))
      (error 'get-font-ref "~s not an indirect-obj" obj)))

(define (gen-name prefix)
  (*next-var-number* (+ 1 (*next-var-number*)))
  (format #f "~a~d" prefix (*next-var-number*)))

(define (get-document-font-refs)
  (let ((fonts (fonts-of (*document*)))
	(dict (build-dictionary '())))
    (for-each
     (lambda (x)
       (let ((font-ref (get-font-ref x)))
	 (add-dictionary-item dict 
			      (caar (values-of font-ref)) 
			      (cdar (values-of font-ref)))))
     fonts)
    dict))

(define (font-name font)
  (name-of font))

(define (page-height)
  (*page-height*))

(define (page-width)
  (*page-width*))

(define (unit-size)
  *unit-size*)

;; helpful document structure macros

(define (with-document thunk)
  (reset-parameters)
  (*document* (build-doc))
  (thunk))

(define (with-document-to-port port thunk)
  (reset-parameters)
  (*document* (build-doc))
  (thunk)
  (write-document-port port))

(define (with-document-to-file filename thunk)
  (reset-parameters)
  (*document* (build-doc))
  (thunk)
  (write-document filename))

(define (with-page thunk . size)
  (let* ((size (get-optional size (cons *default-width* *default-height*)))
         (width (car size))
         (height (cdr size)))
    (*page-width* width)
    (*page-height* height)
    (let* ((pdf-stream
            (build-pdf-stream
             (let1 s-port (open-output-string)
               (set-page-stream s-port)
               (thunk)
               (get-output-string s-port))))
           (content (build-indirect-obj pdf-stream))
           (page (build-page width height content)))
      (*page* page)
      (add-page (*page*))
      )))

(provide "kahua/pdf/main")
