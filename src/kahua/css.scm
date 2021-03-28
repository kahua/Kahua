;; Provide sexp CSS
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(define-module kahua.css
  (use util.match)
  (use util.list)
  (use text.tree)
  (use srfi-11)
  (export parse-stylesheet
          css:value-of
          <css:color>
          css:darken
          css:brighten
          css:blend
          css:bright?
          css:dark?
          )
  )

(select-module kahua.css)

(define-method css:value-of (value)
  (x->string  value))

(define-method css:value-of ((value <string>))
  (format "~s" value))

(define-method css:value-of ((value <keyword>))
  (format "~s" value))

;;==========================================================
;; CSS Parsers
;;

;; parse-stylesheet :: Node -> Stree
(define (parse-stylesheet exp)
  (map parse-statement exp))

(define (parse-statement exp)
  (match exp
    (('@ at-rule)
     (list "@" (parse-at-rule at-rule) "\n\n"))
    (ruleset
     (parse-ruleset ruleset))))

(define (parse-at-rule exp)
  (match exp
    (('import value media ...)
     (list "import "
           (parse-value value)
           (if (null? media) '()
             (list " "
                   (parse-media (car media))))
           ";"
           ))

    (('media media ruleset ...)
     (list "media "
           (parse-media media)
           (format-block (map parse-ruleset ruleset))))

    (('page declaration ...)
     (list "page "
           (format-block (map parse-declaration declaration) ";\n")))

    ((('page rest ...) declaration ...)
     (list "page "
           (map css:value-of rest)
           (format-block (map parse-declaration declaration) ";\n")))

    (('font-face declaration ...)
     (list "page "
           (format-block (map parse-declaration declaration) ";\n")))

    (('charset charset)
     (list "charset "
           (parse-value charset)
           ";"))
    ))

(define (parse-media exp)
  (match exp
    (('or media ...)
     (intersperse ", " media))
    (media
     media)))

(define (parse-ruleset exp)
  (match exp
    ((selector declaration ...)
     (list (parse-selector selector)
           (format-block (map parse-declaration declaration) ";\n")))))

(define (parse-declaration exp)
  (match exp
    ((property value ...)
     (list property ":"
           (intersperse " " (map parse-value value))))))

(define (parse-value exp)
  (match exp
    ((function arg ...)
     (list function "(" (intersperse ", " (map css:value-of arg)) ")"))
    (value
     (css:value-of value))
    ))

(define (parse-selector exp)
  (match exp
    (('>> p ...)
     (intersperse " " (map parse-selector p)))
    (('> p ...)
     (intersperse " > " (map parse-selector p)))
    (('+ p ...)
     (intersperse " + " (map parse-selector p)))
    (('or p ...)
     (intersperse ", " (map parse-selector p)))
    (('@ p attr ...)
     (list (parse-selector p)
           (map parse-attribute-selector attr)))
    (('lang p arg)
     (list (parse-selector p)
           ":lang"
           "(" arg ")"))
    (p
     (intersperse #\# (string-split (format "~a" p) #\$)))
    ))

(define (parse-attribute-selector exp)
  (match exp
    ((`~= attr val)
     (list "[" attr "~=\"" val "\"]"))
    (('-= attr val)
     (list "[" attr "|=\"" val "\"]"))
    (('= attr val)
     (list "[" attr "=\"" val "\"]"))
    (attr
     (list "[" attr "]"))))

(define (format-block set . delimiter)
  (let1 delim (get-optional delimiter "\n")
    (list " {\n"
          (intersperse delim set)
          "\n}\n\n")))

;;==========================================================
;; CSS Color Utility
;;

(define-constant *color-keywords*
  (alist->hash-table '((aliceblue . (240 248 255))
                       (antiquewhite . (250 235 215))
                       (aqua . ( 0 255 255))
                       (aquamarine . (127 255 212))
                       (azure . (240 255 255))
                       (beige . (245 245 220))
                       (bisque . (255 228 196))
                       (black . ( 0 0 0))
                       (blanchedalmond . (255 235 205))
                       (blue . ( 0 0 255))
                       (blueviolet . (138 43 226))
                       (brown . (165 42 42))
                       (burlywood . (222 184 135))
                       (cadetblue . ( 95 158 160))
                       (chartreuse . (127 255 0))
                       (chocolate . (210 105 30))
                       (coral . (255 127 80))
                       (cornflowerblue . (100 149 237))
                       (cornsilk . (255 248 220))
                       (crimson . (220 20 60))
                       (cyan . ( 0 255 255))
                       (darkblue . ( 0 0 139))
                       (darkcyan . ( 0 139 139))
                       (darkgoldenrod . (184 134 11))
                       (darkgray . (169 169 169))
                       (darkgreen . ( 0 100 0))
                       (darkgrey . (169 169 169))
                       (darkkhaki . (189 183 107))
                       (darkmagenta . (139 0 139))
                       (darkolivegreen . ( 85 107 47))
                       (darkorange . (255 140 0))
                       (darkorchid . (153 50 204))
                       (darkred . (139 0 0))
                       (darksalmon . (233 150 122))
                       (darkseagreen . (143 188 143))
                       (darkslateblue . ( 72 61 139))
                       (darkslategray . ( 47 79 79))
                       (darkslategrey . ( 47 79 79))
                       (darkturquoise . ( 0 206 209))
                       (darkviolet . (148 0 211))
                       (deeppink . (255 20 147))
                       (deepskyblue . ( 0 191 255))
                       (dimgray . (105 105 105))
                       (dimgrey . (105 105 105))
                       (dodgerblue . ( 30 144 255))
                       (firebrick . (178 34 34))
                       (floralwhite . (255 250 240))
                       (forestgreen . ( 34 139 34))
                       (fuchsia . (255 0 255))
                       (gainsboro . (220 220 220))
                       (ghostwhite . (248 248 255))
                       (gold . (255 215 0))
                       (goldenrod . (218 165 32))
                       (gray . (128 128 128))
                       (grey . (128 128 128))
                       (green . ( 0 128 0))
                       (greenyellow . (173 255 47))
                       (honeydew . (240 255 240))
                       (hotpink . (255 105 180))
                       (indianred . (205 92 92))
                       (indigo . ( 75 0 130))
                       (ivory . (255 255 240))
                       (khaki . (240 230 140))
                       (lavender . (230 230 250))
                       (lavenderblush . (255 240 245))
                       (lawngreen . (124 252 0))
                       (lemonchiffon . (255 250 205))
                       (lightblue . (173 216 230))
                       (lightcoral . (240 128 128))
                       (lightcyan . (224 255 255))
                       (lightgoldenrodyellow . (250 250 210))
                       (lightgray . (211 211 211))
                       (lightgreen . (144 238 144))
                       (lightgrey . (211 211 211))
                       (lightpink . (255 182 193))
                       (lightsalmon . (255 160 122))
                       (lightseagreen . ( 32 178 170))
                       (lightskyblue . (135 206 250))
                       (lightslategray . (119 136 153))
                       (lightslategrey . (119 136 153))
                       (lightsteelblue . (176 196 222))
                       (lightyellow . (255 255 224))
                       (lime . ( 0 255 0))
                       (limegreen . ( 50 205 50))
                       (linen . (250 240 230))
                       (magenta . (255 0 255))
                       (maroon . (128 0 0))
                       (mediumaquamarine . (102 205 170))
                       (mediumblue . ( 0 0 205))
                       (mediumorchid . (186 85 211))
                       (mediumpurple . (147 112 219))
                       (mediumseagreen . ( 60 179 113))
                       (mediumslateblue . (123 104 238))
                       (mediumspringgreen . ( 0 250 154))
                       (mediumturquoise . ( 72 209 204))
                       (mediumvioletred . (199 21 133))
                       (midnightblue . ( 25 25 112))
                       (mintcream . (245 255 250))
                       (mistyrose . (255 228 225))
                       (moccasin . (255 228 181))
                       (navajowhite . (255 222 173))
                       (navy . ( 0 0 128))
                       (oldlace . (253 245 230))
                       (olive . (128 128 0))
                       (olivedrab . (107 142 35))
                       (orange . (255 165 0))
                       (orangered . (255 69 0))
                       (orchid . (218 112 214))
                       (palegoldenrod . (238 232 170))
                       (palegreen . (152 251 152))
                       (paleturquoise . (175 238 238))
                       (palevioletred . (219 112 147))
                       (papayawhip . (255 239 213))
                       (peachpuff . (255 218 185))
                       (peru . (205 133 63))
                       (pink . (255 192 203))
                       (plum . (221 160 221))
                       (powderblue . (176 224 230))
                       (purple . (128 0 128))
                       (red . (255 0 0))
                       (rosybrown . (188 143 143))
                       (royalblue . ( 65 105 225))
                       (saddlebrown . (139 69 19))
                       (salmon . (250 128 114))
                       (sandybrown . (244 164 96))
                       (seagreen . ( 46 139 87))
                       (seashell . (255 245 238))
                       (sienna . (160 82 45))
                       (silver . (192 192 192))
                       (skyblue . (135 206 235))
                       (slateblue . (106 90 205))
                       (slategray . (112 128 144))
                       (slategrey . (112 128 144))
                       (snow . (255 250 250))
                       (springgreen . ( 0 255 127))
                       (steelblue . ( 70 130 180))
                       (tan . (210 180 140))
                       (teal . ( 0 128 128))
                       (thistle . (216 191 216))
                       (tomato . (255 99 71))
                       (turquoise . ( 64 224 208))
                       (violet . (238 130 238))
                       (wheat . (245 222 179))
                       (white . (255 255 255))
                       (whitesmoke . (245 245 245))
                       (yellow . (255 255 0))
                       (yellowgreen . (154 205 50)))))

(define-class <css:color> ()
  ((r :init-keyword :r)
   (g :init-keyword :g)
   (b :init-keyword :b)))

(define-method css:value-of ((value <css:color>))
  (apply format "rgb(~d,~d,~d)"
         (map (lambda (p)
                (x->integer (ref value p)))
              '(r g b))))

(define (persent? v)
  (#/^\d{1,3}%$/ (x->string v)))

(define (persent->255 v)
  (let1 persent (x->integer ((#/\d+/ (x->string v))))
    (* 255 (/ persent 100))))

(define (hex? v)
  (#/^#[0-9a-f]{1,2}$/ (x->string v)))

(define (hex->255 v)
  (let1 hex ((#/[0-9a-f]+/ (x->string v)))
    (string->number (if (= (string-length hex) 2) hex
                      (string-append hex hex)) 16)))

(define-reader-ctor 'css:color
  (match-lambda*
   (((? number? r) (? number? g) (? number? b))
    (make <css:color> :r r :g g :b b))
   (((? persent? r) (? persent? g) (? persent? b))
    (make <css:color>
      :r (persent->255 r)
      :g (persent->255 g)
      :b (persent->255 b)))
   (((? hex? r) (? hex? g) (? hex? b))
    (make <css:color>
      :r (hex->255 r)
      :g (hex->255 g)
      :b (hex->255 b)))
   ((color-keyword)
    (let1 rgb (hash-table-get *color-keywords* color-keyword)
      (match-let1 (r g b) rgb
        (make <css:color> :r r :g g :b b))))))

(define-method write-object ((self <css:color>) out)
  (format out "#,(css:color ~a ~a ~a)" (ref self 'r) (ref self 'g) (ref self 'b)))


(define (css:darken color percent)
  (let*-values (((h s b) (css:color->HSB color))
                ((r g b) (css:HSB->RGB h s (max (- b percent) 0))))
    (make <css:color> :r r :g g :b b)))

(define (css:brighten color percent)
  (let*-values (((h s b) (css:color->HSB color))
                ((r g b) (css:HSB->RGB h s (max (+ b percent) 1))))
    (make <css:color> :r r :g g :b b)))

(define (css:blend color1 color2)
  (make <css:color>
    :r (/ (+ (ref color1 'r) (ref color2 'r)) 2)
    :g (/ (+ (ref color1 'g) (ref color2 'g)) 2)
    :b (/ (+ (ref color1 'b) (ref color2 'b)) 2)))

(define (css:bright? color)
  (let-values (((h s b) (css:color->HSB color)))
    (> b 0.5)))

(define (css:dark? color)
  (not (css:bright? color)))

(define (css:color->HSB color)
  (css:RGB->HSB (ref color 'r) (ref color 'g) (ref color 'b)))

(define (css:RGB->HSB r g b)
  (let* ((cmax (max r g b))
         (cmin (min r g b))
         (brightness (/ cmax 255))
         (saturation (if (= 0 cmax) 0
                       (/ (- cmax cmin)
                          cmax)))
         (hue (if (= saturation 0) 0
                (/ (cond ((= cmax r)
                          (/ (- g b)
                             (- cmax cmin)))
                         ((= cmax g)
                          (+ (/ (- b r)
                                (- cmax cmin))
                             2))
                         ((= cmax b)
                          (+ (/ (- r g)
                                (- cmax cmin))
                             4)))
                   6)))
         (hue (if (< hue 0) (+ hue 1) hue)))
    (values hue saturation brightness)))

(define (css:HSB->RGB h s b)
  (if (= s 0)
      (let1 rgb (+ (* 255 b) 0.5)
        (values rgb rgb rgb))
    (let* ((h (* (- h (floor h)) 6))
           (f (- h (floor h)))
           (p (* b (- 1 s)))
           (q (* b (- 1 (* s f))))
           (t (* b (- 1 (* s (- 1 f))))))
      (case (x->integer (floor h))
        ((0) (values (+ (* b 255) 0.5)
                     (+ (* t 255) 0.5)
                     (+ (* p 255) 0.5)))
        ((1) (values (+ (* q 255) 0.5)
                     (+ (* b 255) 0.5)
                     (+ (* p 255) 0.5)))
        ((2) (values (+ (* p 255) 0.5)
                     (+ (* b 255) 0.5)
                     (+ (* t 255) 0.5)))
        ((3) (values (+ (* p 255) 0.5)
                     (+ (* q 255) 0.5)
                     (+ (* b 255) 0.5)))
        ((4) (values (+ (* t 255) 0.5)
                     (+ (* p 255) 0.5)
                     (+ (* b 255) 0.5)))
        ((5) (values (+ (* b 255) 0.5)
                     (+ (* p 255) 0.5)
                     (+ (* q 255) 0.5)))))))

(provide "kahua/css")
