;; test PDF generation and typesetting
;; kahua.pdf テスト

;; $Id: pdf.scm,v 1.1 2004/04/07 09:55:33 nobsun Exp $

(use gauche.test)
(use file.util)

;;---------------------------------------------------------------
;; テスト開始
(test-start "kahua.pdf")

;; ロードテスト
;; kahua.pdf, kahua.pdf.* がロードでき、またそのインターフェイスに
;; 齟齬がないことを確認する。
(use kahua.pdf)
(test-module 'kahua.pdf)
(test-module 'kahua.pdf.interp)
(test-module 'kahua.pdf.main)
(test-module 'kahua.pdf.monad)
(test-module 'kahua.pdf.srfi-48)
(test-module 'kahua.pdf.state)
(test-module 'kahua.pdf.typeset)
(test-module 'kahua.pdf.util)

;;---------------------------------------------------------------
;; テスト用の手続きを定義する。
;;
;;    ［テスト項目］
;;
;;    pdf-01  SXMLの組版出力（kahua-webのPDF出力機能）
;;            ・SXMLの解析、平坦化
;;            ・日本語と英語が混在する文章の組版、禁則処理
;;            ・PDFの文字出力
;;
;;    pdf-02  直接kahua.pdfモジュールを使った図形出力
;;            ・PDF描画命令（円、楕円、塗りつぶし色指定、文字出力、座標回転）
;;
(define (pdf-01)
  (let*
      ((data (read (open-input-file "pdf-01.sxml")))
       (data (interp-html-pdf data))
       (data (exec/state (make-state 0 0 #t '() '()) data))
       (data (boxes-of-state data))
       (data (reverse (map reverse-lines data))))
    (with-docdata-to-file "_pdf-01.pdf" (lambda () data))))


(define (pdf-02)
  (with-document-to-file "_pdf-02.pdf"
   (lambda ()
     (let ((helvetica (build-font "Helvetica")))
       (with-page
        (lambda ()
          (in-text-mode
           (set-font (font-name helvetica) 16)
           (move-text 100 750)
           (draw-text "pdf-02"))
        
          (translate 50 600)
        
          (let ((x 50) (y 0))
            (do ((i 0 (+ i 1))
                 (j 8 (* j 1.05)))
                ((= i 4))
              (set-rgb-fill (* 0.1 j) (* 0.3 j) (* 0.1 j))
              (circle x y (* 3 j))
              (close-fill-and-stroke)
              (in-text-mode
               (move-text (- x 20) y)
               (set-gray-stroke 0)
               (set-gray-fill 0)
               (draw-text "Kahua"))
              (set-rgb-fill (* 0.2 j) (* 0.1 j) (* 0.3 j))
              (ellipse (- 500 x) y (* 4 j) (* 3 j))
              (close-fill-and-stroke)
              (in-text-mode
               (move-text (- 480 x) y)
               (set-gray-stroke 0)
               (set-gray-fill 0)
               (draw-text "Gauche"))
              (set! x (+ x 50))
              (set! y (+ y 50))
              ))

          (translate 300 -200)
          (do ((j 0 (+ j 1))
               (i 0.5 (* i 1.05)))
              ((= j 96))
            (in-text-mode
             (set-font (font-name helvetica) i)
             (move-text (* i 3) 0)
             (draw-text "kahua.pdf"))
            (rotate 18))))
       ))))


;; PDFファイル生成テストその１
;;   S式で表現されたHTMLテキスト（pdf-01.sxml）を読み込み
;;   テキスト整形してPDFファイルを生成。
;;   前もって生成したPDFファイルと一致することを確認する。
(pdf-01)
(test* "kahua.pdf.typeset" #t (file-equal? "./pdf-01-req.pdf" "./_pdf-01.pdf"))

;; PDFファイル生成テストその２
;;   PDF描画命令を使って、図形を含むPDFファイルを生成。
;;   前もって生成したPDFファイルと一致することを確認する。
(pdf-02)
(test* "kahua.pdf" #t (file-equal? "./pdf-02-req.pdf" "./_pdf-02.pdf"))

(test-end)

