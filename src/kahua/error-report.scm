;; -*- mode: scheme; coding: utf-8 -*-
(define-module kahua.error-report
  (use kahua.util)
  (use kahua.elem)

  (export kahua-log-bug kahua-error-report-proc)
  )

(select-module kahua.error-report)

(define (kahua-error-report-proc exc)
  (let ((id (kahua-log-bug exc)))
    (html/
     (head/ (title/ "Error"))
     (body/
      (h1/ "エラー")
      (p/ #`"エラー番号 (,|id|)")
      (p/ "エラーが発生しました。上記のエラー番号とともに管理者にご連絡ください。")
      (h1/ "詳細")
      (pre/ (kahua-error-string exc #t))
      ))))

;; returns bug-id
(define (kahua-log-bug exc)
  (let ((id (generate-bug-id)))
    (kahua:log-format "begin error: (~d)" id)
    (kahua-dump-user)
    (kahua:log-format "~a" (kahua-error-string exc #t))
    (kahua:log-format "end   error: (~d)" id)
    id))

(define (generate-bug-id)
  (format #f "~d-~d" (sys-time) (modulo (sys-random) 1000)))

(define (kahua-dump-user)
  (guard (e (else (kahua:log-format "user dump failed")))
    (kahua:log-format "user ~a" (kahua-current-user-name))))

(provide "kahua/error-report")
