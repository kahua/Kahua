#!/usr/bin/env gosh
;;; -*- mode: scheme; coding: utf-8 -*-
;;
;; script checking version and threading type.
;; $Id: check.scm,v 1.1.2.1 2007/04/24 02:24:38 bizenn Exp $

(use gauche.threads)
(use gauche.version)

(define (print-usage args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "Usage: ~a <version>" (car args)))))

(define (main args)
  (cond ((null? (cdr args))
	 (print-usage args)
	 70)
	((version<? (gauche-version) (cadr args))
	 (format (current-error-port) "\n** Required version ~s, but got ~s\n"
		 (cadr args) (gauche-version))
	 1)
	((not (eq? (gauche-thread-type) 'pthread))
	 (format (current-error-port) "\n** Require thread type pthread, but got ~s\n"
		 (gauche-thread-type))
	 1)
	(else 0)))
