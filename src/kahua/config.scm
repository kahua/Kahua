;; Keep configuration parameters
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: config.scm,v 1.4 2004/01/05 10:56:39 shiro Exp $
;;
;; This is intended to be loaded by kahua servers to share common
;; configuration parameters.
;;
;; The configuration file is simply loaded.  It must contain
;; an expression that creates a singleton instance of <kahua-config>.
;;
;; (make <kahua-config>
;;   :sockbase ....
;;   :working-directory ...)
;;

(define-module kahua.config
  (use gauche.mop.singleton)
  (use file.util)
  (export kahua-init <kahua-config>
          kahua-config
          kahua-sockbase
          kahua-logpath
          kahua-config-file
          kahua-static-document-path
          kahua-static-document-url
          ))
(select-module kahua.config)

(define *kahua-conf-default* "/etc/kahua.conf")

(define-class <kahua-config> (<singleton-mixin>)
  (;; sockbase - specifies where to open the server socket.
   ;;     Currently only unix domain socket is supported.
   ;;     The socket directory must be writable by kahua processes.
   (sockbase          :init-keyword :sockbase
                      :init-value "unix:/tmp/kahua")
   ;; working-directory - where kahua processes keeps logs and
   ;;     other various info.  This directory must be writable by
   ;;     kahua processes.
   (working-directory :init-keyword :working-directory
                      :init-value "/var/lib/kahua")

   ;; static-document-path - where kahua process puts static documents.
   ;;     Httpd must be able to show the contents under this directory.
   ;;     This directoy must be writable by kahua processes.
   (static-document-path :init-keyword :static-document-path
                         :init-value "/var/www/html/kahua")

   ;; static-document-url - the url path to reference the contents of
   ;;     static-document-path via httpd.
   (static-document-url  :init-keyword :static-document-url
                         :init-value "/kahua")
   
   ;; repository - specifies where to use cvs repository
   (repository :init-keyword :repository
	       :init-value "/var/lib/kahua/cvs")

   ;; internal
   (conf-file :init-value #f)
   )
  )

(define-method initialize ((self <kahua-config>) initargs)
  (next-method)
  ;; repository path must be absolute(for cvs).
  (let1 reppath (ref self 'repository)
    (if (relative-path? reppath)
      (set! (ref self 'repository)
            (sys-normalize-pathname reppath :absolute #t)))))

(define (sanity-check kahua-conf)
;; do some sanity check
  (let1 wdir (ref kahua-conf 'working-directory)
    (unless (and (file-is-directory? wdir)
                 (file-is-writable? wdir))
      (error "working directory does not exist or is not writable:" wdir))
    (make-directory* (build-path wdir "logs"))
    (make-directory* (build-path wdir "checkout"))))


;; kahua-init [conf-file] [skip-check?]
;; if "skip-check?" is #t, read kahua.conf only(not check 
(define (kahua-init . args)
  (let-optionals* args
		  ((cfile #f)
		   (skip-check? #f))
    (let1 cfile (or cfile *kahua-conf-default*)
      (if (file-is-readable? cfile)
        (begin
         (load cfile :environment (find-module 'kahua.config))
         (set! (ref (instance-of <kahua-config>) 'conf-file) cfile)
         (unless skip-check? (sanity-check (instance-of <kahua-config>))))
      (error "configuration file ~a is not readable.  using default settings."
            cfile))))
  ;; Include working directory to *load-path*.
  ;; We don't use add-load-path here, since it is a macro that does
  ;; work at compile time.
  (push! *load-path*
         (build-path (ref (instance-of <kahua-config>) 'working-directory)
                     "checkout"))
  (instance-of <kahua-config>))


;; utility functions

(define (kahua-config)
  (instance-of <kahua-config>))

(define kahua-sockbase
  (getter-with-setter
   (lambda () (ref (kahua-config) 'sockbase))
   (lambda (base) (set! (ref (kahua-config) 'sockbase) base))))

(define (kahua-logpath filename)
  (build-path (ref (kahua-config) 'working-directory)
              "logs" filename))

(define (kahua-config-file)
  (ref (kahua-config) 'conf-file))

(define (kahua-static-document-path path)
  (build-path (ref (kahua-config) 'static-document-path) path))

(define (kahua-static-document-url path)
  (build-path (ref (kahua-config) 'static-document-url) path))

(provide "kahua/config")
