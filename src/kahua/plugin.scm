;; Kahua plugin manager
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: plugin.scm,v 1.1 2004/02/20 10:46:30 tahara Exp $

(define-module kahua.plugin
  (use srfi-1)
  (use srfi-13)
  (use file.util)
  (use kahua.config)
  (export define-export <kahua-plugin> lookup-exports
          expand-define load-plugin %load-plugin
          define-plugin allow-module register-plugin
          initialize-plugins refresh-plugin
          all-plugins)
  )

(select-module kahua.plugin)

;; plugin container
(define *plugins* (make-hash-table 'string=?))

;; make anonymous module for binding plugin procedues.
(define (make-sandbox-plugin)
  (let ((m (make-module #f)))
    (eval '(import kahua.plugin) m)
    m))

;; plugin's procedues are bound this module.
(define *sandbox-plugin* #f)

;; plugin class.
(define-class <kahua-plugin> ()
  (  
   (name :init-keyword :name)
   (version :init-keyword :version)
   (depend :init-keyword :depend :init-value '())
   (export :init-keyword :export :init-value '())
   ))

;; explicitly. define a procedue globally in somewhere,
;; normally in *sandbox-plugin*.
(define-macro (define-export def . body)
  (if (pair? def)
    `(define-export ,(car def) (lambda ,(cdr def) ,@body))
    `(begin
       (define ,def ,@body)
       )))

;; find out which symbols a plugin defines.
(define (lookup-exports name)
  (let ((symbols (ref (hash-table-get *plugins* name) 'export))
        (modules (cons *sandbox-plugin* (all-modules))))
    (map (lambda (s)
           (cons s
                 (or (find (lambda (m)
                             (hash-table-exists? (module-table m) s))
                           modules)
                     (error "symbol not found." name s))))
         symbols)))

;; find symbol then define in sandbox plugin module.
(define-macro (expand-define name module)
  (let ((exports (lookup-exports name)))
    `(begin
       ,@(map (lambda (e)
                `(define-in-module ,module
                   ,(car e)
                   (eval ,(car e) ,(cdr e))))
              exports)
       )))

;; safe plugin loader for sandbox.
(define-syntax load-plugin
  (syntax-rules ()
    ((load-plugin file)
     (%load-plugin file (current-module)))))

(define-macro (use-plugin name)
  `(load-plugin (symbol->string ',name)))

(define (%load-plugin name target-module)
  (eval `(expand-define ,name ,target-module) *sandbox-plugin*))

(define (register-plugin name version export depend)
  (hash-table-put! *plugins*
                   name
                   (make <kahua-plugin>
                     :name name
                     :version version
                     :export export
                     :depend depend)))

;; plugin registrar.
(define-syntax define-plugin
  (syntax-rules (version export require)
    ((define-plugin name
       (version v)
       (export symbol1 symbol2 ...)
       (depend module1 module2 ...))
       (register-plugin name v
                        '(symbol1 symbol2 ...) '(module1 module2 ...))
     )))


;; plugin registrar for gauche module.
(define-macro (allow-module module)
  `(begin
     (use ,module)
     (register-plugin (symbol->string ',module) "1.0"
                      (module-exports (find-module ',module)) ())
     )
  )

;; load all plugin file.
(define (initialize-plugins)
  (let* ((plugin-dir (build-path (ref (kahua-config) 'working-directory)
                                 "plugins"))
         (plugin-files (directory-list plugin-dir 
                        :filter (lambda (n) (string-suffix? ".scm" n)))))
    (set! *plugins* (make-hash-table 'string=?))
    ;; make new module.
    (set! *sandbox-plugin* (make-sandbox-plugin))
    (for-each refresh-plugin plugin-files)))

;; refresh a target plugin.
(define (refresh-plugin filename)
  (let* ((workdir (ref (kahua-config) 'working-directory))
         (plugin (if (absolute-path? workdir)
                   (build-path workdir "plugins" filename)
                   (build-path (current-directory) workdir
                               "plugins" filename))))
    (load plugin :environment *sandbox-plugin*)))

(define (all-plugins)
  (hash-table-map *plugins* (lambda (name p) (cons name p))))


(provide "kahua/plugin")
