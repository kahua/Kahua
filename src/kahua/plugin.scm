;; Kahua plugin manager
;;
;;  Copyright (c) 2003-2007 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;

(define-module kahua.plugin
  (use srfi-1)
  (use srfi-13)
  (use file.util)
  (export define-export <kahua-plugin> lookup-exports
          expand-define %load-plugin use-plugin
          define-plugin allow-module register-plugin
          initialize-plugins refresh-plugin
          all-plugins)
  )

(select-module kahua.plugin)

;; plugin meta-information container
(define-constant *plugins* (make-hash-table 'string=?))

;; plugin's procedures are bound this module.
(define-constant *sandbox-plugin* (make-hash-table 'eq?))

;; make anonymous module for binding plugin procedures.
(define (make-sandbox-module)
  (let ((m (make-module #f)))
    (eval '(import kahua.plugin) m)
    m))

(define (get-sandbox-module name)
  (if (hash-table-exists? *sandbox-plugin* name)
      (hash-table-get *sandbox-plugin* name)
      (let ((m (make-sandbox-module)))
        (hash-table-put! *sandbox-plugin* name m)
        m)))

;; plugin class.
(define-class <kahua-plugin> ()
  (
   (name :init-keyword :name)
   (version :init-keyword :version)
   (export :init-keyword  :export)
   (depend :init-keyword  :depend)
   ))

;; explicitly. define a procedue globally in somewhere,
;; normally in *sandbox-plugin*.
(define-macro (define-export def . body)
  (if (pair? def)
    `(define-export ,(car def) (lambda ,(cdr def) ,@body))
    `(begin
       (define ,def ,@body)
       )))

;; get symbols which exported from module,
;; this procedure treats the case of 'export-all'd module.
(define (%get-export-symbols name)
  (let1 m (filter (lambda (m)
                    (eq? (module-name m) name))
                  (all-modules))
    (if (null? m)
        '()
        (hash-table-map
         (module-table (car m))
         (lambda (k v) k)))))

;; find out which symbols a plugin defines.
(define (lookup-exports name)
  (let ((symbols (ref (hash-table-get *plugins*
                                      (symbol->string name)) 'export))
        (modules (cons (get-sandbox-module name)
                       (append (filter (lambda (m) (eq? (module-name m) name))
                                       (all-modules))
                               (hash-table-values *sandbox-plugin*)
                               (all-modules)))))
    (map (lambda (s)
           (cons s
                 (or (find (lambda (m)
                             (hash-table-exists? (module-table m) s))
                           modules)
                     (error "symbol not found." name s))))
         (if (eq? symbols #t)
             (%get-export-symbols name)
             symbols))))

;; find symbol then define in sandbox plugin module.
(define-macro (expand-define name module)
  (let ((exports (lookup-exports name)))
    `(begin
       ,@(map (lambda (e)
                `(define-in-module ,module
                   ,(car e)
                   (eval ',(car e) ,(cdr e))))
              exports)
       )))

;; safe plugin loader for sandbox.
(define-syntax use-plugin
  (syntax-rules ()
    ((use-plugin name)
     (if (hash-table-exists? *plugins* (symbol->string 'name))
         (%load-plugin 'name (current-module))
         (error "cannot find plugin" 'name)))))

(define (%load-plugin name target-module)
  (eval `(expand-define ,name ,target-module)
        (get-sandbox-module name)))

(define (register-plugin name version export depend)
  (hash-table-put! *plugins*
                   name
                   (make <kahua-plugin>
                     :name name
                     :version version
                     :export export
                     :depend depend))
  (hash-table-put! *sandbox-plugin*
                   (string->symbol name)
                   (get-sandbox-module
                    (string->symbol (port-name (current-load-port))))))

;; plugin registrar.
(define-syntax define-plugin
  (syntax-rules (version export depend)
    ;; for backward compatibility
    ((_ name
        (version v)
        (export symbol1 symbol2 ...)
        (depend #f))
     (define-plugin name (version v) (export symbol1 symbol2 ...)))
    ((_ name
        (version v)
        (export symbol1 symbol2 ...))
     (register-plugin name v '(symbol1 symbol2 ...) '()))
    ((_ name
        (version v)
        (export symbol1 symbol2 ...)
        (depend module1 ...))
     (register-plugin name v '(symbol1 symbol2 ...) '(module1 ...)))
    ((_ arg1 ...)
     (syntax-error "malformed syntax: " (define-plugin arg1 ...)))
    ))

;; plugin registrar for gauche module.
(define-syntax allow-module
  (syntax-rules ()
    ((allow-module module)
     (let ((m (get-sandbox-module 'module)))
       (eval '(use module) m)
       (eval '(register-plugin (symbol->string 'module) "1.0"
                               (module-exports (find-module 'module)) ())
             m)
       ))))

;; load all plugin file.
(define (initialize-plugins plugin-dir)
  (let1 plugin-files (directory-list plugin-dir :filter (lambda (n) (string-suffix? ".scm" n)))
    (hash-table-clear! *plugins*)
    (hash-table-clear! *sandbox-plugin*)
    (for-each (cut refresh-plugin <> plugin-dir) plugin-files)))

;; refresh a target plugin.
(define (refresh-plugin filename dir)
  (let1 plugin (sys-normalize-pathname (build-path dir filename) :absolute #t)
    (load plugin :environment (get-sandbox-module (string->symbol plugin)))))

(define (all-plugins)
  (hash-table-map *plugins* (lambda (name p) (cons name (ref p 'version)))))


(provide "kahua/plugin")
