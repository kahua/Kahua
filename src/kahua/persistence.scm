;; Persistent metaclass
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: persistence.scm,v 1.5 2004/02/09 10:36:43 shiro Exp $

(define-module kahua.persistence
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use file.util)
  (use util.list)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.version)
  (use gauche.fcntl)
  (use gauche.logger)
  (export <kahua-persistent-meta> <kahua-persistent-base>
          key-of find-kahua-class find-kahua-instance
          <kahua-db> current-db with-db kahua-db-sync
          id->kahua-instance class&key->kahua-instance
          <kahua-collection> make-kahua-collection
          )
  )
(select-module kahua.persistence)

;; Patch to fix 0.7.2 definiciency
(define slot-set-using-accessor!
  (if (version<=? (gauche-version) "0.7.2")
    slot-set-using-accessor
    slot-set-using-accessor!))
;; End patch

;;=========================================================
;; Persistent metaclass
;;

;; Special allocation options:
;;
;;   :persistent
;;       - The slot is stored in the persistent db.
;;

(define-class <kahua-persistent-meta> (<class>)
  (;; In-memory catalog of persistent classes.
   ;; maps class name to class object.
   (class-alist :allocation :class :init-value '())

   ;; A signature given in the class definition in the source file
   ;; to be used to determine class' generation.
   (source-id   :init-keyword :source-id :init-value "")
   
   ;; The following metainformation is set up when a first instance of
   ;; the class is realized (either read from DB or newly created)
   ;; See the "Persistent metainformation" section below for the details.
   (generation    :init-value 0)   ;; generation of in-memory class
   (persistent-generation :init-value #f) ;; generation in persistent db
   ))

(define-method make ((class <kahua-persistent-meta>) . initargs)
  ;; When the first instance of a persistent class is realized (created
  ;; in memory), the in-memory definition and in-db definition is compared.
  (unless (ref class 'persistent-generation)
    (persistent-class-bind-metainfo class))
  (next-method))

(define-method initialize ((class <kahua-persistent-meta>) initargs)
  (next-method)
  (set! (ref class 'class-alist)
        (assq-set! (ref class 'class-alist) (class-name class) class)))

(define-method compute-get-n-set ((class <kahua-persistent-meta>) slot)
  (let ((alloc (slot-definition-allocation slot)))
    (case alloc
      ((:persistent)
       (let* ((slot-num (slot-ref class 'num-instance-slots))
              (acc (make <slot-accessor>
                     :class class :name (slot-definition-name slot)
                     :slot-number slot-num :initializable #t)))
         (inc! (slot-ref class 'num-instance-slots))
         (list (make-kahua-getter acc)
               (make-kahua-setter acc)
               (make-kahua-boundp acc)
               #t)))
      (else (next-method)))))

(define (make-kahua-getter acc)
  (lambda (o)
    (let1 val (slot-ref-using-accessor o acc)
      (if (is-a? val <kahua-wrapper>)
        (let1 real (peel-wrapper val)
          (slot-set-using-accessor! o acc real)
          real)
        val))))

(define (make-kahua-setter acc)
  (lambda (o v)
    (let1 db (current-db)
      (unless db (error "database not active"))
      (unless (memq o (ref db 'modified-instances))
        (push! (ref db 'modified-instances) o))
      (slot-set-using-accessor! o acc v))))

(define (make-kahua-boundp acc)
  (lambda (o)
    (slot-bound-using-accessor? o acc)))

;;=========================================================
;; Persistent baseclass
;;

(define-class <kahua-persistent-base> ()
  (;; unique ID 
   (id    :init-keyword :id :init-form (kahua-db-unique-id))
   ;; management data
   (db    :init-form (current-db))  ; points back to db
   )
  :metaclass <kahua-persistent-meta>)

;; this method should be overriden by subclasses for convenience.
(define-method key-of ((obj <kahua-persistent-base>))
  (format "~6,'0d" (ref obj 'id)))

;; NB: a special initialization protocol is used when an instance
;; is realized from the persistent storage.  We need the slot values
;; to be recovered _before_ calling key-of to initialize instance-by-key
;; table, since key-of may depend on them.  So we can't slot-set!
;; _after_ initialize.  In order to solve this, the realization routine
;; calls 'make' with a special initialization keyword,
;; :realization-slot-values, which is an alist of slot values.
;; This method gets that and recovers slot values before registering
;; the instance to the table.
(define-method initialize ((obj <kahua-persistent-base>) initargs)
  (next-method)
  (let ((db (current-db))
        (id (ref obj 'id))
        (rsv (get-keyword :realization-slot-values initargs #f)))
    (when (hash-table-get (ref db 'instance-by-id) id #f)
      (errorf "instance with same ID (~s) is active (class ~s)"
              id (class-of obj)))
    (when rsv
      ;; we are realizing an instance from the saved one
      (dolist (p rsv)
        (when (assq (car p) (class-slots (class-of obj)))
          (slot-set! obj (car p) (make <kahua-wrapper> :value (cdr p)))))
      ;; obj is marked dirty because of the above setup.
      ;; we revert it to clean.  it is ugly, but it's the easiest
      ;; way to prevent obj from being marked dirty inadvertently.
      (update! (ref db 'modified-instances) (cut delete obj <>)))
    (hash-table-put! (ref db 'instance-by-id) id obj)
    (hash-table-put! (ref db 'instance-by-key)
                     (cons (class-name (class-of obj))
                           (key-of obj))
                     obj)
    ))

(define (find-kahua-class name)
  (or (assq-ref (class-slot-ref <kahua-persistent-meta> 'class-alist) name)
      (error "can't find a class: " name)))

;; kahua-wrapper is used to mark a slot value that has been just
;; read from the disk, and may contain <kahua-proxy> reference.
;; Once the slot is accessed, the getter traverses its value
;; and replaces <kahua-proxy> references, and reset the slot value
;; for the replaced one---thus the user never see <kahua-wrapper> value.
(define-class <kahua-wrapper> ()
  ((value :init-keyword :value)))

(define-method peel-wrapper ((wrapper <kahua-wrapper>))
  ;; this is inefficient, but for now...
  (define (realize-proxy val)
    (cond ((is-a? val <kahua-proxy>)
           (realize-kahua-proxy val))
          ((list? val)
           (map realize-proxy val))
          ((vector? val)
           (map-to <vector> realize-proxy val))
          (else val)))
  (realize-proxy (ref wrapper 'value)))

(define-class <kahua-proxy> ()
  ((class :init-keyword :class)
   (key   :init-keyword :key)
   ))

(define-method realize-kahua-proxy ((proxy <kahua-proxy>))
  (let1 db (current-db)
    (unless db (error "database not active"))
    (or (hash-table-get (ref db 'instance-by-key)
                        (cons (class-name (ref proxy 'class))
                              (ref proxy 'key))
                        #f)
        (read-kahua-instance db (ref proxy 'class) (ref proxy 'key)))))

;; for now, we don't consider shared structure
(define-method kahua-write ((obj <kahua-persistent-base>) port)
  (define (save-slot s)
    (format #t "(~a . " (slot-definition-name s))
    (serialize-value (slot-ref obj (slot-definition-name s)))
    (format #t ")\n"))
  (with-output-to-port port
    (lambda ()
      (format #t "#,(kahua-object ~a ~a\n"
              (class-name (class-of obj)) (ref obj 'id))
      (for-each (lambda (s)
                  (when (eq? (slot-definition-allocation s) :persistent)
                    (save-slot s)))
                (class-slots (class-of obj)))
      (display ")\n"))))

(define (serialize-value v)
  (cond
   ((any (cut is-a? v <>)
         (list <boolean> <number> <string> <symbol> <keyword>))
    (write v) (display " "))
   ((null? v) (display "()"))
   ((pair? v)
    (display "(")
    (let loop ((v v))
      (cond ((null? v))
            ((pair? v) (serialize-value (car v)) (loop (cdr v)))
            (else (display " . ") (serialize-value v))))
    (display ")"))
   ((vector? v)
    (display "#(")
    (for-each serialize-value v)
    (display ")"))
   ((is-a? v <kahua-persistent-base>)
    (format #t "#,(kahua-proxy ~a ~s)"
            (class-name (class-of v)) (key-of v)))
   ((is-a? v <kahua-proxy>)
    (format #t "#,(kahua-proxy ~a ~s)"
            (class-name (ref v 'class)) (ref v 'key)))
   ((is-a? v <kahua-wrapper>)
    (serialize-value (ref v 'value)))
   (else
    (error "object not serializable:" v))))

(define-reader-ctor 'kahua-object
  (lambda (class-name id . vals)
    (or (id->kahua-instance id)
        ;; See notes on (initialize (<kahua-persistent-base>)) for
        ;; the :realization-slot-values argument.
        (make (find-kahua-class class-name) :id id
              :realization-slot-values vals))))

(define-reader-ctor 'kahua-proxy
  (lambda (class-name key)
    (make <kahua-proxy> :class (find-kahua-class class-name) :key key)))

;;=========================================================
;; Persistent metainformation
;;

;; Information of persitent class metaobject is stored using
;; a special persistent class, <kahua-persistent-metainfo>.
;;
;; A persistent class has "in-memory" representation and "in-db"
;; representation.
;;
;; At the beginning, you don't have any instance in the database,
;; nor the "in-db" class.  You only have "in-memory" class that
;; is read from the source code.
;;
;; When the first persistent instance is created, a metainfo structure
;; that records the class information is also created, and stored
;; in the database.  That becomes "in-db" class representation.
;;
;; When you access to the db second time, the metainfo structure is
;; read from the db and compared to the "in-memory" class.  If 
;; two matches, no problem---the schema hasn't been changed.
;;
;; If in-memory class and in-db class doesn't match, there may
;; be two cases.
;;
;;   1. The source code is updated, so in-memory class is newer than
;;      the in-db class.
;;   2. Some other process has used newer sources, and update the
;;      in-db class, while your process is still using older definition.
;;
;; If the case is 1, we'll update the in-db class, along with any
;; instance.  If the case is 2, we won't touch the in-db class, but
;; we update the in-memory class as if it is redefined (note that at
;; this point there's no instance in-memory, so the instance update
;; protocol is not triggered).

(define-class <kahua-persistent-metainfo> (<kahua-persistent-base>)
  (;; class name
   (name             :allocation :persistent :init-value #f
                     :init-keyword :name)
   ;; current generation & signature
   (generation       :allocation :persistent :init-value 0
                     :init-keyword :persistent-generation)
   (signature        :allocation :persistent :init-value '()
                     :init-keyword :signature)

   ;; The following slots keep the history of the class definitions.

   ;; Every time the class signature is changed, the generation number is
   ;; incremented and the old generation and sigature is pushed into
   ;; this alist.
   (signature-alist  :allocation :persistent :init-value '()
                     :init-keyword :signature-alist)
   
   ;; Furthermore, if the in-memory class is given a source-id, the
   ;; association of it and generation number is stored here.  It helps
   ;; to find out whether the in-memory class is older than in-db class, or
   ;; the newer source actually adopted the older definitions.
   ;; Source id can be any string, and it is possible that one source id
   ;; corresponds to multiple generations.
   (source-id-map    :allocation :persistent :init-value '()
                     :init-keyword :source-id-map)
   ))

(define-method key-of ((info <kahua-persistent-metainfo>))
  (x->string (ref info 'name)))

;; Calculates class signature.  Currently we take all persistent-allocated
;; slots.
(define-method persistent-class-signature ((class <kahua-persistent-meta>))
  (define (extract-slot-def slot)
    (and (memq (slot-definition-allocation slot) '(:persistent))
         (list (slot-definition-name slot)
               :allocation (slot-definition-allocation slot)
               )))
  (sort (filter-map extract-slot-def (class-slots class))
        (lambda (x y)
          (string<? (symbol->string (car x)) (symbol->string (car y))))))

;; When an instance of a persistent class is realized for the first
;; time within the process, the in-memory class and the in-DB class
;; are compared.
(define-method persistent-class-bind-metainfo ((class <kahua-persistent-meta>))

  ;; check if two signatures are the same
  (define (signature=? x y) (equal? x y))

  ;; set generation numbers
  (define (set-generation! in-mem in-db)
    (set! (ref class 'generation) in-mem)
    (set! (ref class 'persistent-generation) in-db))
  
  ;; this class is fresh; create new metainfo.
  (define (register-metainfo signature)
    (let1 info (make <kahua-persistent-metainfo>
                 :name (class-name class)
                 :generation 0
                 :signature signature
                 :source-id-map `((,(ref class 'source-id) 0))
                 :signature-alist `())
      (kahua-db-sync) ;;XXX do we need this?
      (set-generation! 0 0)
      info))

  ;; signature doesn't match.  see if the in-memory class is older than
  ;; the in-db class, and if so, returns the (generation . signature)
  (define (find-generation metainfo signature)
    (and-let* ((generations (assoc-ref (ref metainfo 'source-id-map)
                                       (ref class 'source-id))))
      (find (lambda (gen&sig)
              (signature=? (cdr gen&sig) signature))
            (filter-map (cut assv <> (ref metainfo 'signature-alist))
                        generations))))

  ;; we couldn't find matching signature, so we assume the in-memory
  ;; class is newer.  record the newer class signature.
  (define (increment-generation metainfo signature)
    (let ((newgen (+ (ref metainfo 'generation) 1)))
      (slot-push! metainfo 'signature-alist
                  (cons (ref metainfo 'generation)
                        (ref metainfo 'signature)))
      (set! (ref metainfo 'generation) newgen)
      (set! (ref metainfo 'signature) signature)
      (record-source-id metainfo newgen)
      (set-generation! newgen newgen)))

  ;; subfunction of increment-generation
  (define (record-source-id metainfo new-generation)
    (let* ((source-id (ref class 'source-id))
           (p (assoc source-id (ref metainfo 'source-id-map))))
      (if p
        (begin
          (set-cdr! p (cons new-generation (cdr p)))
          ;; touch the slot, so that it is reflected to db later.
          (set! (ref metainfo 'source-id-map)
                (ref metainfo 'source-id-map)))
        (push! (ref metainfo 'source-id-map)
               (list source-id new-generation)))))

  (or (ref class 'persistent-generation)
      (eq? (class-name class) '<kahua-persistent-metainfo>)
      (let ((metainfo (find-kahua-instance <kahua-persistent-metainfo>
                                           (x->string (class-name class))))
            (signature (persistent-class-signature class)))
        (cond ((not metainfo) (register-metainfo signature))
              ((signature=? (ref metainfo 'signature) signature)
               ;; in-memory class is up to date.
               (set-generation! (ref metainfo 'generation)
                                (ref metainfo 'generation)))
              ((find-generation metainfo signature)
               => (lambda (gen&sig)
                    (set-generation! (car gen&sig) (ref metainfo 'generation))
                    ))
              (else
               (increment-generation metainfo signature)))))
  )

;;=========================================================
;; Database
;;

;; Database class -----------------------------------------
(define current-db (make-parameter #f))

(define-class <kahua-db> ()
  ((path       :init-keyword :path :init-value #f)
   (id-counter :init-keyword :id-counter :init-value 0)
   (active     :init-keyword :active :init-value #f)
   (lock-port  :init-value #f) ;; port opened on lock file
   (instance-by-id  :init-form (make-hash-table 'eqv?))
   (instance-by-key :init-form (make-hash-table 'equal?))
   (modified-instances :init-form '())
   ))

(define-method write-object ((obj <kahua-db>) port)
  (format port "#<kahua-db ~s (~a)>"
          (ref obj 'path)
          (if (ref obj 'active) "active" "inactive")))

(define (kahua-db-unique-id)
  (unless (current-db)
    (error "kahua-db-unique-id: No db is active"))
  (begin0 (ref (current-db) 'id-counter)
          (inc! (ref (current-db) 'id-counter))))

(define-syntax with-db
  (syntax-rules ()
    ((with-db (db dbpath) . body)
     (if (and (current-db)
              (ref (current-db) 'active)
              (equal? (ref (current-db) 'path) dbpath))
       (begin . body)
       (let ((db (kahua-db-open dbpath)))
         (parameterize ((current-db db))
           (with-error-handler
               (lambda (e) (kahua-db-close db #f) (raise e))
             (lambda ()
               (begin0 (begin . body) (kahua-db-close db #t))))))))))

(define (id-counter-path path)
  (build-path path "id-counter"))

;; lock mechanism - we need more robust one later, but just for now...
(define (lock-file-path path)
  (build-path path "lock"))

(define (lock-db db)
  (let1 lock-file (lock-file-path (ref db 'path))
    (unless (file-exists? lock-file)
      ;; This is an old db.  This is only transitional, and may
      ;; be called very rarely, so we just leave this though unsafe.
      (with-output-to-file lock-file (lambda () (newline))))
    (let ((record    (make <sys-flock> :type F_WRLCK))
          (lock-port (open-output-file lock-file :if-exists? :append)))
      (define (try-lock retry)
        (cond ((zero? retry) #f)
              ((sys-fcntl lock-port F_SETLK record)
               (set! (ref db 'lock-port) lock-port) #t)
              (else (try-lock (- retry 1)))))
      (try-lock 10))))

(define (unlock-db db)
  (and-let* ((lock-port (ref db 'lock-port))
             (record (make <sys-flock> :type F_UNLCK)))
    (sys-fcntl lock-port F_SETLK record)
    #t))

(define (kahua-db-open path)
  (let ((cntfile (id-counter-path path)))
    (if (file-is-directory? path)
      (if (file-is-regular? cntfile)
        (let1 db 
            (make <kahua-db>
              :path path :active #t
              :id-counter (with-input-from-file cntfile read))
          (unless (lock-db db)
            (error "kahua-db-open: couldn't obtain database lock: " path))
          db)
        (error "kahua-db-open: path is not a db: " path))
      (begin
        ;; There could be a race condition here, but it would be very
        ;; low prob., so for now it should be OK.
        (make-directory* path)
        (make-directory* (build-path path "tmp"))
        (with-output-to-file cntfile (cut write 0))
        (let1 db (make <kahua-db> :active #t :path path)
          (unless (lock-db db)
            (error "kahua-db-open: couldn't obtain database lock: " path))
          db))
      )))

(define (kahua-db-sync . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (for-each (cut write-kahua-instance db <>)
              (ref db 'modified-instances))
    (set! (ref db 'modified-instances) '())))

(define (kahua-db-close db commit)
  (when commit
    (with-output-to-file (id-counter-path (ref db 'path))
      (cut write (ref db 'id-counter)))
    (kahua-db-sync db))
  (unlock-db db)
  (set! (ref db 'modified-instances) '())
  (set! (ref db 'active) #f))

(define (data-path db class . key)
  (apply build-path
         (ref db 'path)
         (string-trim-both (x->string (class-name class)) #[<>])
         key))

;; Instance I/O ----------------------------------------------

(define (id->kahua-instance id)
  (unless (current-db)
    (error "id->kahua-instance: No db is active"))
  (hash-table-get (ref (current-db) 'instance-by-id) id #f))

(define (class&key->kahua-instance class key)
  (unless (current-db)
    (error "clas&key->kahua-instance: No db is active"))
  (hash-table-get (ref (current-db) 'instance-by-key)
                  (cons (class-name class) key) #f))

(define (find-kahua-instance class key)
  (let1 db (current-db)
    (unless db (error "find-kahua-instance: No database is active"))
    (or (class&key->kahua-instance class key)
        (and (file-exists? (data-path db class key))
             (read-kahua-instance db class key)))))

(define-method read-kahua-instance ((db <kahua-db>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (call-with-input-file (data-path db class key) read))

(define-method write-kahua-instance ((db <kahua-db>)
                                     (obj <kahua-persistent-base>))
  (let* ((path  (data-path db (class-of obj) (key-of obj))))
    (make-directory* (sys-dirname path))
    (receive (p tmp) (sys-mkstemp (build-path (ref db 'path) "tmp"))
      (with-error-handler
          (lambda (e) (sys-unlink tmp) (raise e))
        (lambda ()
          (kahua-write obj p)
          (close-output-port p)
          (sys-rename tmp path))))))

;;=========================================================
;; "View" as a collection
;;

;; NB: right now, we read all the instances into memory at once.
;; Later we can change the back-end to read it lazily.

(define-class <kahua-collection> (<collection>)
  ((instances :init-keyword :instances :init-value '()))
  )

(define-method make-kahua-collection ((class <kahua-persistent-meta>) . opts)
  (let1 db (current-db)
    (unless db (error "make-kahua-collection: database not active"))
    (make <kahua-collection>
      :instances (map (cut find-kahua-instance class <>)
                      (if (file-is-directory? (data-path db class))
                        (directory-list (data-path db class) :children? #t)
                        '())))))

(define-method call-with-iterator ((coll <kahua-collection>) proc . opts)
  (let1 p (ref coll 'instances)
    (proc (cut null? p)
          (lambda () (let1 r (car p) (pop! p) r)))))
    
(provide "kahua/persistence")
