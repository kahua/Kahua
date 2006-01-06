;; Persistent metaclass
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: persistence.scm,v 1.36 2006/01/06 14:34:56 shibata Exp $

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
  (use gauche.collection)
  (export <kahua-persistent-meta> <kahua-persistent-base>
          <kahua-persistent-metainfo>
          key-of find-kahua-class find-kahua-instance
          touch-kahua-instance!
          kahua-serializable-object?
          kahua-persistent-classes-in-db
          kahua-persistent-class-generation
          kahua-persistent-class-definition
          <kahua-db> <kahua-db-fs> <kahua-db-dbi>
          <with-db-error>
          current-db with-db kahua-db-sync kahua-db-purge-objs
          id->kahua-instance class&key->kahua-instance
          <kahua-collection> make-kahua-collection
          raise-with-db-error
          persistent-initialize
          kahua-wrapper?
          )
  )
(select-module kahua.persistence)

;;=========================================================
;; Persistent metaclass
;;

;; <kahua-persistent-meta>
;;
;;  - Processes :persistent allocation option.
;;  - Keeps generation of persistent class.
;;  - Keeps mapping from class name to the in-memory class.

;; Cache Consistency Management
;;  - :out-of-transaction :read-only(default), :read/write, :denied
;;  - :write-sync         :error, :auto, handler procedure, :ignore
;;  - :read-sync          :error, :auto, handler procedure, :ignore

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
   (metainfo      :init-value #f)  ;; <kahua-persistent-metainfo> instance
   (generation    :init-value 0)   ;; generation of in-memory class
   (persistent-generation :init-value #f) ;; generation in persistent db
   (write-syncer  :init-value :ignore
                  :init-keyword :write-syncer)
   (read-syncer   :init-value :auto
                  :init-keyword :read-syncer)
   ))

(define-method make ((class <kahua-persistent-meta>) . initargs)
  ;; When the first instance of a persistent class is realized (created
  ;; in memory), the in-memory definition and in-db definition is compared.
  (ensure-metainfo class)
  (next-method))

(define (kahua-read-syncer in-memory in-db-slots)
  ;; we shuold update transaction-id of in-memory object at last,
  ;; but to avoid inifinite loop, we update it here.
  (let1 class (class-of in-memory)
    (for-each (lambda (slot)
                (slot-set-using-class! class in-memory
                                       (car slot)
                                       (make <kahua-wrapper> :value (cdr slot))))
              in-db-slots)))

; (define (kahua-write-syncer in-memory in-db-slots)
;   (let* ((db    (current-db))
;          (id    (ref in-memory 'id))
;          (class (class-of in-memory))
;          (key   (key-of in-memory))
;          (in-db (read-on-the-fly (cut read-kahua-instance db class key))))
;     (define (check-consistency)
;       (every (lambda (cache)
;                (let ((slot           (car cache))
;                      (in-transaction (cdr cache)))
;                  (equal? (ref in-db slot) in-transaction)))
;              (ref in-memory '%in-transaction-cache)))

;     (define (non-touched-slots)
;       (let1 cache (ref in-memory '%in-transaction-cache)
;         (remove (cut assq <> cache) (persistent-slot-syms class))))
    
;     (define (sync-non-touched-slots)
;       (for-each (lambda (slot)
;                   (set! (ref in-memory slot) (ref in-db slot)))
;                 (non-touched-slots)))
                  
;     (unless (check-consistency)
;       (errorf "write syncer failed ~S ~S" in-memory in-db))

;     (sync-non-touched-slots)

;     (touch-kahua-instance! in-memory)

;     (update-transaction! in-memory)
;     (set! (ref in-memory '%in-transaction-cache) '())))

(define-method initialize ((class <kahua-persistent-meta>) initargs)
  (define (make-syncer default v)
    (cond ((eq? v :error)
           (lambda _ (error "object was obtained in other transaction")))
          ((eq? v :auto) default)
          ((procedure? v) v)
          (else (lambda _ #f))))

  (next-method)

   (update! (ref class 'read-syncer)  (pa$ make-syncer kahua-read-syncer))
;   (update! (ref class 'write-syncer) (pa$ make-syncer kahua-write-syncer))
  
  (set! (ref class 'class-alist)
        (assq-set! (ref class 'class-alist) (class-name class) class)))

;; Support of persistent slot
(define-method compute-get-n-set ((class <kahua-persistent-meta>) slot)
  (define (delete-slot-definition-allocation slot)
    (cons (car slot)
          (delete-keyword :allocation (cdr slot))))

  (let ((alloc (slot-definition-allocation slot)))
    (case alloc
      ((:persistent)
       (let* ((slot-num (slot-ref class 'num-instance-slots))
              (acc (let1 slot (delete-slot-definition-allocation slot)
                     (compute-slot-accessor class slot (next-method class slot)))))
         (inc! (slot-ref class 'num-instance-slots))
         (list (make-kahua-getter acc class slot)
               (make-kahua-setter acc slot)
               (make-kahua-boundp acc)
               #t)))
      (else (next-method)))))

(define (make-kahua-getter acc class slot)
  (let ((aot (slot-definition-option slot :out-of-transaction :read-only)))
    (lambda (o)
      (if (current-db)
          (ensure-transaction o)
          (when (eq? aot :denied)
            (error "database not active")))
      
      (let1 val (slot-ref-using-accessor o acc)
        (if (is-a? val <kahua-wrapper>)
            (let1 real (peel-wrapper val)
              (slot-set-using-accessor! o acc real)
              real)
            val)))))

(define (make-kahua-setter acc slot)
  (let ((aot       (slot-definition-option slot :out-of-transaction :read-only))
        (slot-name (car slot)))
    (lambda (o v)
      (let1 db (current-db)
        (if db
            (begin
              (ensure-transaction o)
              (unless (memq o (ref db 'modified-instances))
                (push! (ref db 'modified-instances) o))
              (slot-set-using-accessor! o acc v))
          (if (eq? aot :read/write)
              (begin
                (unless (assq slot-name (ref o '%in-transaction-cache))
                  (push! (ref o '%in-transaction-cache)
                         (cons slot-name (slot-ref-using-accessor o acc))))
                (floted-instance-touch! o)
                (slot-set-using-accessor! o acc v))
            (error "database not active")))))))

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
   ;; alist of slot data which is in the DB but not in the current
   ;; in-memory class.
   (%hidden-slot-values :init-keyword :%hidden-slot-values :init-value '())
   ;; persistent class generation of the in-db instance
   (%persistent-generation :init-keyword :%persistent-generation :init-value 0)
   ;; flag if this instance exists only in memory, or has been stored in db
   (%floating-instance :init-keyword :%floating-instance :init-value #t)
   ;; transaction id
   (%transaction-id :init-value -1)
   ;; alist of slot data which keeps slot values obtained at
   ;; a transaction indicated by %trancation-id.
   ;; it to be used to check in-db / in-memory consistency.
   (%in-transaction-cache :init-value '())
   )
  :metaclass <kahua-persistent-meta>)

;; this method should be overriden by subclasses for convenience.
(define-method key-of ((obj <kahua-persistent-base>))
  (define (id->idstr num)
    (let* ((ml  6)
	   (str (number->string num))
	   (pad (- ml (string-length str))))
      (string-append
       (make-string (if (< pad 0) 0 pad) #\0)
       str)))
  (id->idstr (slot-ref-using-class (current-class-of obj) obj 'id)))

;; Initializing persistent instance:
;;  * (initialize (obj <kahua-persistent-base>)) initializes in-memory
;;   image of persistent object, and registered it to the index table
;;   of the current database.
;;  * To calculate index, we need to retrieve key, so the slot values
;;   have to be set before that.  We can't count on initargs, since
;;   a persistent slot may not have init-keyword.  So, the persistent
;;   object realization routine passes the alist of slot values via
;;   :%realization-slot-values keyword argument, and we set! the slot
;;   values.
;;  * The object is an instance of in-memory persistent class.  It may
;;   be different from in-db persistent class.  The persistent object
;;   deserializer translates the in-db slot values to in-memory slot
;;   values before passing it to :%realization-slot-values argument.
;; *  When new persistent object is created, call persistent-initialize
;;   method to initialize object.

(define-method persistent-initialize ((obj <kahua-persistent-base>) initargs)
  #f)

(define-method initialize ((obj <kahua-persistent-base>) initargs)
  (next-method)
  (let ((db (current-db))
        (id (ref obj 'id))
        (rsv (get-keyword :%realization-slot-values initargs #f)))
    (when (id->kahua-instance id)
      (errorf "instance with same ID (~s) is active (class ~s)"
              id (class-of obj)))
    
    (update-transaction! obj)
    
    (if rsv
        (begin
          ;; we are realizing an instance from the saved one
          (dolist (p rsv)
            (when (assq (car p) (class-slots (class-of obj)))
              ;; loop!
              (slot-set! obj (car p) (make <kahua-wrapper> :value (cdr p)))))
          ;; obj is marked dirty because of the above setup.
          ;; we revert it to clean.  it is ugly, but it's the easiest
          ;; way to prevent obj from being marked dirty inadvertently.
          (update! (ref db 'modified-instances) (cut delete obj <>)))
      ;; initialize persistent object for the first time.
      (persistent-initialize obj initargs))

    (hash-table-put! (ref db 'instance-by-id) id obj)
    (hash-table-put! (ref db 'instance-by-key)
                     (cons (class-name (class-of obj))
                           (key-of obj))
                     obj)))

(define (find-kahua-class name)
  (or (assq-ref (class-slot-ref <kahua-persistent-meta> 'class-alist) name)
      (error "can't find a class: " name)))

;; Mark a persistent object dirty
(define-method touch-kahua-instance! ((obj <kahua-persistent-base>))
  (let1 db (ref obj 'db)
    (unless (ref db 'active)
      (error "database not active"))
    (unless (memq obj (ref db 'modified-instances))
      (push! (ref db 'modified-instances) obj))))

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

(define (kahua-wrapper? val)
  (is-a? val <kahua-wrapper>))

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
        (and-let* ((i (read-kahua-instance db (ref proxy 'class) (ref proxy 'key))))
          (set! (ref i '%floating-instance) #f)
          i)
        )))

;; The bottom-level writer ----------------------------------------
;;   write-kahua-instance calls kahua-write.

;; for now, we don't consider shared structure
(define-method kahua-write ((obj <kahua-persistent-base>) port)
  (define (save-slot s)
    (display "(")
    (display (car s))
    (display " . ")
    (serialize-value (cdr s))
    (display ")\n"))
  (with-output-to-port port
    (lambda ()
      (receive (generation vals hidden)
          (export-slot-values obj)
	(display "#,(kahua-object ( ")
	(display (class-name (class-of obj)))
	(display " ")
	(display generation)
	(display ") ")
	(display (ref obj 'id))
        (for-each save-slot
                  (if (null? hidden)
                    vals
                    (cons `(%hidden-slot-values . ,hidden) vals)))
        (display ")\n")))))

;; serialization
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
    (display "#,(kahua-proxy ")
    (display (class-name (class-of v)))
    (display " ")
    (write (key-of v))
    (display " )"))
   ((is-a? v <kahua-proxy>)
    (display "#,(kahua-proxy ")
    (display (class-name (ref v 'class)))
    (display " ")
    (write (ref v 'key))
    (display " )"))
   ((is-a? v <kahua-wrapper>)
    (serialize-value (ref v 'value)))
   (else
    (error "object not serializable:" v))))

(define (kahua-serializable-object? v)
  (or (any (cut is-a? v <>)
           (list <boolean> <number> <string> <symbol> <keyword> <null>
                 <kahua-persistent-base> <kahua-proxy> <kahua-wrapper>))
      (and (pair? v)
           (let loop ((v v))
             (cond ((null? v) #t)
                   ((pair? v)
                    (and (kahua-serializable-object? (car v))
                         (loop (cdr v))))
                   (else
                    (kahua-serializable-object? (cdr v))))))
      (and (vector? v)
           (let loop ((i (- (vector-length v) 1)))
             (if (negative? i)
               #t
               (and (kahua-serializable-object? (vector-ref v i))
                    (loop (- i 1))))))
      ))

;; The bottom-level reader ----------------------------------------
;;   read-kahua-instance will read srfi-10 syntax of
;;   kahua-object and kahua-proxy, which triggers the following
;;   procedures.

(define-reader-ctor 'kahua-object
  (lambda (class-desc id . vals)
    (let1 object (id->kahua-instance id)
      ;; See notes on (initialize (<kahua-persistent-base>)) for
      ;; the :%realization-slot-values argument.
      (let* ((cname (if (pair? class-desc) (car class-desc) class-desc))
             (class (find-kahua-class cname))
             (generation (find-instance-generation class class-desc)))
        (receive (slot-alist save-slots)
            (import-slot-values class generation vals)
          ;; when a in-memory object corresponding to id exisis,
          ;; we should sync in-memory in-db object.
          ;; Note:
          ;;  if a in-memory object is obtained at current transaction,
          ;;  read-kahua-instance dose not called. In other words, this
          ;;  reader macro dose not called.
          (if object
              (begin
                ((ref class 'read-syncer) object slot-alist)
                object)
              (begin
                (make class :id id
                      :%realization-slot-values slot-alist
                      :%hidden-slot-values save-slots
                      :%persistent-generation generation))
              ))))))

(define-reader-ctor 'kahua-proxy
  (lambda (class-name key)
    (make <kahua-proxy> :class (find-kahua-class class-name) :key key)))

(define (find-instance-generation class class-desc)
  (if (eq? (class-name class) '<kahua-persistent-metainfo>)
    0
    (begin
      (ensure-metainfo class)
      (if (pair? class-desc)
        (begin
          ;; The database may have been updated since the last bind-metainfo.
          (when (> (cadr class-desc) (ref class 'persistent-generation))
            (persistent-class-bind-metainfo class))
          (cadr class-desc))
        (ref class 'persistent-generation)))))

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
;;   2. Some other process has used newer sources, and updated the
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

   ;; For each generation (except the newest one), a "translation directive"
   ;; is prepared.  Each directive contains information to translate
   ;; an instance's slot value of the generation to the one of the next
   ;; generation, and vice versa.
   ;; The default translation directive is calculated by the difference
   ;; of persistent slots.  In future, an interface will be provided
   ;; to register an arbitrary translation procedures.
   (translator-alist :allocation :persistent :init-value '()
                     :init-keyword :translator-alist)
   ))

(define-method key-of ((info <kahua-persistent-metainfo>))
  (x->string (ref info 'name)))

(define-method ensure-metainfo ((class <kahua-persistent-meta>))
  (unless (ref class 'metainfo)
    (persistent-class-bind-metainfo class))
  (ref class 'metainfo))

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

;; Extracts persistent slot definitions from signature.  For the time
;; being, both are the same, but it may be changed later.
(define (signature->slot-definitions signature)
  (sort-slot-definitions signature))

(define (sort-slot-definitions sdefs)
  (sort sdefs (lambda (a b)
                (string<? (symbol->string (car a))
                          (symbol->string (car b))))))

;; Returns a list of persistent slot definitions of given generation.
(define-method persistent-class-slots ((class <kahua-persistent-meta>)
                                       (generation <integer>))
  (persistent-class-slots (ensure-metainfo class) generation))

(define-method persistent-class-slots ((metainfo <kahua-persistent-metainfo>)
                                       (generation <integer>))
  (cond ((= generation (ref metainfo 'generation))
         (signature->slot-definitions (ref metainfo 'signature)))
        ((assv-ref (ref metainfo 'signature-alist) generation)
         => signature->slot-definitions)
        (else
         (error "persistent-class-slots: class ~a doesn't have generation ~a"
                (ref metainfo 'name) generation))))

;; When an instance of a persistent class is realized for the first
;; time within the process, the in-memory class and the in-DB class
;; are compared.
(define-method persistent-class-bind-metainfo ((class <kahua-persistent-meta>))

  ;; check if two signatures are the same
  (define (signature=? x y) (equal? x y))

  ;; set generation numbers
  (define (set-generation! in-mem in-db metainfo)
    (set! (ref class 'metainfo) metainfo)
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
      (set-generation! 0 0 info)
      info))

  ;; signature doesn't match.  see if the in-memory class is older than
  ;; the in-db class, and if so, returns the (generation . signature)
  (define (find-generation metainfo signature)
    (and-let* ((idmap (ref metainfo 'source-id-map))
               (generations (assoc-ref idmap (ref class 'source-id))))
      (find (lambda (gen&sig)
              (signature=? (cdr gen&sig) signature))
            (filter-map (cut assv <> (ref metainfo 'signature-alist))
                        generations))))

  ;; we couldn't find matching signature, so we assume the in-memory
  ;; class is newer.  record the newer class signature.
  (define (increment-generation metainfo signature)
    (let* ((oldgen (ref metainfo 'generation))
           (newgen (+ oldgen 1))
           (oldsig (ref metainfo 'signature)))
      (slot-push! metainfo 'signature-alist (cons oldgen oldsig))
      (slot-push! metainfo 'translator-alist
                  (cons oldgen
                        (compute-translation-directive
                         (signature->slot-definitions oldsig)
                         (signature->slot-definitions signature))))
      (set! (ref metainfo 'generation) newgen)
      (set! (ref metainfo 'signature) signature)
      (record-source-id metainfo newgen)
      (set-generation! newgen newgen metainfo)))

  ;; register class' source-id to metainfo
  (define (record-source-id metainfo new-generation)
    (let* ((source-id (ref class 'source-id))
           (p (assoc source-id (ref metainfo 'source-id-map))))
      (if p
        (unless (memv new-generation (cdr p))
          (set-cdr! p (cons new-generation (cdr p)))
          ;; touch the slot, so that it is reflected to db later.
          (set! (ref metainfo 'source-id-map)
                (ref metainfo 'source-id-map)))
        (push! (ref metainfo 'source-id-map)
               (list source-id new-generation)))))

  ;; Main part of persistent-class-bind-metainfo
  (or (eq? (class-name class) '<kahua-persistent-metainfo>)
      (let ((metainfo (find-kahua-instance <kahua-persistent-metainfo>
                                           (x->string (class-name class))))
            (signature (persistent-class-signature class)))
        (cond ((not metainfo) (register-metainfo signature))
              ((signature=? (ref metainfo 'signature) signature)
               ;; in-memory class is up to date.
               (let1 gen (ref metainfo 'generation)
                 (record-source-id metainfo gen)
                 (set-generation! gen gen metainfo)))
              ((find-generation metainfo signature)
               => (lambda (gen&sig)
                    (set-generation! (car gen&sig)
                                     (ref metainfo 'generation)
                                     metainfo)))
              (else
               (increment-generation metainfo signature)))))
  )

;; Translating between in-db slot value alist and in-memory slots
;;
;;   export-slot-values - given in-memory object, extracts
;;     persistent slot values and translates it to fit the
;;     in-db class, and returns three values: the instance generation,
;;     alist of persistent slot values, and alist of saved (hidden)
;;     slot values.
;;
;;   import-slot-values - given alist of slot names & values which
;;     fits in in-db class, translates it to the in-memory slots,
;;     and returns two values: translated slot value alist and
;;     'saved' slot value alist, which contains the information that
;;     exists in-db but not in-memory.
;;
;; Case-by-case analysis of slot translation:
;;
;;   Importing (reading):
;;   * in-db version > in-memory version
;;     DB has been updated, but our process is running with
;;     older source.  If we see an added slot (a slot that exists in-db
;;     but not in-memory), we put it to the hidden-slot-values alist,
;;     so that the value will be kept.  If we see a removed slot
;;     (a slot that exists in-memory but not in-db), we may find the
;;     old value kept in the hidden-slot-values of in-db instance;
;;     otherwise, we leave it out to be initialized in a default way.
;;
;;   * in-memory version < in-db version
;;     We are running newer version.   If we see an added slot (a slot
;;     that exists in-memory but not in-db), we leave it missing so it
;;     is initialized by the default way.  If we see a deleted slot
;;     (a slot that exists in-db but not in-memory), we keep the value
;;     in the hidden-slot-values alist.
;;
;;   Exporting (writing):
;;   * in-db version > in-memory version
;;     If in-db version has added slot, whose value should have been
;;     stored in the hidden-slot-values alist, so we recover the value
;;     from it.
;;
;;   * in-memory version > in-db version
;;     We update in-db version.  The value stored in the hidden-slot-values
;;     are saved as is, so that other processes which uses older versions
;;     can still access the older values.

(define (export-slot-values obj)

  (define (default-slot-values)
    (filter-map (lambda (s)
                  (and (eq? (slot-definition-allocation s) :persistent)
                       (slot-bound? obj (slot-definition-name s))
                       (cons (slot-definition-name s)
                             (slot-ref obj (slot-definition-name s)))))
                (class-slots (class-of obj))))

  (let* ((class  (class-of obj))
         (c-gen  (ref class 'generation))
         (p-gen  (ref obj '%persistent-generation))
         (hidden (ref obj '%hidden-slot-values)))
    (if (<= p-gen c-gen)
      (values c-gen (default-slot-values) hidden)
      ;; db is newer.  we need to upgrade in-memory instance.
      (let loop ((gen c-gen)
                 (vals (default-slot-values))
                 (hidden hidden))
        (if (= p-gen gen)
          (values p-gen vals hidden)
          (receive (vals hidden) (translate-up class gen vals hidden)
            (loop (+ gen 1) vals hidden)))))
    ))

(define (import-slot-values class p-gen alist)
  (let ((hidden (assq-ref alist '%hidden-slot-values '()))
        (vals   (alist-delete '%hidden-slot-values alist)))
    (if (eq? (class-name class) '<kahua-persistent-metainfo>)
      (values vals hidden) ;; metainfo isn't managed by generations.
      (let ((c-gen (and (ensure-metainfo class)
                        (ref class 'generation))))

        (define (downgrade gen vals hidden)
          (if (= gen c-gen)
            (values vals hidden)
            (receive (vals hidden) (translate-down class gen vals hidden)
              (downgrade (- gen 1) vals hidden))))

        (define (upgrade gen vals hidden)
          (if (= gen c-gen)
            (values vals hidden)
            (receive (vals hidden) (translate-up class gen vals hidden)
              (upgrade (+ gen 1) vals hidden))))

        ;; Main part of import-slot-values
        (cond ((eqv? c-gen p-gen)
               (values vals hidden)) ;; we are up to date.
              ((< c-gen p-gen)
               (downgrade p-gen vals hidden))
              ((> c-gen p-gen)
               (upgrade p-gen vals hidden)))
        ))))

;; Given two slot list A and B, returns a list of translation directive.
;; <translation-directive> : (<slot-name> . <description>)
;; <description> : :drop   ;; slot is only in A (so it is dropped to make B)
;;               | :add    ;; slot is only in B (so it is added to make B)
;; Assumes both input slot list are sorted.
;; Slots common in both A and B aren't included in the directive list.

(define (compute-translation-directive A-slots B-slots)
  (let loop ((A-slots A-slots) (B-slots B-slots) (dirs '()))
    (cond ((null? A-slots)
           (append! dirs (map (lambda (s) (cons (car s) :add)) B-slots)))
          ((null? B-slots)
           (append! dirs (map (lambda (s) (cons (car s) :drop)) A-slots)))
          ((eq? (caar A-slots) (caar B-slots))
           (loop (cdr A-slots) (cdr B-slots) dirs))
          ((string<? (symbol->string (caar A-slots))
                     (symbol->string (caar B-slots)))
           (loop (cdr A-slots) B-slots (acons (caar A-slots) :drop dirs)))
          (else
           (loop A-slots (cdr B-slots) (acons (caar B-slots) :add dirs))))
    ))

;; Given translation directive and slot value alist, returns
;; a translated slot value alist and hidden slot value alist.
;; Which indicates the conversion direction; either a->b or b->a.
(define (translate-slot-alist directive alist hidden which)
  (let ((src (case which ((a->b) :drop) ((b->a) :add))))

    ;; slot dropped; move the slot value from r to h, if any.
    (define (drop-slot slot r h)
      (cond ((assq slot r) =>
             (lambda (p) (values (alist-delete (car p) r)
                                 (assq-set! h (car p) (cdr p)))))
            (else (values r h))))

    ;; slot added; move the slot value from h to r, if any.
    (define (add-slot slot r h)
      (cond ((assq slot h) =>
             (lambda (p) (values (assq-set! r (car p) (cdr p))
                                 (alist-delete (car p) h))))
            (else (values r h))))

    ;; loop over directive
    (define (translate dir r h)
      (cond ((null? dir)
             (values r h))
            ((eqv? (cdar dir) src)  ;; source dropped
             (receive (r h) (drop-slot (caar dir) r h)
               (translate (cdr dir) r h)))
            (else
             (receive (r h) (add-slot (caar dir) r h)
               (translate (cdr dir) r h)))))
    (translate directive alist hidden)))

(define (translate-up class generation alist hidden)
  (let* ((info (ensure-metainfo class))
         (dir (assq-ref (ref info 'translator-alist) generation)))
    (if (not dir)
      (begin
        ;;(warn "class translator doesn't exist: class ~a, generation ~a"
        ;;      (class-name class) generation)
        (values alist '()))
      (translate-slot-alist dir alist hidden 'a->b))))

(define (translate-down class generation alist hidden)
  (let* ((info (ensure-metainfo class))
         (dir (assq-ref (ref info 'translator-alist) (- generation 1))))
    (if (not dir)
      (begin
        ;;(warn "class translator doesn't exist: class ~a, generation ~a"
        ;;      (class-name class) (- generation 1))
        (values alist '()))
      (translate-slot-alist dir alist hidden 'b->a))))

;; Metainformation retrieval utilities:
;;
;; The following several APIs provide a way to query DB about
;; persistent classes without prior knowledge of them.
;; It is useful to write metalevel tools.

;; Returns a list of persistent classes in the current database.
(define (kahua-persistent-classes-in-db)
  (map (cut ref <> 'name)
       (make-kahua-collection <kahua-persistent-metainfo>)))

;; Returns a the current generation of the named class.
(define (kahua-persistent-class-generation name)
  (and-let* ((info (find-kahua-instance <kahua-persistent-metainfo>
                                        (x->string name))))
    (ref info 'generation)))

;; Returns a define-class form, which creates the definition
;; of the persistent class if evaluated.  NOTE: This does not
;; reproduce the original class definition, but merely creates
;; a class definition that is 'compatible' enough to read/write
;; the persistent object.  It loses class hierarchy (it becomes
;; direct subclass of <kahua-persistent-base>), all slot options
;; except :allocation, and all non-persistent slots.

(define (kahua-persistent-class-definition name . maybe-generation)
  (and-let* ((info (find-kahua-instance <kahua-persistent-metainfo>
                                        (x->string name)))
             (gen (get-optional maybe-generation (ref info 'generation)))
             (sig (if (= gen (ref info 'generation))
                    (ref info 'signature)
                    (assv-ref (ref info 'signature-alist) gen)))
             (sid (find (lambda (p) (memv gen (cdr p)))
                        (ref info 'source-id-map))))
    `(define-class ,name (<kahua-persistent-base>)
       ,(signature->slot-definitions sig)
       :source-id ,(car sid))))

;;=========================================================
;; Database
;;

;; Database class -----------------------------------------
(define current-db (make-parameter #f))

(define-class <kahua-db-meta> (<class>)
  ((all-instances :init-value '())))

(define-method make ((class <kahua-db-meta>) . initargs)
   (let* ((path (get-keyword :path initargs))
          (i    (or (find (lambda (i) (string=? path (ref i 'path)))
                          (ref class 'all-instances))
                    (let1 new-i (next-method)
                      (push! (ref class 'all-instances) new-i)
                      new-i))))
     ;; init-keywords are not evaluated.
     i))

(define-class <kahua-db> ()
  ((path       :init-keyword :path :init-value #f)
   (id-counter :init-keyword :id-counter :init-value 0)
   (active     :init-keyword :active :init-value #f)
   (instance-by-id  :init-form (make-hash-table 'eqv?))
   (instance-by-key :init-form (make-hash-table 'equal?))
   (modified-instances :init-form '())
   (current-transaction-id :init-value 0)
   (floated-modified-instances :init-value '()) ;; modified, but...
   )
  :metaclass <kahua-db-meta>)

;; filesystem-based persistent store (default)
(define-class <kahua-db-fs> (<kahua-db>)
  ((lock-port  :init-value #f) ;; port opened on lock file
   ))

;; DBI-based persistent store (optional)
;;  Currently, only one server per backend database type can be 
;;  used simultaneously, since a driver will be created as a
;;  singleton.
;;  NB: DBI bridge is _temporary_.  The current implementation
;;  isn't efficient, and also it has hazards when multiple
;;  processes access to the same DB.  The data format in DB
;;  will be changed in future in incompatible way.
(define-class <kahua-db-dbi> (<kahua-db>)
  ((driver     :allocation :each-subclass :init-value #f)
   (user       :allocation :each-subclass :init-value #f)
   (password   :allocation :each-subclass :init-value #f)
   (options    :allocation :each-subclass :init-value #f)
   (connection :init-value #f)
   (query      :init-value #f)
   (table-map  :init-value '())  ;; class-name -> table map
   ))

(define-class <kahua-db-mysql> (<kahua-db-dbi>) ())
(define-class <kahua-db-postgresql> (<kahua-db-dbi>) ())

(when (library-exists? 'dbi :strict? #f)
  (autoload dbi
            <dbi-exception>
            dbi-make-driver dbi-make-connection dbi-make-query
            dbi-execute-query dbi-close dbi-get-value dbi-escape-sql))

(define-method initialize ((db <kahua-db-dbi>) initargs)
  (next-method)
  (unless (ref db 'driver)
    (let1 m (#/(.*?):(?:([^:]+)(?::([^:]*)(?::(.*))?)?)?/ (ref db 'path))
      (unless m (error "unsupported database driver path: ~a" (ref db 'path)))
      (set! (ref db 'driver)   (dbi-make-driver (m 1)))
      (set! (ref db 'user)     (m 2))
      (set! (ref db 'password) (m 3))
      (set! (ref db 'options)  (m 4))
      (log-format "DBI(~a) setup: user ~a, options ~a" (m 1) (m 2) (m 4))
      )))

;; Depending on path, select appropriate subclass of <kahua-db>.
(define (select-db-class path)
  (cond ((#/^(.*?):/ path)
         => (lambda (m)
              (let1 dbtype (m 1)
                (cond ((equal? dbtype "mysql") <kahua-db-mysql>)
                      ((equal? dbtype "pg")    <kahua-db-postgresql>)
                      (else (error "unknown external database driver: ~s"
                                   dbtype))))))
        (else <kahua-db-fs>)))

(define-method write-object ((obj <kahua-db>) port)
  (format port "#<kahua-db ~s (~a)>"
          (ref obj 'path)
          (if (ref obj 'active) "active" "inactive")))

(define (kahua-db-unique-id)
  (unless (current-db)
    (error "kahua-db-unique-id: No db is active"))
  (begin0 (ref (current-db) 'id-counter)
          (inc! (ref (current-db) 'id-counter))))

(define-condition-type <with-db-error> <error>
  with-db-error?
  (continuation  kahua-error-with-db))

(define (raise-with-db-error e)
  (call/cc
   (lambda (cnt)
     (raise (make-compound-condition
             e
             (make-condition <with-db-error> 'continuation cnt))))))

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
            (lambda (e)
              (with-error-handler
               (lambda (e2)
                 (if (with-db-error? e)
                     ((kahua-error-with-db e) #f)
                   (raise e2)))
               (lambda ()
                 (kahua-db-close db #f)
                 (if (with-db-error? e)
                     ((kahua-error-with-db e) #f)
                   (raise e)))))
            (lambda ()
              (inc! (ref db 'current-transaction-id))
              ;;(kahua-meta-write-syncer)
              (begin0 (begin . body)
                (when (ref (current-db) 'active)
                  (kahua-db-close db #t)))))))))))

(define (kahua-db-purge-objs)
  (let ((db (current-db)))
    (set! (ref db 'instance-by-id)  (make-hash-table 'eqv?))
    (set! (ref db 'instance-by-key) (make-hash-table 'equal?))))

(define (id-counter-path path)
  (build-path path "id-counter"))

;; lock mechanism - we need more robust one later, but just for now...
(define (lock-file-path path)
  (build-path path "lock"))

(define-method lock-db ((db <kahua-db-dbi>)) #t)
(define-method lock-db ((db <kahua-db-fs>))
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
              (else (sys-sleep 1) (try-lock (- retry 1)))))
      (try-lock 10))))

(define-method unlock-db ((db <kahua-db-dbi>)) #t)
(define-method unlock-db ((db <kahua-db-fs>))
  (and-let* ((lock-port (ref db 'lock-port))
             (record (make <sys-flock> :type F_UNLCK)))
    (sys-fcntl lock-port F_SETLK record)
    #t))

(define (kahua-db-open path)
  (let1 class (select-db-class path)
    (if (eq? class <kahua-db-fs>)
      (kahua-db-open-fs class path)
      (kahua-db-open-dbi class path))))

(define (kahua-db-open-fs class path)
  (let ((cntfile (id-counter-path path)))
    (if (file-is-directory? path)
      (if (file-is-regular? cntfile)
        (let1 db (make class :path path)
          (set! (ref db 'active) #t)
          (set! (ref db 'id-counter) (with-input-from-file cntfile read))
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
        (let1 db (make class :path path)
          (set! (ref db 'active) #t)
          (unless (lock-db db)
            (error "kahua-db-open: couldn't obtain database lock: " path))
          db))
      )))

(define (kahua-db-open-dbi class path)
  (let* ((db   (make class :path path))
         (_    (set! (ref db 'active) #t))
         (conn (dbi-make-connection (ref db 'driver)
                                    (ref db 'user)
                                    (ref db 'password)
                                    (ref db 'options)))
         (q    (dbi-make-query conn)))

    (define (safe-query query)
      (with-error-handler
          (lambda (e)
            (if (is-a? e <dbi-exception>) #f (raise e)))
        (lambda () (dbi-execute-query q query))))

    (define (query-idcount)
      (and-let* ((r (safe-query "select value from kahua_db_idcount"))
                 (p (map (cut dbi-get-value <> 0) r))
                 ((not (null? p))))
        (x->integer (car p))))

    (define (query-classtable)
      (and-let* ((r (safe-query "select class_name, table_name from kahua_db_classes")))
        (map (lambda (row)
               (cons (string->symbol (dbi-get-value row 0))
                     (dbi-get-value row 1)))
             r)))
    
    (set! (ref db 'connection) conn)
    (set! (ref db 'query) q)
    ;; check table existence
    (let1 z (query-idcount)
      (unless z
        ;; this is the first time we use db.
        ;; TODO: error check
        (for-each
         (cut dbi-query q <>)
         '("create table kahua_db_classes (class_name varchar(255), table_name varchar(255), primary key (class_name))"
           "create table kahua_db_idcount (value integer)"
           "insert into kahua_db_idcount values (0)"))
        (let1 zz (query-idcount)
          (unless zz
            (error "couldn't initialize database"))
          (set! z zz)))
      (set! (ref db 'id-counter) z)
      (set! (ref db 'table-map) (query-classtable))
      db)))

;; dbi util for comperhensive error message (Temporary)
(define (dbi-query q sql)
  (with-error-handler
      (lambda (e)
        (if (is-a? e <dbi-exception>)
          (error "DBI error: ~a" (ref e 'message))
          (raise e)))
    (lambda ()
      (dbi-execute-query q sql))))

(define (kahua-db-sync . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (for-each (cut write-kahua-instance db <>)
              (ref db 'modified-instances))
    (set! (ref db 'modified-instances) '())
    (kahua-db-write-id-counter db)))

(define-method kahua-db-write-id-counter ((db <kahua-db-fs>))
  (with-output-to-file (id-counter-path (ref db 'path))
    (cut write (ref db 'id-counter))))

(define-method kahua-db-write-id-counter ((db <kahua-db-dbi>))
  (let ((q (ref db 'query)))
    (dbi-query q #`"update kahua_db_idcount set value = ,(ref db 'id-counter)")))

(define (kahua-db-rollback . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (define (rollback-object obj)
      (when (ref obj '%floating-instance)
        (begin
          (hash-table-delete! (ref db 'instance-by-id) (ref obj 'id))
          (hash-table-delete! (ref db 'instance-by-key)
                              (cons
                               (class-name (class-of obj))
                               (key-of obj)))
          (let1 klass (current-class-of obj)
            (when (eq? klass
                       <kahua-persistent-metainfo>)
              (let1 klass (find-kahua-class (ref obj 'name))
                (set! (ref klass 'metainfo) #f)))))))

    (for-each rollback-object
              (ref db 'modified-instances))))

(define-method kahua-db-close ((db <kahua-db-fs>) commit)
  (if commit
      (kahua-db-sync db)
    (kahua-db-rollback db))
  (unlock-db db)
  (set! (ref db 'modified-instances) '())
  (set! (ref db 'active) #f))

(define-method kahua-db-close ((db <kahua-db-dbi>) commit)
  (if commit
      (kahua-db-sync db)
    (kahua-db-rollback db))
  (dbi-close (ref db 'query))
  (dbi-close (ref db 'connection))
  (set! (ref db 'query) #f)
  (set! (ref db 'connection) #f)
  (set! (ref db 'modified-instances) '())
  (set! (ref db 'active) #f))

(define (data-path db class . key)
  (apply build-path
         (ref db 'path)
         (string-trim-both (x->string (class-name class)) #[<>])
         key))

;; cache consistency management -----------------------------------
(define (current-transaction-id)
  (ref (current-db) 'current-transaction-id))

(define (current-transaction? o)
  (and (current-db)
       (= (slot-ref-using-class (current-class-of o) o '%transaction-id)
          (current-transaction-id))))

(define (update-transaction! o)
  (slot-set-using-class! (current-class-of o) o
                         '%transaction-id
                         (current-transaction-id)))

(define (ensure-transaction o)
  (let1 klass (current-class-of o)
    (unless (or (eq? klass <kahua-persistent-metainfo>)
                (not (slot-bound-using-class? klass o '%transaction-id))
                (current-transaction? o))
      ;; First, we update %transaction-id slot to avoid infinit loop.
      (update-transaction! o)
      ;; read-kahua-instance syncs in-db and in-memory object.
      (read-kahua-instance o))))

(define (persistent-slot-syms class)
  (map car (filter (lambda (slot)
                     (eq? (slot-definition-allocation slot) :persistent))
                   (class-slots class))))

(define (floted-instance-touch! o)
  (update! (ref (ref o 'db) 'floated-modified-instances)
           (lambda (instances)
             (if (memq o instances)
                 instances
                 (cons o instances)))))

(define (kahua-meta-write-syncer)
  (update! (ref (current-db) 'floated-modified-instances)
           (lambda (instances)
             (for-each (lambda (i)
                         ((ref (class-of i) 'write-syncer) i))
                       instances)
             '()))
  (kahua-db-sync))

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
        (and-let* ((i (read-kahua-instance db class key)))
          (set! (ref i '%floating-instance) #f)
          i))))

(define-method read-kahua-instance ((object <kahua-persistent-base>))
  (read-kahua-instance (current-db) (current-class-of object) (key-of object)))

(define-method read-kahua-instance ((db <kahua-db-fs>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (let1 path (data-path db class key)
    (and (file-exists? path)
         (call-with-input-file path read))))

(define-method read-kahua-instance ((db <kahua-db-dbi>)
                                    (class <kahua-persistent-meta>)
                                    (key <string>))
  (define (escape-string str)
    (if (memq 'dbi-do (module-exports (find-module 'dbi))) ;; Gauche-dbi 0.2 and later.
	(string-append "'"
		       (dbi-escape-sql (ref db 'connection) str)
		       "'")
	(dbi-escape-sql (ref db 'connection) str)))

  (and-let* ((tab (assq-ref (ref db 'table-map) (class-name class)))
             (r   (dbi-query
                   (ref db 'query)
                   #`"select dataval from ,|tab| where keyval = ,(escape-string key)"))
             (rv  (map (cut dbi-get-value <> 0) r))
             ((not (null? rv))))
    (call-with-input-string (car rv) read)))

(define-method write-kahua-instance ((db <kahua-db-fs>)
                                     (obj <kahua-persistent-base>))
  (let* ((path  (data-path db (class-of obj) (key-of obj))))
    (make-directory* (sys-dirname path))
    (receive (p tmp) (sys-mkstemp (build-path (ref db 'path) "tmp"))
      (with-error-handler
          (lambda (e) (sys-unlink tmp) (raise e))
        (lambda ()
          (kahua-write obj p)
          (close-output-port p)
          (sys-rename tmp path)
          (set! (ref obj '%floating-instance) #f))))))

(define-method write-kahua-instance ((db <kahua-db-dbi>)
                                     (obj <kahua-persistent-base>))
  (define (escape-string str)
    (if (memq 'dbi-do (module-exports (find-module 'dbi))) ;; Gauche-dbi 0.2 and later.
	(string-append "'"
		       (dbi-escape-sql (ref db 'connection) str)
		       "'")
	(dbi-escape-sql (ref db 'connection) str)))
  
  (define (table-name)
    (let1 cname (class-name (class-of obj))
      (or (assq-ref (ref db 'table-map) cname)
          (let1 newtab (format "kahua_~a" (length (ref db 'table-map)))
            (dbi-query
             (ref db 'query)
             #`"insert into kahua_db_classes values (',|cname|',, ',|newtab|')")
            (dbi-query
             (ref db 'query)
             #`"create table ,|newtab| (keyval varchar(255),, dataval text,, primary key (keyval))")
            (push! (ref db 'table-map) (cons cname newtab))
            newtab))))

  (let* ((data (call-with-output-string (cut kahua-write obj <>)))
         (key  (key-of obj))
         (tab  (table-name)))
    (if (ref obj '%floating-instance)
      (dbi-query
       (ref db 'query)
       #`"insert into ,|tab| values (,(escape-string key),, ,(escape-string data))")
      (dbi-query
       (ref db 'query)
       #`"update ,|tab| set dataval = ,(escape-string data) where keyval = ,(escape-string key)"))
    (set! (ref obj '%floating-instance) #f)
    ))


;;=========================================================
;; "View" as a collection
;;

;; NB: right now, we read all the instances into memory at once.
;; Later we can change the back-end to read it lazily.

(define-class <kahua-collection> (<collection>)
  ((instances :init-keyword :instances :init-value '()))
  )

(define-method append-map (proc (col <collection>))
  (fold (lambda (v r)
          (append (proc v) r))
        '()
        col))

(define-method make-kahua-collection ((class <kahua-persistent-meta>) . opts)
  (let1 db (current-db)
    (unless db (error "make-kahua-collection: database not active"))
    (let-keywords* opts ((subclasses? :subclasses #f))
      (if subclasses?
          (append-map (lambda (c)
                        (coerce-to <list> (make-kahua-collection db c opts)))
                      (cons class
                            (class-subclasses class)))
          (make-kahua-collection db class opts)))))

(define-method make-kahua-collection ((db <kahua-db-fs>)
                                      class opts)
  (make <kahua-collection>
    :instances (map (cut find-kahua-instance class <>)
                    (if (file-is-directory? (data-path db class))
                      (directory-list (data-path db class) :children? #t)
                      '()))))

(define-method make-kahua-collection ((db <kahua-db-dbi>)
                                      class opts)
  (let1 tab (assq-ref (ref db 'table-map) (class-name class))
    (if (not tab)
      (make <kahua-collection> :instances '())
      (let* ((r (dbi-query (ref db 'query)
                           #`"select keyval from ,|tab|"))
             (keys (if r (map (cut dbi-get-value <> 0) r) '())))
        (make <kahua-collection>
          :instances (map (cut find-kahua-instance class <>) keys))))))

(define-method call-with-iterator ((coll <kahua-collection>) proc . opts)
  (let1 p (ref coll 'instances)
    (proc (cut null? p)
          (lambda () (let1 r (car p) (pop! p) r)))))
    
(define-method class-subclasses ((class <class>))
  (define-method class-subclasses* ((class <class>))
    (let1 subs (class-direct-subclasses class)
      (if (null? subs)
	  '()
	  (append subs
		  (append-map class-subclasses* subs)))))
  (delete-duplicates (class-subclasses* class)))

(define instance-changing-class-stack (make-parameter '()))

(define-method change-class ((obj <kahua-persistent-base>) (new-class <class>))
  (let* ((old-class (current-class-of obj))
         ;; dump persisten slot value using old-class.
         (carry-over-slots
          (cond
           ((assq obj (instance-changing-class-stack))
            => (lambda (p) ((cdr p) #f)))
           ;; change-class is called recursively.  abort change-class protocol.
           (else
            (filter-map
             (lambda (slot)
               (let1 slot-name (slot-definition-name slot)
                 (and (eq? (slot-definition-allocation slot) :persistent)
                      (or (and
                           (slot-exists-using-class? old-class obj slot-name)
                           (call/cc
                            (lambda (pcont)
                              (parameterize
                                  ((instance-changing-class-stack
                                    (acons obj pcont (instance-changing-class-stack))))
                                (and (slot-bound-using-class? old-class obj slot-name)
                                     (cons slot-name
                                           (slot-ref-using-class old-class obj slot-name)))))))
                          slot-name))))
             (class-slots new-class))))))
    ;; change-class
    (next-method)
    (unless (eq? (class-name old-class)
                 (class-name new-class))
      (ensure-metainfo new-class)
      (slot-set-using-class! new-class obj 'id (kahua-db-unique-id))
      (slot-set-using-class! new-class obj '%persistent-generation 0)
      (slot-set-using-class! new-class obj '%floating-instance #t))
    ;; restore persistent slots value.
    (unless (memq obj (ref (current-db) 'modified-instances))
      (push! (ref (current-db) 'modified-instances) obj))
    (dolist (slot carry-over-slots)
      (if (pair? slot)
          (slot-set! obj (car slot) (cdr slot))
        (let ((acc (class-slot-accessor new-class slot)))
          (slot-initialize-using-accessor! obj acc '()))))))


(provide "kahua/persistence")
