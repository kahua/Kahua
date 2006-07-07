;; Persistent metaclass
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: persistence.scm,v 1.50.2.12 2006/07/07 10:38:57 bizenn Exp $

(define-module kahua.persistence
  (use srfi-1)
  (use file.util)
  (use util.list)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.collection)
  (export <kahua-persistent-meta> <kahua-persistent-base>
          <kahua-persistent-metainfo>
	  kahua-persistent-id
          key-of find-kahua-class find-kahua-instance
          touch-kahua-instance!
          kahua-serializable-object?
          kahua-persistent-classes-in-db
          kahua-persistent-class-generation
          kahua-persistent-class-definition
          <kahua-db>
          <with-db-error>
          current-db with-db kahua-db-sync kahua-db-rollback
	  kahua-db-purge-objs
          id->kahua-instance class&key->kahua-instance
          <kahua-collection>
          raise-with-db-error
          persistent-initialize
          kahua-wrapper?
          key-of-using-instance
          kahua-check-transaction!
	  kahua-write

	  ;; for Database Driver Module.
	  kahua-db-unique-id
	  lock-db unlock-db
	  kahua-db-open
	  kahua-db-close
	  read-kahua-instance
	  write-kahua-instance
	  kahua-db-write-id-counter
	  make-kahua-collection
          )
  )
(select-module kahua.persistence)

(define kahua-check-transaction! (make-parameter #t))

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
    (let* ((db (current-db))
	   (already-modified? (modified-instance? db in-memory)))
      (for-each (lambda (slot)
		  (slot-set-using-class! class in-memory
					 (car slot)
					 (make <kahua-wrapper> :value (cdr slot))))
		in-db-slots)
      (or already-modified? (reset-modified-instance! db in-memory)))))

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
    (call-with-values
      (cut car+cdr slot)
      (lambda (sn opts)
	(cons sn (delete-keyword :allocation opts)))))

  (let ((alloc (slot-definition-allocation slot)))
    (case alloc
      ((:persistent)
       (let1 acc (let1 slot (delete-slot-definition-allocation slot)
		   (compute-slot-accessor class slot (next-method class slot)))
         (inc! (slot-ref class 'num-instance-slots))
         (list (make-kahua-getter acc class slot)
               (make-kahua-setter acc slot)
               (make-kahua-boundp acc)
               #t)))
      (else (next-method)))))

;; Prohibit to override the slots have :final option as #t.
(define-method compute-slots ((class <kahua-persistent-meta>))
  (receive (slots vs)
      (fold2 (lambda (class slots vs)
	       (fold2 (lambda (s slots vs)
			(if (assq (slot-definition-name s) slots)
			    (if (slot-definition-option s :final #f)
				(values slots (cons s vs))
				(values slots vs))
			    (values (cons s slots) vs)))
		      slots
		      vs
		      (class-direct-slots class)))
	     '()
	     '()
	     (class-precedence-list class))
    (unless (null? vs)
      (errorf "Class ~s attempted to override slot(s) ~a, which is declared as :final in its superclass."
	      class
	      (string-join
	       (reverse! (map (lambda (s) (symbol->string (slot-definition-name s))) vs)) ", ")))
    (reverse! slots)))

(define (modified-instance? db obj)
  (memq obj (modified-instances-of db)))

(define (add-modified-instance! db obj)
  (unless (modified-instance? db obj)
    (push! (modified-instances-of db) obj)))

(define (reset-modified-instance! db obj)
  (update! (modified-instances-of db) (cut delete obj <>)))

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
	      (add-modified-instance! db o)
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
   (%kahua-persistent-base::id :init-keyword :%kahua-persistent-base::id
			       :getter kahua-persistent-id
			       :init-form (kahua-db-unique-id) :final #t)
   ;; management data
   (%kahua-persistent-base::db :init-form (current-db) :final #t)  ; points back to db
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
   ;; persistent key
   (%persistent-key ;; :allocation :persistent
                    :init-value #f)
   )
  :metaclass <kahua-persistent-meta>)

;; this method should be overriden by subclasses for convenience.
(define-method key-of ((obj <kahua-persistent-base>))
  (format "~6,'0d" (slot-ref-using-class (current-class-of obj) obj '%kahua-persistent-base::id)))

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
  (slot-set! obj '%persistent-key (key-of obj)))

(define-method initialize ((obj <kahua-persistent-base>) initargs)
  (next-method)
  (let ((db (current-db))
        (id (kahua-persistent-id obj))
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
          (reset-modified-instance! db obj))
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
  (let1 db (ref obj '%kahua-persistent-base::db)
    (unless (active? db)
      (error "database not active"))
    (add-modified-instance! db obj)))

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
	  ((pair? val)
	   (cons (realize-proxy (car val))
		 (realize-proxy (cdr val))))
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
  (find-kahua-instance (ref proxy 'class) (ref proxy 'key)))

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
	(display (kahua-persistent-id obj))
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
                         (kahua-serializable-object? (cdr v))))
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
                (make class :%kahua-persistent-base::id id
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
         (errorf "persistent-class-slots: class ~a doesn't have generation ~a"
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
   (active     :init-keyword :active :init-value #f :accessor active?)
   (instance-by-id  :init-form (make-hash-table 'eqv?))
   (instance-by-key :init-form (make-hash-table 'equal?))
   (modified-instances :init-form '() :accessor modified-instances-of)
   (current-transaction-id :init-value 0)
   (floated-modified-instances :init-value '()) ;; modified, but...
   )
  :metaclass <kahua-db-meta>)

(define (kahua-concrete-db-class dbtype)
  (let* ((module (string->symbol #`"kahua.persistence.,|dbtype|"))
	 (path (module-name->path module))
	 (class-name (string->symbol #`"<kahua-db-,|dbtype|>")))
    (eval `(begin
	     (require ,path)
	     (with-module ,module ,class-name))
	  (current-module))))

;; Depending on path, select appropriate subclass of <kahua-db>.
(define (select-db-class path)
  (cond ((#/^(\w*?):/ path)
         => (lambda (m)
              (let1 dbtype (m 1)
		(guard (e (else (errorf "unknown external database driver: ~s: ~a"
					dbtype (ref e 'message))))
		  (if (equal? dbtype "pg")
		      (kahua-concrete-db-class "postgresql") ; for backword compatibility
		      (kahua-concrete-db-class dbtype))))))
        (else (kahua-concrete-db-class "fs"))))	; fall back to default file-system DB.

(define (kahua-override-error mn)
  (errorf "You should override this method for concrete database class: ~a" mn))

(define-method write-object ((obj <kahua-db>) port)
  (format port "#<~a ~s (~a)>"
	  (class-name (class-of obj))
          (ref obj 'path)
          (if (active? obj) "active" "inactive")))

(define (kahua-db-unique-id)
  (let1 db (current-db)
    (unless db (error "kahua-db-unique-id: No db is active"))
    (kahua-db-unique-id db)))

(define-method kahua-db-unique-id ((db <kahua-db>))
  (kahua-override-error "kahua-db-unique-id"))

(define-method lock-db ((db <kahua-db>))
  (kahua-override-error "lock-db"))

(define-method unlock-db ((db <kahua-db>))
  (kahua-override-error "unlock-db"))

(define-method kahua-db-open ((db <kahua-db>))
  (kahua-override-error "kahua-db-open"))

(define-method kahua-db-close ((db <kahua-db>))
  (kahua-override-error "kahua-db-close"))

(define-method read-kahua-instance ((db <kahua-db>)
				    (class <kahua-persistent-meta>)
				    (key <string>))
  (kahua-override-error "read-kahua-instance"))

(define-method write-kahua-instance ((db <kahua-db>)
				     (obj <kahua-persistent-base>))
  (kahua-override-error "write-kahua-instance"))

(define-method kahua-db-write-id-counter ((db <kahua-db>))
  (kahua-override-error "kahua-db-write-id-counter"))

(define-method make-kahua-collection ((db <kahua-db>))
  (kahua-override-error "make-kahua-collection"))

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
              (active? (current-db))
              (or (boolean? dbpath)
		  (equal? (ref (current-db) 'path) dbpath)))
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
                (when (active? (current-db))
                  (kahua-db-close db #t)))))))))))

(define (kahua-db-purge-objs)
  (let ((db (current-db)))
    (set! (ref db 'instance-by-id)  (make-hash-table 'eqv?))
    (set! (ref db 'instance-by-key) (make-hash-table 'equal?))))

(define-method kahua-db-open ((path <string>))
  (kahua-db-open (make (select-db-class path) :path path)))

(define (kahua-db-sync . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (for-each (cut write-kahua-instance db <>) (modified-instances-of db))
    (set! (modified-instances-of db) '())
    (kahua-db-write-id-counter db)))

(define (kahua-db-rollback . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (define (rollback-object obj)
      (if (ref obj '%floating-instance)
          (begin
            (hash-table-delete! (ref db 'instance-by-id) (kahua-persistent-id obj))
            (hash-table-delete! (ref db 'instance-by-key)
                                (cons
                                 (class-name (class-of obj))
                                 (key-of obj)))
            (let1 klass (current-class-of obj)
              (when (eq? klass
                         <kahua-persistent-metainfo>)
                (let1 klass (find-kahua-class (ref obj 'name))
                  (set! (ref klass 'metainfo) #f)))))
        (read-kahua-instance obj)))

    (for-each rollback-object (modified-instances-of db))))

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
  (when (kahua-check-transaction!)
    (let1 klass (current-class-of o)
      (unless (or (eq? klass <kahua-persistent-metainfo>)
                  (not (slot-bound-using-class? klass o '%transaction-id))
                  (current-transaction? o))
        ;; First, we update %transaction-id slot to avoid infinit loop.
        (update-transaction! o)
        ;; read-kahua-instance syncs in-db and in-memory object.
        (read-kahua-instance o)
        ))))

(define (persistent-slot-syms class)
  (map car (filter (lambda (slot)
                     (eq? (slot-definition-allocation slot) :persistent))
                   (class-slots class))))

(define (floted-instance-touch! o)
  (update! (ref (ref o '%kahua-persistent-base::db) 'floated-modified-instances)
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

(define (key-of-using-instance obj)
  (ref obj '%floating-instance))

(define-method read-kahua-instance ((object <kahua-persistent-base>))
  (read-kahua-instance (current-db) (current-class-of object) (key-of object)))

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
      (slot-set-using-class! new-class obj '%kahua-persistent-base::id (kahua-db-unique-id))
      (slot-set-using-class! new-class obj '%persistent-generation 0)
      (slot-set-using-class! new-class obj '%floating-instance #t))
    ;; restore persistent slots value.
    (add-modified-instance! (current-db) obj)
    (dolist (slot carry-over-slots)
      (if (pair? slot)
          (slot-set! obj (car slot) (cdr slot))
        (let ((acc (class-slot-accessor new-class slot)))
          (slot-initialize-using-accessor! obj acc '()))))))


(provide "kahua/persistence")
