;; Persistent metaclass
;;
;;  Copyright (c) 2003-2006 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2006 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: persistence.scm,v 1.72 2006/12/26 09:42:13 bizenn Exp $

(define-module kahua.persistence
  (use srfi-1)
  (use srfi-11)
  (use file.util)
  (use util.list)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.collection)
  (use kahua.util)
  (export <kahua-persistence-error>
	  kahua-persistence-error
	  <kahua-persistent-meta>
	  <kahua-persistent-base>
          <kahua-persistent-metainfo>
	  kahua-persistent-id path-of
          key-of find-kahua-class
	  kahua-instance
	  find-kahua-instance
          touch-kahua-instance!
          kahua-serializable-object?
	  kahua-indexable-object?
          kahua-persistent-classes-in-db
          kahua-persistent-class-generation
          kahua-persistent-class-definition
	  removed?
	  modified-instances-of

	  floating-instance?
	  touch-down-instance!

	  ;; Kahua Object Database
          <kahua-db>
	  kahua-db-create
	  <kahua-db-error>
          <with-db-error>
          current-db with-db kahua-db-sync kahua-db-rollback
	  with-kahua-db-connection with-kahua-db-transaction
	  kahua-db-purge-objs
          <kahua-collection>
          raise-with-db-error
          persistent-initialize
          kahua-wrapper?
          kahua-check-transaction!
	  kahua-write
	  kahua-interp-index-translator

	  dump-id-cache
	  dump-key-cache
	  dump-index-cache

	  read-id-cache
	  read-key-cache
	  index-value-write
	  kahua-update-index!

	  ;; for Database Driver Module.
	  kahua-db-unique-id
	  lock-db unlock-db with-locking-db
	  kahua-db-open
	  kahua-db-reopen
	  kahua-db-close
	  kahua-db-ping
	  start-kahua-db-transaction
	  finish-kahua-db-transaction
	  with-kahua-db-transaction
	  read-kahua-instance
	  write-kahua-instance
	  write-kahua-modified-instances
	  kahua-db-write-id-counter
	  make-kahua-collection
	  remove-kahua-instance
	  make-kahua-collection-filter

	  kahua-persistent-instances

	  ;; for Check and fix database consistency.
	  dbutil:check-kahua-db-classcount
	  dbutil:check-kahua-db-idcount
	  dbutil:check-removed-flag-column
	  dbutil:check-removed-flag-column-for-all-tables
	  dbutil:check-alive-directory

	  dbutil:with-dummy-reader-ctor
	  dbutil:persistent-classes-fold
	  dbutil:check&fix-database
	  dbutil:kahua-db-fs->efs
          ))

(select-module kahua.persistence)

(define-condition-type <kahua-persistence-error> <kahua-error> #f)
(define (kahua-persistence-error fmt . args)
  (apply errorf <kahua-persistence-error> fmt args))

;; Deprecated, and now no effect.
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
   (class-alist :allocation :class :init-value '() :accessor class-alist-of)

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
   (index-cache   :init-form (create-index-cache))
   ))

(define-method make ((class <kahua-persistent-meta>) . initargs)
  ;; When the first instance of a persistent class is realized (created
  ;; in memory), the in-memory definition and in-db definition is compared.
  (ensure-metainfo class)
  (next-method))

(define persistent-class-alist
  (getter-with-setter
   (lambda () (class-slot-ref <kahua-persistent-meta> 'class-alist))
   (lambda (o) (class-slot-set! <kahua-persistent-meta> 'class-alist o))))

(define (kahua-read-syncer in-memory in-db-slots)
  ;; we shuold update transaction-id of in-memory object at last,
  ;; but to avoid inifinite loop, we update it here.
  (let1 class (current-class-of in-memory)
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
           (lambda _ (kahua-persistence-error "object was obtained in other transaction")))
          ((eq? v :auto) default)
          ((procedure? v) v)
          (else (lambda _ #f))))

  (next-method)

  (update! (ref class 'read-syncer)  (pa$ make-syncer kahua-read-syncer))
;   (update! (ref class 'write-syncer) (pa$ make-syncer kahua-write-syncer))
  (set! (class-alist-of class)
	(assq-set! (class-alist-of class) (class-name class) class)))

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

(define (reset-modified-index-slots! obj)
  (slot-set-using-class! (current-class-of obj) obj '%modified-index-slots '()))

(define (reset-modified-instance! db obj)
  (update! (modified-instances-of db) (cut delete obj <>))
  (reset-modified-index-slots! obj))

(define (clear-modified-instances! db)
  (for-each reset-modified-index-slots! (modified-instances-of db))
  (set! (modified-instances-of db) '()))

(define (%removed-object? obj)
  (and (is-a? (current-class-of obj) <kahua-persistent-meta>)
       (removed? obj)))

(define (%sanitize-object obj)
  (if (%removed-object? obj)
      #f
      obj))

(define (make-kahua-getter acc class slot)
  (let ((aot (slot-definition-option slot :out-of-transaction :read-only)))
    (lambda (o)
      (if (current-db)
          (ensure-transaction o)
          (when (eq? aot :denied)
            (error "database not active")))
      (let* ((val (slot-ref-using-accessor o acc))
	     (real (if (and (kahua-wrapper? val)
			    (not (assq o (instance-changing-class-stack))))
		       (peel-wrapper val)
		       val)))
	(cond ((%removed-object? real)
	       (slot-set-using-accessor! o acc #f)
	       (add-modified-instance! (current-db) o)
	       #f)
	      ((not (eq? val real))
	       (slot-set-using-accessor! o acc real)
	       real)
	      (else val))))))

(define (make-kahua-setter acc slot)
  (let ((aot       (slot-definition-option slot :out-of-transaction :read-only))
        (slot-name (slot-definition-name slot)))
    (lambda (o v)
      (let ((db (current-db))
	    (v (%sanitize-object v)))
        (if db
            (begin
              (ensure-transaction o)
	      (add-modified-instance! db o)
	      (check-index-slot o slot v)
              (slot-set-using-accessor! o acc v))
	    (if (eq? aot :read/write)
		(begin
		  (unless (assq slot-name (ref o '%in-transaction-cache))
		    (push! (ref o '%in-transaction-cache)
			   (cons slot-name (slot-ref-using-accessor o acc))))
		  (floated-instance-touch! o)
		  (check-index-slot o slot v)
		  (slot-set-using-accessor! o acc v))
		(error "database not active")))))))

(define (make-kahua-boundp acc)
  (lambda (o)
    (slot-bound-using-accessor? o acc)))

(define (safe-ref o sn fallback)
  (let1 class (current-class-of o)
    (if (slot-bound-using-class? class o sn)
	(slot-ref-using-class class o sn)
	fallback)))

(define (check-index-slot o slot v)
  (define (peel* v)
    (if (kahua-wrapper? v)
	(peel-wrapper v)
	v))
  (and-let* ((idx (slot-definition-option slot :index #f))
	     (sn (slot-definition-name slot))
	     (class (current-class-of o)))
    (let*-values (((modified-slots) (safe-ref o '%modified-index-slots '()))
		  ((nv) (peel* v))
		  ((directive ov) (cond ((assq sn modified-slots) =>
					 (lambda (e)
					   (cond ((eq? (list-ref e 1) :add)
						  (unregister-index-cache o sn idx (list-ref e 4))
						  (values :add #f))
						 (else
						  (values :modify (list-ref e 3))))))
					((slot-bound-using-class? class o sn)
					 (values :modify (slot-ref-using-class class o sn)))
					(else (values :add #f)))))
      (unless (or (and (eq? directive :modify) (equal? ov nv)) (removed? o))
	(case directive ((:drop :modify) (unregister-index-cache o sn idx ov)))
	(slot-set-using-class! class o '%modified-index-slots
			       (assq-set! modified-slots sn (list directive idx ov nv)))
	(case directive ((:modify :add) (register-index-cache o sn idx nv)))))))

(define (index-value-write value)
  (cond ((any (pa$ is-a? value) `(,<string> ,<number> ,<symbol> ,<boolean> ,<keyword>))
	 (write value))
	((null? value) (write '()))
	((pair? value)
	 (write-char #\()(index-value-write (car value))
	 (for-each (lambda (e)
		     (write-char #\space)
		     (index-value-write e)) (cdr value))
	 (write-char #\)))
	((vector? value)
	 (write-char #\#)
	 (index-value-write (vector->list value)))
	(else (kahua-persistence-error "Object ~s cannot be used as index value" value))))

(define (kahua-indexable-object? obj)
  (or (any (pa$ is-a? obj) `(,<string> ,<number> ,<symbol> ,<boolean> ,<keyword>))
      (and (or (list? obj) (vector? obj))
	   (let/cc ret
	     (for-each (lambda (obj)
			 (unless (kahua-indexable-object? obj)
			   (ret #f)))
		       obj)
	     #t))))

(define (drop-all-index-values! obj)
  (let1 class (current-class-of obj)
    (define (drop-index-value o slot)
      (and-let* ((idx (slot-definition-option slot :index #f))
		 (sn (slot-definition-name slot)))
	(let* ((modified-slots (safe-ref o '%modified-index-slots '()))
	       (ov (cond ((assq sn modified-slots) => (cut list-ref <> 3))
			 (else (slot-ref-using-class class o sn)))))
	  (slot-set-using-class! class o '%modified-index-slots
				 (assq-set! modified-slots sn (list :drop idx ov #f)))
	  (unregister-index-cache o sn idx (slot-ref-using-class class o sn)))))
    (for-each (pa$ drop-index-value obj) (class-slots class))))

;;
;; Index Slot Cache Handling
;;
;; Unique Index:
;;   key:   (slot-name . index-value)
;;   value: object
;;
;; Any Index:
;;   key:   (slot-name . index-value)
;;   value: (object ...)

(define create-index-cache (cut make-hash-table 'equal?))
(define (clear-index-cache! class)
  (let1 cache (slot-ref class 'index-cache)
    (hash-table-for-each cache (lambda (k _) (hash-table-delete! cache k)))))
(define (clear-all-index-cache!)
  (for-each (compose clear-index-cache! cdr) (persistent-class-alist)))

(define (index-cache-key slot-name value)
  (cons slot-name value))

(define (read-index-cache class slot-name value)
  (and-let* ((slot (class-slot-definition class slot-name))
	     (index (slot-definition-option slot :index #f)))
    (hash-table-get (slot-ref class 'index-cache) (index-cache-key slot-name value) #f)))

(define (%register-index-cache class obj slot-name index value)
  (let* ((idxcache (slot-ref class 'index-cache))
	 (key (index-cache-key slot-name value)))
    (case index
      ((:unique)
       (or (and-let* ((v (hash-table-get idxcache key #f)))
	     (if (eq? v obj)
		 #t			; do nothing
		 (kahua-persistence-error "~s: index slot ~s conflict with ~s" obj key v)))
	   (hash-table-put! idxcache key obj)))
      ((:any)
       (hash-table-update! idxcache key
			   (lambda (objs)
			     (if (memq obj objs) objs (cons obj objs)))
			   '()))
      (else (kahua-persistence-error "Unknown index type: ~s" index)))))

(define (register-index-cache obj slot-name index value)
  (%register-index-cache (current-class-of obj) obj slot-name index value))

(define (unregister-index-cache obj slot-name index value)
  (let* ((class (current-class-of obj))
	 (idxcache (slot-ref class 'index-cache))
	 (key (index-cache-key slot-name value)))
    (case index
      ((:unique)
       (and (eq? obj (hash-table-get idxcache key #f))
	    (hash-table-delete! idxcache key)))
      ((:any)
       (hash-table-update! idxcache key (cut delete! obj <> eq?) '())
       (when (null? (hash-table-get idxcache key '()))
	 (hash-table-delete! idxcache key)))
      (else
       (kahua-persistence-error "Unknown index type: ~s" index)))))

(define (dump-index-cache class)
  (hash-table-map (slot-ref class 'index-cache) cons))

;;=========================================================
;; Persistent baseclass
;;

(define-class <kahua-persistent-base> ()
  (;; unique ID 
   (%kahua-persistent-base::id :init-keyword :%kahua-persistent-base::id
			       :init-form (%kahua-db-unique-id) :final #t)
   (%kahua-persistent-base::removed? :init-keyword :%kahua-persistent-base::removed?
				     :init-value #f :final #t) ; It's removed?
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
   (%persistent-key :init-value #f)
   ;; updated index alist ((slot-name :directive :kind old-value new-value) ...)
   (%modified-index-slots))
  :metaclass <kahua-persistent-meta>)

(define (kahua-persistent-base? obj)
  (is-a? obj <kahua-persistent-base>))

;; You should not override this method.
(define-method kahua-persistent-id ((obj <kahua-persistent-base>))
  (slot-ref-using-class (current-class-of obj) obj '%kahua-persistent-base::id))

;; this method should be overriden by subclasses for convenience.
(define-method key-of ((obj <kahua-persistent-base>))
  (format "~6,'0d" (slot-ref-using-class (current-class-of obj) obj '%kahua-persistent-base::id)))

(define-method removed? ((obj <kahua-persistent-base>))
  (let1 class (current-class-of obj)
    (and (slot-bound-using-class? class obj '%kahua-persistent-base::removed?)
	 (slot-ref-using-class class obj '%kahua-persistent-base::removed?))))

(define-method floating-instance? ((obj <kahua-persistent-base>))
  (let1 class (current-class-of obj)
    (and (slot-bound-using-class? class obj '%floating-instance)
	 (slot-ref-using-class class obj '%floating-instance))))

(define-method touch-down-instance! ((obj <kahua-persistent-base>))
  (slot-set-using-class! (current-class-of obj) obj '%floating-instance #f))

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
  obj)					; DUMMY

(define-method initialize ((obj <kahua-persistent-base>) initargs)
  (define (check-id-cache db id)
    (and-let* ((o (read-id-cache db id)))
      (kahua-persistence-error "instance ~s with the same ID (~s) is active" o id)))
  (let ((db (current-db))
	(class (class-of obj)))
    ;; Avoid that slots initialized twice.
    (define (persistent-initialize-from-db obj initargs rsv)
      (define (slot-initialize slot)
	(let* ((slot-name (slot-definition-name slot))
	       (accessor (class-slot-accessor class slot-name)))
	  (cond ((assq slot-name rsv)
		 => (lambda (p)
		      (slot-set-using-accessor! obj accessor
						(make <kahua-wrapper> :value (cdr p)))))
		(else (slot-initialize-using-accessor! obj accessor initargs)))))
      (let* ((slots (class-slots class))
	     (db (current-db)))
	(and-let* ((id (get-keyword :%kahua-persistent-base::id initargs))
		   (o (read-id-cache db id)))
	  (kahua-persistence-error "instance ~s with the same ID (~s) is active" o id))
	(let*-values (((index-slots non-index-slots)
		       (partition (cut slot-definition-option <> :index #f) slots))
		      ((transaction-id rest-slots)
		       (partition (lambda (s) (eq? (slot-definition-name s) '%transaction-id)) non-index-slots)))
	  (for-each slot-initialize rest-slots)
	  (for-each slot-initialize index-slots)
	  (for-each slot-initialize transaction-id)) ; Avoid to initialize recursively.
	(update-transaction! obj)
	(reset-modified-instance! db obj)
	obj))

    (unless db (kahua-persistence-error "No database is active"))
    (cond ((get-keyword :%realization-slot-values initargs #f)
	   => (pa$ persistent-initialize-from-db obj initargs))
	  (else
	   (next-method)
	   (and-let* ((id (kahua-persistent-id obj))
		      (o (read-id-cache db id)))
	     (kahua-persistence-error "instance ~s with the same ID (~s) is active" o id))
	   (update-transaction! obj)
	   ;; initialize persistent object for the first time.
	   (persistent-initialize obj initargs)))
    (register-id-cache db obj)
    (register-key-cache db obj)
    (unless (slot-bound? obj '%modified-index-slots)
      (slot-set! obj '%modified-index-slots '()))
    obj))

(define (find-kahua-class name)
  (or (assq-ref (persistent-class-alist) name)
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

(define (kahua-proxy? obj)
  (is-a? obj <kahua-proxy>))

(define-method realize-kahua-proxy ((proxy <kahua-proxy>))
  (find-kahua-instance (ref proxy 'class) (ref proxy 'key)))

;; The bottom-level writer ----------------------------------------
;;   write-kahua-instance calls kahua-write.

;; for now, we don't consider shared structure
(define (%save-slot s)
  (write-char #\()
  (display (car s))
  (display " . ")
  (serialize-value (cdr s))
  (write-char #\))
  (newline))

;; kahua-object (version 1)
(define (kahua-object-write obj port)
  (with-output-to-port port
    (lambda ()
      (receive (generation vals hidden) (export-slot-values obj)
	(display "#,(kahua-object (")
	(display (class-name (class-of obj)))
	(write-char #\space)
	(display generation)
	(display ") ")
	(display (kahua-persistent-id obj))
        (for-each %save-slot
                  (if (null? hidden)
		      vals
		      (cons `(%hidden-slot-values . ,hidden) vals)))
        (display ")\n")))))
;; kahua-object (version 2: add removed flag)
(define (kahua-object2-write obj port)
  (with-output-to-port port
    (lambda ()
      (receive (generation vals hidden) (export-slot-values obj)
	(display "#,(kahua-object2 (")
	(display (class-name (class-of obj)))
	(write-char #\space)
	(display generation)
	(display ") ")
	(display (kahua-persistent-id obj))
	(write-char #\space)
	(write (removed? obj))
	(newline)
	(for-each %save-slot
                  (if (null? hidden)
		      vals
		      (cons `(%hidden-slot-values . ,hidden) vals)))
	(display ")\n")))))

(define-method kahua-write ((obj <kahua-persistent-base>) port)
  (kahua-object2-write obj port))

;; serialization
(define (serialize-value v)
  (let1 v (%sanitize-object v)
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
     ((kahua-persistent-base? v)
      (display "#,(kahua-proxy ")
      (display (class-name (class-of v)))
      (display " ")
      (write (key-of v))
      (display " )"))
     ((kahua-proxy? v)
      (display "#,(kahua-proxy ")
      (display (class-name (ref v 'class)))
      (display " ")
      (write (ref v 'key))
      (display " )"))
     ((kahua-wrapper? v)
      (serialize-value (ref v 'value)))
     (else
      (error "object not serializable:" v)))))

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

;; kahua-object (1st version)
(define (kahua-object-read class-desc id . vals)
  (apply kahua-object2-read class-desc id #f vals))
(define-reader-ctor 'kahua-object kahua-object-read)

;; kahua-object2 (2nd version: add removed flag)
(define (kahua-object2-read class-desc id removed? . vals)
  (let1 object (read-id-cache (current-db) id)
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
	      (slot-set-using-class! (current-class-of object) object
				     '%kahua-persistent-base::removed? removed?)
	      ((ref class 'read-syncer) object slot-alist)
	      object)
	    (begin
	      (make class :%kahua-persistent-base::id id
		    :%realization-slot-values slot-alist
		    :%hidden-slot-values save-slots
		    :%persistent-generation generation
		    :%kahua-persistent-base::removed? removed?
		    :%floating-instance #f))
	    )))))
(define-reader-ctor 'kahua-object2 kahua-object2-read)

;; kahua-proxy
(define (kahua-proxy-read cname key)
  (make <kahua-proxy> :class (find-kahua-class cname) :key key))
(define-reader-ctor 'kahua-proxy kahua-proxy-read)

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
   (previous-generation :allocation :persistent :init-value 0
			:init-keyword :previous-generation)
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
   ;; ((slot-name :directive :result-kind) ...)
   (index-translator :init-value '() :accessor index-translator-of)
   ))

(define-method key-of ((info <kahua-persistent-metainfo>))
  (x->string (ref info 'name)))

(define-method ensure-metainfo ((class <kahua-persistent-meta>))
  (unless (slot-ref class 'metainfo)
    (persistent-class-bind-metainfo class))
  (ref class 'metainfo))

(define (ensure-all-classes)
  (for-each (lambda (c)
             (unless (eq? (car c) '<kahua-persistent-base>)
               (ensure-metainfo (cdr c))))
           (persistent-class-alist)))

;; Calculates class signature.  Currently we take all persistent-allocated
;; slots.
(define-method persistent-class-signature ((class <kahua-persistent-meta>))
  (define (extract-slot-def slot)
    (let1 allocation (slot-definition-allocation slot)
      (and (memq allocation '(:persistent))
	   `(,(slot-definition-name slot)
	     :allocation ,allocation
	     ,@(or (and-let* ((index (slot-definition-option slot :index #f)))
		     (list :index index))
		   '())))))
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

  (define (find-signature metainfo generation)
    (if (= generation (ref metainfo 'generation))
	(ref metainfo 'signature)
	(and-let* ((gen&sig (find (lambda (gen&sig) (= (car gen&sig) generation))
				  (ref metainfo 'signature-alist))))
	  (cdr gen&sig))))

  ;; we couldn't find matching signature, so we assume the in-memory
  ;; class is newer.  record the newer class signature.
  (define (increment-generation metainfo signature)
    (let* ((oldgen (ref metainfo 'generation))
           (newgen (+ oldgen 1))
           (oldsig (ref metainfo 'signature)))
      (slot-push! metainfo 'signature-alist (cons oldgen oldsig))
      (receive (translator-alist index-translator)
	  (compute-translation-directive oldsig signature)
	(set! (index-translator-of metainfo) index-translator)
	(unless (null? translator-alist)
	  (slot-push! metainfo 'translator-alist (cons oldgen translator-alist))))
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
      (let1 signature (persistent-class-signature class)
	(or (and-let* ((metainfo (find-kahua-instance <kahua-persistent-metainfo>
						      (x->string (class-name class))))
		       (prev-gen (ref metainfo 'previous-generation)))
	      (begin0
		(cond ((signature=? (ref metainfo 'signature) signature)
		       ;; in-memory class is up to date.
		       (let*-values (((gen) (ref metainfo 'generation))
				     ((prev-sig) (find-signature metainfo prev-gen))
				     ((_ index-translator)
				      (compute-translation-directive prev-sig signature)))
			 (set! (index-translator-of metainfo) index-translator)
			 (record-source-id metainfo gen)
			 (set-generation! gen gen metainfo)))
		      ((find-generation metainfo signature)
		       => (lambda (gen&sig)
			    (let*-values (((prev-sig) (find-signature metainfo prev-gen))
					  ((_ index-translator)
					   (compute-translation-directive prev-sig signature)))
			      (set! (index-translator-of metainfo) index-translator)
			      (set-generation! (car gen&sig)
					       (ref metainfo 'generation)
					       metainfo))))
		      (else
		       (increment-generation metainfo signature)))
		(and-let* ((curr-gen (ref class 'generation))
			   ((not (= prev-gen curr-gen))))
		  (slot-set! metainfo 'previous-generation curr-gen))))
	    ;; metainfo does not exist.
	    (register-metainfo signature)))))

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

(define (compute-translation-directive A-sig B-sig)
  ;; FIXME!!
  ;; This should be more general.
  ;; And assume that (eq? (slot-definition-name A-slot)
  ;;                      (slot-definition-name B-slot))
  (define (compute-index-change A-slot B-slot)
    (let ((A-idx (slot-definition-option A-slot :index #f))
	  (B-idx (slot-definition-option B-slot :index #f)))
      (cond ((eq? A-idx B-idx) #f)
	    ((not A-idx) (list :add B-idx))
	    ((not B-idx) (list :drop A-idx))
	    (else        (list :modify B-idx)))))
  (define (index-directive-filter s directive)
    (and-let* ((idx (slot-definition-option s :index #f)))
      (list (slot-definition-name s) directive idx)))

  (let loop ((A-slots (signature->slot-definitions A-sig))
	     (B-slots (signature->slot-definitions B-sig))
	     (dirs '()) (index-dirs '()))
    (cond ((null? A-slots)
	   (values
	    (append! dirs (map (lambda (s) (cons (slot-definition-name s) :add)) B-slots))
	    (append! index-dirs (filter-map (cut index-directive-filter <> :add) B-slots))))
          ((null? B-slots)
           (values
	    (append! dirs (map (lambda (s) (cons (car s) :drop)) A-slots))
	    (append! index-dirs (filter-map (cut index-directive-filter <> :drop) A-slots))))
	  (else
	   (let* ((A-slot (car A-slots))
		  (B-slot (car B-slots))
		  (A-slot-name (slot-definition-name A-slot))
		  (B-slot-name (slot-definition-name B-slot)))
	     (cond ((eq? A-slot-name B-slot-name)
		    (or (and-let* ((index-change (compute-index-change (car A-slots) (car B-slots))))
			  (loop (cdr A-slots) (cdr B-slots)
				dirs (acons A-slot-name index-change index-dirs)))
			(loop (cdr A-slots) (cdr B-slots) dirs index-dirs)))
		   ((string<? (symbol->string A-slot-name) (symbol->string B-slot-name))
		    (loop (cdr A-slots) B-slots (acons A-slot-name :drop dirs)
			  (or (and-let* ((directive (index-directive-filter A-slot :drop)))
				(cons directive index-dirs))
			      index-dirs)))
		   (else
		    (loop A-slots (cdr B-slots) (acons B-slot-name :add dirs)
			  (or (and-let* ((directive (index-directive-filter B-slot :add)))
				(cons directive index-dirs))
			      index-dirs)))))))
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
;; Kahua Object Database
;;

;; Database class -----------------------------------------
(define current-db (make-parameter #f))

(define-condition-type <kahua-db-error> <error> kahua-db-error?)
(define (kahua-db-error fmt . args)
  (error <kahua-db-error> :message (apply format fmt args)))

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
  ((path       :init-keyword :path :init-value #f :accessor path-of)
   (active     :init-keyword :active :init-value #f :accessor active?)
   (instance-by-id  :init-form (create-id-cache))
   (instance-by-key :init-form (create-key-cache))
   (modified-instances :init-form '() :accessor modified-instances-of)
   (current-transaction-id :init-value 0)
   (floated-modified-instances :init-value '()) ;; modified, but...
   )
  :metaclass <kahua-db-meta>)

(define-generic kahua-db-create)

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
  (cond ((#/^(\w+):/ path)
         => (lambda (m)
              (let1 dbtype (m 1)
		(guard (e (else (errorf "unknown external database driver: ~s: ~a"
					dbtype (ref e 'message))))
		  (if (equal? dbtype "pg")
		      (kahua-concrete-db-class "postgresql") ; for backword compatibility
		      (kahua-concrete-db-class dbtype))))))
        (else (kahua-concrete-db-class "efs")))) ; fall back to default extended file-system DB.

(define-method write-object ((obj <kahua-db>) port)
  (format port "#<~a ~s (~a)>"
	  (class-name (class-of obj))
          (ref obj 'path)
          (if (active? obj) "active" "inactive")))

(define-method write-object ((obj <kahua-wrapper>) port)
  (format port "#<~a ~s>" (class-name (class-of obj)) (ref obj 'value)))

(define (%kahua-db-unique-id)
  (let1 db (current-db)
    (unless db (error "kahua-db-unique-id: No db is active"))
    (kahua-db-unique-id db)))

(define-generic kahua-db-unique-id)

(define-generic lock-db)
(define-generic unlock-db)
(define-method with-locking-db ((db <kahua-db>) thunk)
  (dynamic-wind
      (cut lock-db db)
      thunk
      (cut unlock-db db)))
(define-generic kahua-db-open)
(define-generic kahua-db-close)
(define-generic kahua-db-ping)
(define-generic kahua-db-reopen)
(define-generic read-kahua-instance)
(define-generic write-kahua-instance)
(define-method remove-kahua-instance ((obj <kahua-persistent-base>))
  (ensure-transaction obj)
  (slot-set! obj '%kahua-persistent-base::removed? #t)
  (drop-all-index-values! obj)
  (touch-kahua-instance! obj))
(define-generic kahua-db-write-id-counter)
(define-generic kahua-persistent-instances)

(define-condition-type <with-db-error> <kahua-error>
  with-db-error?
  (continuation  kahua-error-with-db))

(define (raise-with-db-error e . may-be-cont)
  (define (raise/cc e cont)
    (raise (make-compound-condition
	    e
	    (make-condition <with-db-error> 'continuation cont))))
  (let1 cont (get-optional may-be-cont #f)
    (if cont
	(raise/cc e cont)
	(call/cc (pa$ raise/cc e)))))

(define-syntax with-db
  (syntax-rules ()
    ((_ (db dbpath) . body)
     (if (already-db-opened? dbpath)
	 (with-kahua-db-transaction (current-db) (lambda (db) . body))
	 (with-kahua-db-connection dbpath
	   (lambda (db)
	     (with-kahua-db-transaction db (lambda (db) . body))))))))

(define (already-db-opened? dbpath)
  (and-let* ((db (current-db)))
    (and (active? db)
	 (or (boolean? dbpath)
	     (equal? (path-of db) dbpath)))))

(define (with-kahua-db-connection dbpath proc)
  (let1 db (kahua-db-open dbpath)
    (parameterize ((current-db db))
      (guard (e (else
		 (guard (e2 ((with-db-error? e) ((kahua-error-with-db e) #f))
			    (else (raise e)))
		   (kahua-db-close db #f)
		   (if (with-db-error? e)
		       ((kahua-error-with-db e) #f)
		       (raise e)))))
	(inc! (ref db 'current-transaction-id))
	;;(kahua-meta-write-syncer)
	(begin0 (proc db)
		(when (active? db)
		  (kahua-db-close db #t)))))))

(define (kahua-db-purge-objs . may-be-db)
  (let1 db (get-optional may-be-db (current-db))
    (clear-id-cache! db)
    (clear-key-cache! db)
    (clear-all-index-cache!)))

(define-method kahua-db-open ((path <string>))
  (kahua-db-open (make (select-db-class path) :path path)))

(define-method start-kahua-db-transaction ((db <kahua-db>))
  (inc! (ref db 'current-transaction-id))
  (unless (lock-db db)
    (error "kahua-db-open: couldn't obtain database lock: " db)))
(define-method finish-kahua-db-transaction ((db <kahua-db>) commit?)
  (unlock-db db))
(define (with-kahua-db-transaction db proc)
  (start-kahua-db-transaction db)
  (guard (e (else
	     (guard (e2 ((with-db-error? e)
			 ((kahua-error-with-db e) #f))
			(else
			 (kahua-db-purge-objs db)
			 (clear-modified-instances! db)
			 (raise e)))
	       (finish-kahua-db-transaction db #f)
	       (if (with-db-error? e)
		   ((kahua-error-with-db e) #f)
		   (raise e)))))
    (begin0
      (proc db)
      ;; If kahua-db-sync fail, call kahua-db-rollback but not raise error
      ;; FIXME!!
      (finish-kahua-db-transaction db #t))))

(define-method kahua-interp-index-translator ((db <kahua-db>) cn translator)
  (kahua-persistence-error "~s does not support index translation" db))

(define-method kahua-update-index! ((db <kahua-db>) (metainfo <kahua-persistent-metainfo>))
  (and-let* ((t (index-translator-of metainfo))
	     ((pair? t))
	     (class (find-kahua-class (slot-ref metainfo 'name))))
    (kahua-interp-index-translator db class t)
    (set! (index-translator-of metainfo) '())))

(define-method write-kahua-modified-instances ((db <kahua-db>))
  (receive (metainfos others) (partition (cut is-a? <> <kahua-persistent-metainfo>)
					 (modified-instances-of db))
    (for-each (lambda (m)
		(kahua-update-index! db m)
		(write-kahua-instance db m))
	      metainfos)
    (for-each (lambda (o)
		(write-kahua-instance db o)
		(when (removed? o)
		  (unregister-id-cache db o)
		  (unregister-key-cache db o)))
	      others)))

(define (kahua-db-sync . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (write-kahua-modified-instances db)
    (clear-modified-instances! db)
    (kahua-db-write-id-counter db)))

(define (kahua-db-rollback . maybe-db)
  (let1 db (get-optional maybe-db (current-db))
    (define (rollback-object obj)
      (if (floating-instance? obj)
          (begin
	    (unregister-id-cache db obj)
	    (unregister-key-cache db obj)
            (let1 klass (current-class-of obj)
              (when (eq? klass
                         <kahua-persistent-metainfo>)
                (let1 klass (find-kahua-class (ref obj 'name))
                  (set! (ref klass 'metainfo) #f)))))
	  (read-kahua-instance obj)))

    (for-each rollback-object (modified-instances-of db))
    (clear-all-index-cache!)
    (clear-modified-instances! db)))

;;
;; On Memory Cache (by ID)
;;

(define create-id-cache (cut make-hash-table 'eqv?))

(define (clear-id-cache! db)
  (slot-set! db 'instance-by-id (create-id-cache)))

(define-method register-id-cache ((db <kahua-db>) (obj <kahua-persistent-base>))
  (hash-table-put! (slot-ref db 'instance-by-id) (kahua-persistent-id obj) obj))

(define-method unregister-id-cache ((db <kahua-db>) (obj <kahua-persistent-base>))
  (hash-table-delete! (slot-ref db 'instance-by-id) (kahua-persistent-id obj)))

(define-method read-id-cache ((db <kahua-db>) (id <integer>))
  (hash-table-get (slot-ref db 'instance-by-id) id #f))

(define-method on-id-cache? ((db <kahua-db>) (id <integer>) slot-name slot-value)
  (and-let* ((obj (read-id-cache db id))
	     ((not (removed? obj)))
	     (class (current-class-of obj)))
    (and (slot-bound-using-class? class obj slot-name)
	 (equal? (slot-ref-using-class class obj slot-name) slot-value)
	 obj)))

(define (dump-id-cache db)
  (hash-table-map (slot-ref db 'instance-by-id) cons))

(define (all-instances-on-id-cache db)
  (hash-table-values (slot-ref db 'instance-by-id)))

;;
;; On Memory Cache (by Key)
;;

(define create-key-cache (cut make-hash-table 'equal?))

(define (clear-key-cache! db)
  (slot-set! db 'instance-by-key (create-key-cache)))

(define make-cache-key cons)

(define-method register-key-cache ((db <kahua-db>) (obj <kahua-persistent-base>))
  (let1 key (key-of obj)
    (slot-set! obj '%persistent-key key)
    (hash-table-put! (slot-ref db 'instance-by-key)
		     (make-cache-key (class-name (class-of obj)) key)
		     obj)))

(define-method unregister-key-cache ((db <kahua-db>) (obj <kahua-persistent-base>))
  (hash-table-delete! (slot-ref db 'instance-by-key)
		      (make-cache-key (class-name (class-of obj)) (key-of obj))))

(define-method read-key-cache ((db <kahua-db>) (cn <symbol>) key)
  (hash-table-get (slot-ref db 'instance-by-key)
		  (make-cache-key cn key) #f))

(define-method on-key-cache? ((db <kahua-db>) (cn <symbol>) key slot-name slot-value)
  (and-let* ((obj (read-key-cache db cn key))
	     ((not (removed? obj)))
	     (class (current-class-of obj)))
    (and (slot-bound-using-class class obj slot-name)
	 (equal? (slot-ref-using-class class obj slot-name) slot-value)
	 obj)))

(define (dump-key-cache db)
  (hash-table-map (slot-ref db 'instance-by-key) cons))

;;
;; cache consistency management -----------------------------------
;;
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
      (read-kahua-instance o)
      )))

(define (persistent-slot-syms class)
  (map car (filter (lambda (slot)
                     (eq? (slot-definition-allocation slot) :persistent))
                   (class-slots class))))

(define (floated-instance-touch! o)
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

(define (kahua-instance class id . args)
  (let ((db (current-db))
	(sanitize (if (get-optional args #f)
		      identity
		      %sanitize-object)))
    (unless db (error "kahua-instance: No database is active"))
    (sanitize (or (and-let* ((i (read-id-cache db id))
			     ((eq? (class-name (current-class-of i))
				   (class-name class)))) ; not subclass
		    i)
		  (read-kahua-instance db class id)))))

(define-method find-kahua-instance ((class <kahua-persistent-meta>)
				    (key <string>)
				    . args)
  (let* ((db (current-db))
	 (include-removed-object? (get-optional args #f))
	 (sanitize (if include-removed-object?
		       identity
		       %sanitize-object)))
    (unless db (error "find-kahua-instance: No database is active"))
    (sanitize (or (read-key-cache db (class-name class) key)
		  (read-kahua-instance db class key include-removed-object?)))))

(define (index-slot-type class slot-name)
  (or (and-let* ((slot (class-slot-definition class slot-name)))
	(slot-definition-option slot :index))
      (kahua-persistence-error "~s doesn't have such a slot: ~a" class slot-name)))

(define-method find-kahua-instance ((class <kahua-persistent-meta>)
				    (slot-name <symbol>)
				    slot-value . args)
  (let1 index-type (index-slot-type class slot-name)
    (case index-type
      ((:unique)
       (let1 res (make-kahua-collection class :index (cons slot-name slot-value)
					:include-removed-object? (get-optional args #f))
	 (and (< 0 (size-of res))
	      (car (map identity res)))))
      (else (kahua-persistence-error "~a is not a unique index slot" slot-name)))))

(define-method read-kahua-instance ((object <kahua-persistent-base>))
  ;; This is very transitional code.
  (guard (e (else (read-kahua-instance (current-db) (current-class-of object) (key-of object))))
    (read-kahua-instance (current-db) (current-class-of object) (kahua-persistent-id object))))

;;=========================================================
;; "View" as a collection
;;

;; NB: right now, we read all the instances into memory at once.
;; Later we can change the back-end to read it lazily.

(define-class <kahua-collection> (<collection>)
  ((class :init-keyword :class)
   (instances :init-keyword :instances :init-value '())))

(define-method make-kahua-collection ((class <kahua-persistent-meta>) . opts)
  (define-method append-map (proc (col <collection>))
    (fold (lambda (v r)
	    (append (proc v) r))
	  '()
	  col))
  (define-method class-subclasses ((class <class>))
    (define-method class-subclasses* ((class <class>))
      (let1 subs (class-direct-subclasses class)
	(if (null? subs)
	    '()
	    (append subs
		    (append-map class-subclasses* subs)))))
    (delete-duplicates (class-subclasses* class)))

  (let1 db (current-db)
    (unless db (error "make-kahua-collection: database not active"))
    (let-keywords* opts ((subclasses? :subclasses #f))
      (if subclasses?
          (append-map (lambda (c)
                        (coerce-to <list> (make-kahua-collection db c opts)))
                      (cons class
                            (class-subclasses class)))
          (make-kahua-collection db class opts)))))

(define (kahua-cached-instances db class opts . may-be-sweep?)
  (define (make-basic-filter class filter-proc)
    (lambda (obj)
      (and (eq? (class-name (current-class-of obj))
		(class-name class))
	   (filter-proc obj))))
  (let-keywords* opts ((index #f)
		       (keys #f)
		       (predicate #f)
		       (include-removed-object? #f))
    (cond ((or include-removed-object? (and index (get-optional may-be-sweep? #f)))
	   (let1 filter-proc (make-basic-filter class (make-kahua-collection-filter class opts))
	     (filter-map filter-proc (all-instances-on-id-cache db))))
	  (keys
	   (let* ((filter-proc (make-kahua-collection-filter class (delete-keyword :keys opts)))
		  (cname (class-name (current-class-of class))))
	     (filter-map (lambda (k)
			   (and-let* ((obj (read-key-cache db cname k))
				      ((not (removed? obj))))
			     (filter-proc obj)))
			 keys)))
	  (index
	   (let* ((filter-proc (make-kahua-collection-filter class (delete-keyword :index opts)))
		  (slot-name (car index))
		  (slot-value (cdr index))
		  (index-type (index-slot-type class slot-name)))
	     (or (and-let* ((objs (read-index-cache class slot-name slot-value)))
		   (case index-type
		     ((:unique) (list objs))
		     ((:any)    objs)
		     (else (kahua-persistence-error "Unknown index type: ~s" index-type))))
		 '())))
	  (else
	   (let1 filter-proc (make-basic-filter class (make-kahua-collection-filter class opts))
	     (filter-map (lambda (o) (and (not (removed? o)) (filter-proc o)))
			 (all-instances-on-id-cache db)))))))

(define-method make-kahua-collection ((db <kahua-db>)
                                      class opts)
  (receive (cache-sweep? persistent-sweep?)
      (or (and-let* ((index (get-keyword :index opts #f))
		     (metainfo (ensure-metainfo class))
		     (slot-name (car index))
		     (translator (assq slot-name (index-translator-of metainfo)))
		     (directive (list-ref translator 1)))
	    (values #t (eq? directive :add)))
	  (values #f #f))
    (let* ((cached-list (kahua-cached-instances db class opts cache-sweep?))
	   (persistent-list (kahua-persistent-instances db class opts persistent-sweep?)))
      (make <kahua-collection>
	:class class
	:instances (append! cached-list persistent-list)))))

(define (make-index-filter class index-cond)
  (and-let* (((pair? index-cond))
	     (sn (car index-cond))
	     (value (cdr index-cond))
	     (idx (index-slot-type class sn)))
    (lambda (obj)
      (equal? value (slot-ref obj sn)))))

(define (make-keys-filter class keys)
  (and keys
       (lambda (obj) (member (key-of obj) keys))))

(define (make-kahua-collection-filter class opts)
  (let-keywords* opts ((index #f)
		       (keys #f)
		       (predicate #f)
		       (include-removed-object? #f))
    (let* ((index-filter (make-index-filter class index))
	   (keys-filter (make-keys-filter class keys))
	   (include-removed-filter (and (not include-removed-object?)
					(lambda (o) (not (removed? o))))))
      (make-filter-pipeline `(,index-filter ,keys-filter ,include-removed-filter ,predicate)))))

(define-method call-with-iterator ((coll <kahua-collection>) proc . opts)
  (let1 p (ref coll 'instances)
    (proc (cut null? p)
          (lambda () (let1 r (car p) (pop! p) r)))))
    
;;
;; Support for Class Redefinition
;;

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
	    (unless (or (eq? new-class <kahua-persistent-base>)
			(not (slot-bound-using-class? old-class obj '%transaction-id))
			(current-transaction? obj))
	      (and-let* ((key (slot-ref-using-class old-class obj '%persistent-key)))
		(update-transaction! obj)
		(read-kahua-instance (current-db) old-class key)))
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
      (slot-set! obj '%kahua-persistent-base::id (%kahua-db-unique-id))
      (slot-set! obj '%persistent-generation 0)
      (slot-set! obj '%floating-instance #t))
    ;; restore persistent slots value.
    (add-modified-instance! (current-db) obj)
    (dolist (slot carry-over-slots)
      (if (pair? slot)
          (slot-set! obj (car slot) (cdr slot))
	  (let ((acc (class-slot-accessor new-class slot)))
	    (slot-initialize-using-accessor! obj acc '()))))
    obj))

;;
;; for check and fix database consistency.
;;

(define-generic dbutil:check-kahua-db-classcount)
(define-generic dbutil:check-kahua-db-idcount)
(define-generic dbutil:check-removed-flag-column)
(define-generic dbutil:check-removed-flag-column-for-all-tables)
(define-generic dbutil:check-alive-directory)

(define-generic dbutil:check&fix-database)

;;
;; Dummy Reader Constructor for database maintainance
;;
(define-class <dbutil:dummy-persistent-class> ()
  ((id :init-keyword :id)
   (removed? :init-keyword :removed? :init-value #f)
   (class-name :init-keyword :class-name)
   (generation :init-keyword :generation :init-value 0)
   (slot-values :init-keyword :slot-values)))
(define-class <dbutil:dummy-proxy-class> ()
  ((key :init-keyword :key)
   (class-name :init-keyword :class-name)))

(define (kahua-object-dummy-read class-desc id . vals)
  (receive (cn gen) (if (pair? class-desc)
			(apply values class-desc)
			(values class-desc 0))
    (make <dbutil:dummy-persistent-class>
      :id id :class-name cn :generation gen :slot-values vals)))
(define (kahua-object2-dummy-read class-desc id removed? . vals)
  (let1 obj (apply kahua-object-dummy-read class-desc id vals)
    (slot-set! obj 'removed? removed?)
    obj))
(define (kahua-proxy-dummy-read cname key)
  (make <dbutil:dummy-proxy-class> :key key :class-name cname))

(define (dbutil:switch-to-dummy-reader-ctor)
  (define-reader-ctor 'kahua-object kahua-object-dummy-read)
  (define-reader-ctor 'kahua-object2 kahua-object2-dummy-read)
  (define-reader-ctor 'kahua-proxy kahua-proxy-dummy-read))
(define (dbutil:restore-reader-ctor)
  (define-reader-ctor 'kahua-object kahua-object-read)
  (define-reader-ctor 'kahua-object2 kahua-object2-read)
  (define-reader-ctor 'kahua-proxy kahua-proxy-read))

(define (dbutil:with-dummy-reader-ctor thunk)
  (dynamic-wind
      dbutil:switch-to-dummy-reader-ctor
      thunk
      dbutil:restore-reader-ctor))

(define-method dbutil:persistent-classes-fold ((db <kahua-db>) proc knil)
  (let1 classes (map (cut ref <> 'name) (make-kahua-collection db <kahua-persistent-metainfo>
							       '(:include-removed-object? #t)))
    (dbutil:with-dummy-reader-ctor
     (lambda ()
       (fold proc knil (cons '<kahua-persistent-metainfo> classes))))))

(define-generic dbutil:kahua-db-fs->efs)

(provide "kahua/persistence")
