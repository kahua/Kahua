;; test kahua.persistence
;; Kahua.persistenceモジュールのテスト

;; $Id: persistence.scm,v 1.10 2006/01/28 02:52:31 shibata Exp $

(use gauche.test)
(use gauche.collection)
(use file.util)
(use util.list)

;; A hook to use this file for both stand-alone test and
;; DBI-backed-up test.
(define *dbname*
  (if (symbol-bound? '*dbname*)
    (begin (test-start #`"persistence/dbi (,*dbname*)")
           *dbname*)
    (begin (test-start "persistence")
           (build-path (sys-getcwd) "_tmp"))))
(sys-system #`"rm -rf ,*dbname*")

(define-syntax with-clean-db
  (syntax-rules ()
    ((_ (db dbpath) . body)
     (with-db (db dbpath)
       (kahua-db-purge-objs)
       . body))))

;; ロードテスト:
;;   kahua.persistentceがロードでき、またそのインタフェースに
;;   齟齬がないことを確認する。
(use kahua.persistence)
(test-module 'kahua.persistence)

;;----------------------------------------------------------
;; 基本的なテスト
(test-section "database basics")

;;  存在しないデータベース名を与えてデータベースをオープンし、
;;  データベースが正しく作成されることを確認する。
(test* "creating database" '(#t #t #t)
       (with-db (db *dbname*)
         (cons (is-a? db <kahua-db>)
               (if (is-a? db <kahua-db-fs>)
                 (list
                  (file-is-directory? *dbname*)
                  (file-exists? (build-path *dbname* "id-counter")))
                 (list
                  (is-a? (ref db 'connection) <dbi-connection>)
                  (is-a? (ref db 'query) <dbi-query>))))))

;;  データベースがwith-dbの動的スコープ中で有効であり、
;;  その外で無効になることを確認する。
(test* "database activeness" '(#t #f)
       (receive (db active?)
           (with-db (db *dbname*) (values db (ref db 'active)))
         (list active? (ref db 'active))))

;;----------------------------------------------------------
;; インスタンス作成テスト
(test-section "instances")

;;  新しい永続クラスを定義し、それが永続メタクラス<kahua-persistent-meta>に
;;  登録されていることを確認する。
(define-class <kahua-test> (<kahua-persistent-base>)
  ((quick :allocation :persistent :init-keyword :quick :init-value 'i)
   (quack :init-keyword :quack :init-value 'a)
   (quock :allocation :persistent :init-keyword :quock :init-value 'o))
  :source-id "Rev 1")

(define-method list-slots ((obj <kahua-test>))
  (map (cut ref obj <>) '(id quick quack quock)))

(define (get-test-obj id)
  (find-kahua-instance <kahua-test> (format "~6,'0d" id)))

(test* "metaclass stuff" (list #t <kahua-test>)
       (list (is-a? <kahua-test> <kahua-persistent-meta>)
             (find-kahua-class '<kahua-test>)))

;;  永続インスタンスを作成し、スロットが初期化されていること確認する。
(test* "creation (1)" '(1 ii aa "oo")
       (with-clean-db (db *dbname*)
         (list-slots (make <kahua-test> :quick 'ii :quack 'aa :quock "oo"))))

;;  再びトランザクションを開始し、先程作成した永続オブジェクトが得られる
;;  ことを確認する。
(test* "read (1)" '(1 ii a "oo")
       (with-clean-db (db *dbname*)
         (list-slots (get-test-obj 1))))

;;  ひとつのトランザクションでの変更が、次のトランザクションにも保持されて
;;  いることを確認する。
(test* "modify (1)" '(1 "II" a "oo")
       (begin
         (with-clean-db (db *dbname*)
           (set! (ref (get-test-obj 1) 'quick) "II"))
         (with-clean-db (db *dbname*)
           (list-slots (get-test-obj 1))))
       )

;;  もう一つの永続インスタンスを作成し、変更が可能であることを確認する。
;;  また、その変更が先程作成した永続インスタンスには影響しないことを
;;  確認する。
(test* "creation (2)" '(2 hh bb "pp")
       (with-clean-db (db *dbname*)
         (list-slots (make <kahua-test> :quick 'hh :quack 'bb :quock "pp"))))

(test* "modify (2)" '(2 "hh" a "PP")
       (begin
         (with-clean-db (db *dbname*)
           (set! (ref (get-test-obj 2) 'quick) "hh")
           (set! (ref (get-test-obj 2) 'quock) "PP"))
         (with-clean-db (db *dbname*)
           (list-slots (get-test-obj 2))))
       )

(test* "read (1)" '(1 "II" a "oo")
       (with-clean-db (db *dbname*)
         (list-slots (get-test-obj 1))))

;;  永続クラスの世代番号が正しく初期化されていること、すなわち
;;  in-memory世代もin-db世代も0であることを確認する。
(test* "generation" '(0 0)
       (with-clean-db (db *dbname*)
         (list (ref <kahua-test> 'generation)
               (ref <kahua-test> 'persistent-generation))))

;;----------------------------------------------------------
;; トランザクションに関するテスト
(test-section "transaction")

;;   永続インスタンス変更後にトランザクションをerrorで中断し、
;;   再びトランザクションを開始して、永続インスタンスが変更されて
;;   いないことを確認する。
(test* "abort transaciton" '(2 "hh" a "PP")
       (with-error-handler
           (lambda (e)
             (with-clean-db (db *dbname*)
               (list-slots (get-test-obj 2))))
         (lambda ()
           (with-clean-db (db *dbname*)
             (set! (ref (get-test-obj 2) 'quick) 'whoops)
             (error "abort!")))))

;;   永続インスタンス変更後に一度中間commitしてからまたインスタンスを
;;   変更し、トランザクションをerrorで中断する。
;;   再びトランザクションを開始して、永続インスタンスが中間commitまでの
;;   変更を受け、それ以降の変更は受けていないことを確認する。
(test* "commit & abort" '(2 whoops a "PP")
       (with-error-handler
           (lambda (e)
             (with-clean-db (db *dbname*)
               (list-slots (get-test-obj 2))))
         (lambda ()
           (with-clean-db (db *dbname*)
             (set! (ref (get-test-obj 2) 'quick) 'whoops)
             (kahua-db-sync db)
             (set! (ref (get-test-obj 2) 'quock) 'whack)
             (error "abort!")))))

;;----------------------------------------------------------
;; 永続オブジェクト間の参照に関するテスト
(test-section "references")

;;   永続オブジェクトへの参照を別の永続オブジェクトのスロットにセットし、
;;   コミットできることを確認する。
(test* "reference write" #t
       (with-clean-db (db *dbname*)
         (set! (ref (get-test-obj 1) 'quick) (get-test-obj 2))
         (is-a? (ref (get-test-obj 1) 'quick) <kahua-test>)))

;;   再びもとの永続オブジェクトを読み出し、参照先の永続オブジェクトも
;;   正しく読まれていることを確認する。
(test* "reference read" '(2 whoops a "PP")
       (with-clean-db (db *dbname*)
         (list-slots (ref (get-test-obj 1) 'quick))))

;;   ふたつの永続オブジェクトが相互に参照しあう構造を作成し、それが
;;   コミットできることを確認する。
(test* "circular reference write" '(#t #t)
       (with-clean-db (db *dbname*)
         (set! (ref (get-test-obj 2) 'quick) (get-test-obj 1))
         (list (eq? (get-test-obj 1) (ref (get-test-obj 2) 'quick))
               (eq? (get-test-obj 2) (ref (get-test-obj 1) 'quick)))))

;;   作成した循環参照構造を再び読み出し、構造が正しく再現されている
;;   ことを確認する。
(test* "circular reference read" '(#t #t)
       (with-clean-db (db *dbname*)
         (list (eq? (get-test-obj 1) (ref (get-test-obj 2) 'quick))
               (eq? (get-test-obj 2) (ref (get-test-obj 1) 'quick)))))

;;----------------------------------------------------------
;; クラス再定義
(test-section "class redefinition")

;(with-clean-db (db *dbname*) (kahua-db-purge-objs))

;; 永続クラスを再定義する。スロットの変更点は以下の通り。

;; Slot changes:
;;  quick - no change (persistent)
;;  quack - no change (transient)
;;  muick - added (persistent)
;;  quock - changed (persistent -> virtual)
(define-class <kahua-test> (<kahua-persistent-base>)
  ((quick :allocation :persistent :init-keyword :quick :init-value 'i)
   (quack :init-keyword :quack :init-value 'a)
   (muick :allocation :persistent :init-keyword :muick :init-value 'm)
   (quock :allocation :virtual
          :slot-ref  (lambda (o) (ref o 'quick))
          :slot-set! (lambda (o v) #f))  ;; to vanish
   )
  :source-id "Rev 2")

;;   オブジェクトマネージャに、再定義されたクラスが登録されていることを確認。
(test* "redefining class" <kahua-test>
       (find-kahua-class '<kahua-test>))

;;   インスタンスが新しいクラスに対応してアップデートされることを確認する。
(test* "updating instance for new class" #t
       (with-clean-db (db *dbname*)
         (eq? (ref (get-test-obj 1) 'quick)
              (ref (get-test-obj 1) 'quock))))

(test* "updating instance for new class" '(#t #t)
       (with-clean-db (db *dbname*)
         (list (equal? (list-slots (ref (get-test-obj 1) 'quock))
                       (list-slots (get-test-obj 2)))
               (equal? (list-slots (ref (get-test-obj 2) 'quock))
                       (list-slots (get-test-obj 1))))))

;;   アップデートしたインスタンスに対する変更がデータベースに反映されることを
;;   確認する。
(test* "redefining class (write)" '("M" "M" "M")
       (with-clean-db (db *dbname*)
         (set! (ref (get-test-obj 1) 'muick) '("M" "M" "M"))
         (ref (get-test-obj 1) 'muick)))

(test* "redefining class (read)" '("M" "M" "M")
       (with-clean-db (db *dbname*)
         (ref (get-test-obj 1) 'muick)))

;;   再定義した永続クラスの世代番号がインクリメントされていることを確認する。
(test* "generation" '(1 1)
       (with-clean-db (db *dbname*)
         (list (ref <kahua-test> 'generation)
               (ref <kahua-test> 'persistent-generation))))

;;----------------------------------------------------------
;; サブクラスのテスト
(test-section "subclassing")

;(with-clean-db (db *dbname*) (kahua-db-purge-objs))

;;   永続クラス<kahua-test>を継承したサブクラスを作成する。
(define-class <kahua-test-sub> (<kahua-test>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 3")

(define-method list-slots ((obj <kahua-test-sub>))
  (map (cut ref obj <>) '(id quick quack woo boo quock)))

(define-method key-of ((obj <kahua-test-sub>))
  (string-append (ref obj 'woo) (ref obj 'boo)))

;;   サブクラスの永続インスタンスが正しく作成され、継承されたスロット、
;;   追加されたスロット、共にデータが永続化されることを確認する。
(test* "write" '(4 "quick" "quack" "woo" "boo" "quock")
       (with-clean-db (db *dbname*)
         (list-slots
          (make <kahua-test-sub> :quick "quick" :quack "quack"
                :woo "woo" :boo "boo" :quock "quock"))))

(test* "read"  '(4 "quick" a "woo" "boo" "quock")
       (with-clean-db (db *dbname*)
         (list-slots (find-kahua-instance <kahua-test-sub> "wooboo"))))

(test* "write" '(5 i a "wooo" "booo" #f)
       (with-clean-db (db *dbname*)
         (list-slots
          (make <kahua-test-sub> :woo "wooo" :boo "booo"))))

(test* "read"  '(5 i a "wooo" "booo" #f)
       (with-clean-db (db *dbname*)
         (list-slots (find-kahua-instance <kahua-test-sub> "wooobooo"))))

;;   親クラスの永続インスタンスへの参照を含む構造を作成し、
;;   それがデータベースに反映されることを確認する。
(test* "reference to parent (write)" #t
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <kahua-test-sub> "wooboo")
           (set! (ref obj 'quick) (get-test-obj 1))
           (eq? (ref obj 'quick) (get-test-obj 1)))))

(test* "reference to parent (read)" #t
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <kahua-test-sub> "wooboo")
           (eq? (ref obj 'quick) (get-test-obj 1)))))

;;   この子クラスの世代番号が初期化されていることを確認する。
(test* "generation" '(0 0)
       (with-clean-db (db *dbname*)
         (list (ref <kahua-test-sub> 'generation)
               (ref <kahua-test-sub> 'persistent-generation))))

;;----------------------------------------------------------
;; 永続オブジェクトコレクション<kahua-collection>に関するテスト
(test-section "collection")

;;  <kahua-test>永続クラス、および<kahua-test-sub>永続クラスから
;;  そのクラスの永続インスタンスのコレクションが作成できることを
;;  確認する。
(test* "kahua-test" '(1 2)
       (sort (with-clean-db (db *dbname*)
               (map (cut ref <> 'id)
                    (make-kahua-collection <kahua-test>)))))

(test* "kahua-test-sub" '("woo" "wooo")
       (sort (with-clean-db (db *dbname*)
               (map (cut ref <> 'woo)
                    (make-kahua-collection <kahua-test-sub>)))))

;; This tests instance-by-key table initialization protocol
;; 永続コレクションの作成時に、in-memoryデータベースのインデックスハッシュが
;; 正しくセットアップされることを確認する。
(test* "kahua-test-sub" '((<kahua-test-sub> . "wooboo")
                          (<kahua-test-sub> . "wooobooo"))
       (with-clean-db (db *dbname*)
         (make-kahua-collection <kahua-test-sub>)
         (sort (hash-table-keys (ref db 'instance-by-key))
               (lambda (a b) (string<? (cdr a) (cdr b))))))

;; <kahua-test>と，そのsubclassである<kahua-test-sub>両者ともの
;; 永続インスタンスのコレクションを，<kahua-test>に対する
;; make-kahua-collectionを用いて作成できることを確認する．
(test* "kahua-test-subclasses" '(1 2 4 5)
       (sort (with-clean-db (db *dbname*)
               (map (cut ref <> 'id)
                    (make-kahua-collection <kahua-test> :subclasses #t)))))

;;----------------------------------------------------------
;; メタ情報履歴に関するテスト：永続クラスの変更をオブジェクトマネージャ
;; が認識し、世代番号を自動的に付与して管理していることを確認する。
(test-section "metainfo history")

;; Tests source-id change
;;   スロット定義を変えずにsource-idだけを変えた永続クラスを再定義し、
;;   永続クラスの世代番号が変化しないこと、変更したsource-idから世代番号への
;;   マッピングが正しく設定されていることを確認する。
(define-class <kahua-test-sub> (<kahua-test>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 4")

(test* "generation with source-id change"
       '("1B" 0 0 (0))
       (with-clean-db (db *dbname*)
         (let1 ins (make <kahua-test-sub>
                     :woo "1" :quick 'q1 :muick 'm1 :quock 'o1)
           (list (key-of ins)
                 (ref <kahua-test-sub> 'generation)
                 (ref <kahua-test-sub> 'persistent-generation)
                 (assoc-ref (ref (ref <kahua-test-sub> 'metainfo)
                                 'source-id-map)
                            "Rev 4")))))

;;   さらに、Source-idを戻した永続クラスを再定義し、永続スロット以外の
;;   定義変更では永続クラスの世代番号が変化しないこと、およびSource-idから
;;   世代番号へのマッピングに齟齬がないことを確認する。
(define <kahua-test-sub-save> <kahua-test-sub>)

(define-class <kahua-test-sub> (<kahua-test>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (bar :init-keyword :bar :init-value #f)
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 3")

(test* "generation with source-id change (revert source-id)"
       '("2B" 0 0 (0))
       (with-clean-db (db *dbname*)
         (let1 ins (make <kahua-test-sub> :woo "2")
           (list (key-of ins)
                 (ref <kahua-test-sub> 'generation)
                 (ref <kahua-test-sub> 'persistent-generation)
                 (assoc-ref (ref (ref <kahua-test-sub> 'metainfo)
                                 'source-id-map)
                            "Rev 3")))))

;;   Source-idを保ったまま永続クラスのスロット定義を変更し、永続クラスの
;;   世代番号が変更されること、および当該source-idからの世代番号へのマップが
;;   複数世代に設定されることを確認する。
(define-class <kahua-test-sub> (<kahua-test>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (bar :allocation :persistent :init-keyword :bar :init-value #f)
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 3")

(test* "generation with source-id change (update)" '(1 1 (1 0))
       (with-clean-db (db *dbname*)
         (make <kahua-test-sub>
           :woo "3" :quick 'q3 :muick 'm3 :quock 'o3 :bar 'b3)
         (list (ref <kahua-test-sub> 'generation)
               (ref <kahua-test-sub> 'persistent-generation)
               (assoc-ref (ref (ref <kahua-test-sub> 'metainfo)
                               'source-id-map)
                          "Rev 3"))))

;;   上記の定義を保ったまま永続クラスのsource-idを変更し、source-idから
;;   世代番号へのmany-to-manyのマッピングが正しく管理されていることを
;;   確認する。
(define-class <kahua-test-sub> (<kahua-test>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (bar :allocation :persistent :init-keyword :bar :init-value #f)
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 4")

(test* "generation with source-id change (change source-id)" '(1 1 (1 0))
       (with-clean-db (db *dbname*)
         (make <kahua-test-sub> :woo "4" :quock 'o4)
         (list (ref <kahua-test-sub> 'generation)
               (ref <kahua-test-sub> 'persistent-generation)
               (assoc-ref (ref (ref <kahua-test-sub> 'metainfo)
                               'source-id-map)
                          "Rev 4"))))

;;   親の永続クラスを再定義することによって<kahua-test-sub>の自動再定義を
;;   トリガし、その変更がデータベースのメタ情報履歴にも反映されることを
;;   確認する。
;;   slot change: drop muick.
(define-class <kahua-test> (<kahua-persistent-base>)
  ((quick :allocation :persistent :init-keyword :quick :init-value 'i)
   )
  :source-id "Rev 3")

(test* "generation with source-id change (change parent)" '(2 2 (2 1 0))
       (with-clean-db (db *dbname*)
         (make-kahua-collection <kahua-test-sub>)
         (list (ref <kahua-test-sub> 'generation)
               (ref <kahua-test-sub> 'persistent-generation)
               (assoc-ref (ref (ref <kahua-test-sub> 'metainfo)
                               'source-id-map)
                          "Rev 4"))))

;;   次のテストのために、もう一世代変更しておく。
;;   (親クラスで削除されたスロットmuickを子クラスで復活)
(define-class <kahua-test-sub> (<kahua-test>)
  ((woo   :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (bee :allocation :persistent :init-keyword :boo :init-value 'bee)
   (muick :allocation :persistent :init-keyword :muick :init-value #f))
  :source-id "Rev 5")

(test* "generation with source-id change (change source-id again)" '(3 3 (3))
       (with-clean-db (db *dbname*)
         (make <kahua-test-sub> :woo "5")
         (list (ref <kahua-test-sub> 'generation)
               (ref <kahua-test-sub> 'persistent-generation)
               (assoc-ref (ref (ref <kahua-test-sub> 'metainfo)
                               'source-id-map)
                          "Rev 5"))))

;;----------------------------------------------------------
;; インスタンスの世代間の変更に関するテスト：異なる世代の永続クラスで
;; 作成されたインスタンスにアクセスする際に、世代間の自動変換が行われる
;; ことを確認。以下のコメントでは、<kahua-test-sub>[n]で世代nの
;; <kahua-test-sub>クラスであることを表記する。
(test-section "instance translation")

;; テスト開始前に、現在の永続ストレージの内容を確認しておく。
;; 永続クラス<kahua-test-sub>の変遷は以下の通りである。
;; (世代[4]は以下のテスト中に定義される)
;;
;; generation   [0]        [1]         [2]         [3]         [4]
;; ----------------------------------------------------------------
;; p-slots     quick       quick       quick       quick
;;             muick       muick                   muick
;;             woo         woo         woo         woo         woo
;;             boo         boo         boo         boo         boo
;;             quock       quock       quock                   quock
;;                         bar         bar                     bar
;;                                                 bee         bee
;; 
;; source-id   "Rev 3"     "Rev 3"     "Rev 4"     "Rev 5"     "Rev 6"
;;             "Rev 4"     "Rev 4"
;; -------------------------------------------------------
;;
;; 現在の永続クラスの世代
;;   in-memory class:  <kahua-test-sub>[3]
;;   in-db     class:  <kahua-test-sub>[3]
;; 現在の永続インスタンスのin-dbの世代
;;   "wooboo"    [0]
;;   "woobooo"   [0]
;;   "1B"        [0]
;;   "2B"        [0]
;;   "3B"        [1]
;;   "4B"        [1]
;;   "5B"        [3]

;;   まず、<kahua-test-sub>[0]で作成された永続インスタンスを読み出し、
;;   それが<kahua-test-sub>[3]の構成にアップデートされていることを確認する。
(test* "translation [0]->[3]"
       '(:slots 
         ((quick . q1) (muick . m1) (woo . "1") (boo . "B") (bee . beebee))
         :hidden
         ((quock . o1))
         :instance-generation 0)
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <kahua-test-sub> "1B")
           (set! (ref obj 'bee) 'beebee)
           (list :slots (map (lambda (s) (cons s (ref obj s)))
                             '(quick muick woo boo bee))
                 :hidden (ref obj '%hidden-slot-values)
                 :instance-generation (ref obj '%persistent-generation)))))

;;   一旦 <kahua-test-sub> の定義を世代[2]に戻し、インスタンス"1B"に
;;   アクセス。世代[3]で削除されたスロットquockの値が復活していることを
;;   確認する。

(define-class <kahua-test-sub> (<kahua-test>)
  ((woo   :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (bar :allocation :persistent :init-keyword :bar :init-value #f)
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 4")

(test* "translation [3]->[2]"
       '(:class-generations
         (2 3)
         :slots
         ((quick . q1) (woo . "1") (boo . "B") (quock . o1) (bar . #t))
         :hidden
         ((bee . beebee) (muick . m1))
         :instance-generation 3)
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <kahua-test-sub> "1B")
           (set! (ref obj 'bar) #t)
           (list :class-generations
                 (list (ref <kahua-test-sub> 'generation)
                       (ref <kahua-test-sub> 'persistent-generation))
                 :slots (map (lambda (s) (cons s (ref obj s)))
                             '(quick woo boo quock bar))
                 :hidden (ref obj '%hidden-slot-values)
                 :instance-generation (ref obj '%persistent-generation)))))

;;   世代[1]のインスタンス"3B"にもアクセスし、それが世代[2]にアップデート
;;   されることを確認する。

(test* "translation [1]->[2]"
       '(:class-generations
         (2 3)
         :slots
         ((quick . q3) (woo . "3") (boo . "B") (quock . o3) (bar . b3))
         :instance-generation 1)
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <kahua-test-sub> "3B")
           (touch-kahua-instance! obj)
           (list :class-generations
                 (list (ref <kahua-test-sub> 'generation)
                       (ref <kahua-test-sub> 'persistent-generation))
                 :slots (map (lambda (s) (cons s (ref obj s)))
                             '(quick woo boo quock bar))
                 :instance-generation (ref obj '%persistent-generation)))))

;;   再び<kahua-test-sub>の定義を世代[3]に戻し、インスタンス"1B", "3B"に
;;   それぞれアクセスする。"1B"を世代[2]に戻した際に消えたスロット(bee)、
;;   及び、"3B"を世代[2]に移行した際に消えたスロット (muick) が復活している
;;   ことを確認する。また、各永続インスタンスの世代は最も進んだ世代のまま
;;   (すなわち、"1B"では[3], "3B"では[2])であることを確認する。

(define-class <kahua-test-sub> (<kahua-test>)
  ((woo   :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (bee :allocation :persistent :init-keyword :boo :init-value 'bee)
   (muick :allocation :persistent :init-keyword :muick :init-value #f))
  :source-id "Rev 5")

(test* "translation [2]->[3]"
       '(:class-generations
         (3 3)
         :slots
         (((quick . q1) (woo . "1") (boo . "B") (muick . m1) (bee . beebee))
          ((quick . q3) (woo . "3") (boo . "B") (muick . m3) (bee . bee)))
         :instance-generation (3 2))
       (with-clean-db (db *dbname*)
         (let1 objs
             (list (find-kahua-instance <kahua-test-sub> "1B")
                   (find-kahua-instance <kahua-test-sub> "3B"))
           (for-each touch-kahua-instance! objs)
           (list :class-generations
                 (list (ref <kahua-test-sub> 'generation)
                       (ref <kahua-test-sub> 'persistent-generation))
                 :slots (map (lambda (obj)
                               (map (lambda (s) (cons s (ref obj s)))
                                    '(quick woo boo muick bee)))
                             objs)
                 :instance-generation (map (cut ref <> '%persistent-generation)
                                           objs)))))

;; この段階での各インスタンスのin-dbの世代は次のようになっている。
;;   "wooboo"    [0]
;;   "wooobooo"  [0]
;;   "1B"        [3]
;;   "2B"        [0]
;;   "3B"        [3]
;;   "4B"        [1]
;;   "5B"        [3]

;;   今度は<kahua-test-sub>の定義を世代[0]まで戻す。世代[0]および[3]の
;;   永続インスタンス複数を読み出し、全てがin-memoryでは世代[0]の
;;   インスタンスになっていることを確認する。

(define-class <kahua-test> (<kahua-persistent-base>)
  ((quick :allocation :persistent :init-keyword :quick :init-value 'i)
   (quack :init-keyword :quack :init-value 'a)
   (muick :allocation :persistent :init-keyword :muick :init-value 'm)
   (quock :allocation :virtual
          :slot-ref  (lambda (o) (ref o 'quick))
          :slot-set! (lambda (o v) #f))
   )
  :source-id "Rev 2")

(define-class <kahua-test-sub> (<kahua-test>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   ;; this shadows parent's quock
   (quock :allocation :persistent :init-keyword :quock :init-value #f))
  :source-id "Rev 3")

(test* "translation [0]->[0]"
       '(:class-generations
         (0 3)
         :slots
         (((woo . "woo") (boo . "boo") (muick . m) (quock . "quock"))
          ((woo . "wooo") (boo . "booo") (muick . m) (quock . Q)))
         :instance-generation (0 0))
       (with-clean-db (db *dbname*)
         (let1 objs
             (list (find-kahua-instance <kahua-test-sub> "wooboo")
                   (find-kahua-instance <kahua-test-sub> "wooobooo"))
           (set! (ref (cadr objs) 'quock) 'Q)
           (list :class-generations
                 (list (ref <kahua-test-sub> 'generation)
                       (ref <kahua-test-sub> 'persistent-generation))
                 :slots (map (lambda (obj)
                               (map (lambda (s) (cons s (ref obj s)))
                                    '(woo boo muick quock)))
                             objs)
                 :instance-generation (map (cut ref <> '%persistent-generation)
                                           objs)))))

(test* "translation [3]->[0]"
       '(:slots
         (((woo . "1") (boo . "B") (muick . m1) (quock . o1))
          ((woo . "3") (boo . "B") (muick . m3) (quock . o3))
          ((woo . "5") (boo . "B") (muick . #f) (quock . QQ)))
         :instance-generation (3 3 3))
       (with-clean-db (db *dbname*)
         (let1 objs
             (list (find-kahua-instance <kahua-test-sub> "1B")
                   (find-kahua-instance <kahua-test-sub> "3B")
                   (find-kahua-instance <kahua-test-sub> "5B"))
           (set! (ref (caddr objs) 'quock) 'QQ)
           (list :slots (map (lambda (obj)
                               (map (lambda (s) (cons s (ref obj s)))
                                    '(woo boo muick quock)))
                             objs)
                 :instance-generation (map (cut ref <> '%persistent-generation)
                                           objs)))))


;;   次いで、<kahua-test-sub>を再定義する。今度は<kahua-test>を
;;   継承しない。この定義が世代[4]となることを確認する。また、
;;   各世代の永続インスタンスを読み込み、それらが新しい世代に
;;   アップデートされていること、世代間のtranslationで消えたスロット
;;   の値が失われていないこと、を確認する。

(define-class <kahua-test-sub> (<kahua-persistent-base>)
  ((woo :allocation :persistent :init-keyword :woo :init-value "W")
   (boo :allocation :persistent :init-keyword :boo :init-value "B")
   (quock :allocation :persistent :init-keyword :quock :init-value #f)
   (bar :allocation :persistent :init-keyword :bar :init-value #f)
   (bee :allocation :persistent :init-keyword :boo :init-value 'bee)
   )
  :source-id "Rev 6")

(test* "translation [0]->[4]"
       '(:class-generations
         (4 4)
         :slots
         (((woo . "woo") (boo . "boo") (quock . "quock") (bar . #f) (bee . bee))
          ((woo . "wooo") (boo . "booo") (quock . Q) (bar . wooobooo) (bee . bee))
          ((woo . "2") (boo . "B") (quock . #f) (bar . b2) (bee . bee)))
         :instance-generation (0 0 0))
       (with-clean-db (db *dbname*)
         (let1 objs
             (list (find-kahua-instance <kahua-test-sub> "wooboo")
                   (find-kahua-instance <kahua-test-sub> "wooobooo")
                   (find-kahua-instance <kahua-test-sub> "2B"))
           (set! (ref (cadr objs) 'bar) 'wooobooo)
           (set! (ref (caddr objs) 'bar) 'b2)
           (list :class-generations
                 (list (ref <kahua-test-sub> 'generation)
                       (ref <kahua-test-sub> 'persistent-generation))
                 :slots (map (lambda (obj)
                               (map (lambda (s) (cons s (ref obj s)))
                                    '(woo boo quock bar bee)))
                             objs)
                 :instance-generation (map (cut ref <> '%persistent-generation)
                                           objs)))))
       

(test* "translation [1]->[4]"
       '(:slots
         ((woo . "4") (boo . "B") (quock . o4) (bar . b4) (bee . bee))
         :instance-generation 1)
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <kahua-test-sub> "4B")
           (set! (ref obj 'bar) 'b4)
           (list :slots (map (lambda (s) (cons s (ref obj s)))
                             '(woo boo quock bar bee))
                 :instance-generation (ref obj '%persistent-generation)))))

(test* "translation [3]->[4]"
       '(:slots
         (((woo . "1") (boo . "B") (quock . o1) (bar . #t) (bee . beebee))
          ((woo . "3") (boo . "B") (quock . o3) (bar . b3) (bee . bee))
          ((woo . "5") (boo . "B") (quock . QQ) (bar . #f) (bee . bee)))
         :instance-generation (3 3 3))
       (with-clean-db (db *dbname*)
         (let1 objs
             (list (find-kahua-instance <kahua-test-sub> "1B")
                   (find-kahua-instance <kahua-test-sub> "3B")
                   (find-kahua-instance <kahua-test-sub> "5B"))
           (for-each touch-kahua-instance! objs)
           (list :slots (map (lambda (obj)
                               (map (lambda (s) (cons s (ref obj s)))
                                    '(woo boo quock bar bee)))
                             objs)
                 :instance-generation (map (cut ref <> '%persistent-generation)
                                           objs)))))

;;   上でアップデートした永続インスタンスのうち、変更を受けたか
;;   touch-kahua-instance! で「触られた」もののみ、永続インスタンスの
;;   世代が更新されていることを確認する。

(test* "translation (instances' persistent generations)"
       '(("1B" . 4) ("2B" . 4) ("3B" . 4) ("4B" . 4) ("5B" . 4)
         ("wooboo" . 0) ("wooobooo" . 4))
       (with-clean-db (db *dbname*)
         (sort
          (map (lambda (obj)
                 (cons (key-of obj)
                       (ref obj '%persistent-generation)))
               (make-kahua-collection <kahua-test-sub>))
          (lambda (a b)
            (string<? (car a) (car b))))))

;;----------------------------------------------------------
;; トランザクション管理のテスト
(test-section "transaction / default(read-only, no-sync)")

(define-class <transaction-test-1> (<kahua-persistent-base>)
  ((a :init-value 0 :init-keyword :a :allocation :persistent)))

(define-method key-of ((self <transaction-test-1>))
  "key")

(test "ref out of transaction" 1
      (lambda ()
        (let1 object (with-clean-db (db *dbname*)
                       (make <transaction-test-1> :a 1))
          (ref object 'a))))

(test "write in other transaction" #t
      (lambda ()
        (with-clean-db (db *dbname*)
          (let1 object (find-kahua-instance <transaction-test-1> "key")
            (set! (ref object 'a) 2)))
        #t))

(test "check (write in other transaction" 2
      (lambda ()
        (with-clean-db (db *dbname*)
          (let1 object (find-kahua-instance <transaction-test-1> "key")
            (ref object 'a)))))

(test "set! out of transaction" *test-error*
      (lambda ()
        (let1 object (with-clean-db (db *dbname*)
                       (make <transaction-test-1> :a 1))
          (set! (ref object 'a) 1)
          #t)))

(test-section "transaction / access denied")

(define-class <transaction-test-2> (<kahua-persistent-base>)
  ((a :init-value 0 :init-keyword :a :allocation :persistent
      :out-of-transaction :denied)))

(test "ref out of transaction" *test-error*
      (lambda ()
        (let1 object (with-clean-db (db *dbname*)
                       (make <transaction-test-2> :a 0))
          (ref object 'a))))

(test "ref in other transaction" 1
      (lambda ()
        (let1 object (with-clean-db (db *dbname*)
                       (make <transaction-test-2> :a 1))
          (with-clean-db (db *dbname*)
            (ref object 'a)))))

(test "set! out of transaction" *test-error*
      (lambda ()
        (let1 object (with-clean-db (db *dbname*)
                       (make <transaction-test-2> :a 0))
          (set! (ref object 'a) 1))))

(test-section "transaction / read-only auto-sync")

(define-class <transaction-test-3> (<kahua-persistent-base>)
  ((key :init-value #f :init-keyword :key :allocation :persistent)
   (a :init-value 0 :init-keyword :a :allocation :persistent))
  :read-syncer :auto)

(define-method key-of ((self <transaction-test-3>))
  (ref self 'key))

(define (geto key)
  (with-clean-db (db *dbname*)
    (find-kahua-instance <transaction-test-3> key)))

(test "ref out of transaction" 0
      (lambda ()
        (let1 object (with-clean-db (db *dbname*)
                       (make <transaction-test-3> :key "0" :a 0))
          (ref object 'a))))

(define (other-transaction num)
  (with-db (db *dbname*)
    (let1 object (geto "0")
      (set! (ref object 'a) num)))
  (sys-exit 0))

(test "write in other transaction" 1
      (lambda ()
        (let1 object (geto "0")
          (let1 pid (sys-fork)
            (if (= pid 0)
                (other-transaction 1)
                (begin
                  (sys-waitpid pid)
                  (with-db (db *dbname*) (ref object 'a))))))))

(test "overwrite object" 5
      (lambda ()
        (let1 object (geto "0")
          (let1 pid (sys-fork)
            (if (= pid 0)
                (other-transaction 2)
                (begin
                  (sys-waitpid pid)
                  (with-db (db *dbname*) (set! (ref object 'a) 5))
                  (with-db (db *dbname*) (ref object 'a))))))))

; (test-section "transaction / read/write auto-sync")
; (define-class <transaction-test-4> (<kahua-persistent-base>)
;   ((a :init-value 0 :init-keyword :a :allocation :persistent
;       :out-of-transaction :read/write))
;   :read-syncer  :auto
;   :write-syncer :auto)

; (define-method key-of ((self <transaction-test-4>))
;   "key")

; (define object #f)

; (test* "make" #t
;        (with-db (db *dbname*)
;          (set! object (make <transaction-test-4> :a 0))
;          #t))

; (test "write out of transaction" 1
;       (lambda () (set! (ref object 'a) 1) 1))

; ;; トランザクション開始時にon-memory cacheがdbに書き込まれ
; ;; ることを確認する。
; (test* "read in other transaction (auto synched: 1)" 1
;        (with-db (db *dbname*)
;          (ref (find-kahua-instance <transaction-test-4> "key") 'a)))

; ;; 前トランザクションで書き込まれたデータを別トランザクション
; ;; にて読み出せることを確認する。
; (test* "read in other transaction (auto synched: 2)" 1
;        (with-db (db *dbname*) (ref object 'a)))

;;----------------------------------------------------------
;; unboundなスロットのテスト
(test-section "unbound slot")

(define-class <unbound-slot-class> (<kahua-persistent-base>)
  ((normal :allocation :persistent :init-value 'val)
   (unbound :allocation :persistent)))

(define-method key-of ((self <unbound-slot-class>))
  (x->string (ref self 'normal)))

(test* "make unbound slot instance" '(val #f)
       (with-clean-db (db *dbname*)
         (let1 obj (make <unbound-slot-class>)
           (list (ref obj 'normal)
                 (slot-bound? obj 'unbound)
                 ))))


(test* "check unbound slot" '(val #f)
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <unbound-slot-class> "val")
           (list (ref obj 'normal)
                 (slot-bound? obj 'unbound)
                 ))))

;;----------------------------------------------------------
;; 初期化メソッドinitializeとpersistent-initialize methodのチェック
(test-section "initialize and persistent-initialize method")

(define-class <init-A> (<kahua-persistent-base>)
  ((base1 :allocation :persistent :init-value 0)
   (base2 :allocation :persistent :init-value 0)
   (key :init-value "a" :accessor key-of)))

(define-method persistent-initialize ((obj <init-A>) initargs)
  (update! (ref obj 'base1) (cut + <> 1)))

(define-method initialize ((obj <init-A>) initargs)
  (next-method)
  (update! (ref obj 'base2) (cut + <> 1)))


(test* "make first instance" '(1 1)
       (with-clean-db (db *dbname*)
         (let1 obj (make <init-A>)
           (list (ref obj 'base1)
                 (ref obj 'base2)))))

(test* "find instance" '(1 2)
       (with-clean-db (db *dbname*)
         (let1 obj (find-kahua-instance <init-A> "a")
           (list (ref obj 'base1)
                 (ref obj 'base2)))))

;;----------------------------------------------------------
;; 永続クラス再定義のチェック
(test-section "persistent class redefine")

(define-class <redefine-A> (<kahua-persistent-base>)
  ((base :allocation :persistent :init-value 0)
   (key :init-value "a" :accessor key-of)))

(define-class <redefine-B> (<kahua-persistent-base>)
  ((base :allocation :persistent :init-value 1)
   (key :init-value "b" :accessor key-of)))

(define *id* #f)
(define *id2* #f)

(test* "make first instance(1)" 0
       (with-db (db *dbname*)
         (let1 obj (make <redefine-A>)
           (set! *id* (ref obj 'id))
           (ref obj 'base))))

(redefine-class! <redefine-A> <redefine-B>)

(test* "redefine instance(1)" '(#f 0)
       (with-db (db *dbname*)
                (let1 obj (find-kahua-instance <redefine-A> "a")
                  (set! *id2* (ref obj 'id))
                  (list (eq? *id* (ref obj 'id))
                        (ref obj 'base)))))

(test* "find redefined instance(1)" '(#t 0)
       (with-clean-db (db *dbname*)
                (let1 obj (find-kahua-instance <redefine-B> "a")
           (list (eq? *id2* (ref obj 'id))
                 (ref obj 'base)))))

(define-class <redefine-C> (<kahua-persistent-base>)
  ((base :allocation :persistent :init-value 0)
   (key :init-value "c" :accessor key-of)))

(test* "make first instance(2)" 0
       (with-db (db *dbname*)
         (let1 obj (make <redefine-C>)
           (set! *id* (ref obj 'id))
           (ref obj 'base))))

(define-class <redefine-C> (<kahua-persistent-base>)
  ((base :allocation :persistent :init-value 1)
   (base2 :allocation :persistent :init-value 10)
   (key :init-value "c" :accessor key-of)))

(test* "find redefined instance(2)" '(#t 0 10)
       (with-clean-db (db *dbname*)
                (let1 obj (find-kahua-instance <redefine-C> "c")
           (list (eq? *id* (ref obj 'id))
                 (ref obj 'base)
                 (ref obj 'base2)))))

;;----------------------------------------------------------
;; 永続クラスと他のメタクラスを同時に使うチェック
;; 継承順序もチェック

(test-section "useing other metaclass")

(use gauche.mop.validator)

(define-class <valid-A> (<kahua-persistent-base> <validator-mixin>)
  ((number :allocation :persistent :init-value "0"
           :validator (lambda (obj value)
                        (if (not (string? value))
                            value
                          (string->number value))))))

(define-class <valid-B> (<validator-mixin> <kahua-persistent-base>)
  ((string :allocation :persistent :init-value "0"
           :validator (lambda (obj value)
                        (if (kahua-wrapper? value)
                            value
                          (x->string value))))))

(define-method key-of ((obj <valid-A>))
  "valid-a")

(define-method key-of ((obj <valid-B>))
  "valid-b")

(test* "make mixin instance" '(10 "(a b c)")
       (with-clean-db (db *dbname*)
         (let ((a-obj (make <valid-A>))
               (b-obj (make <valid-B>)))
           (slot-set! a-obj 'number "10")
           (slot-set! b-obj 'string '(a b c))
           (list (ref a-obj 'number)
                 (ref b-obj 'string)))))

(test* "find mixin instance" '(10 "(a b c)")
       (with-clean-db (db *dbname*)
         (let ((a-obj (find-kahua-instance <valid-A> "valid-a"))
               (b-obj (find-kahua-instance <valid-B> "valid-b")))
           (list (ref a-obj 'number)
                 (ref b-obj 'string)))))

(test-section "big date")

(define-class <big> (<kahua-persistent-base>)
  ((a :allocation :persistent)))

(define-method key-of ((obj <big>))
  "big")

(test* "make big instance" 100000
       (with-clean-db (db *dbname*)
         (let ((obj (make <big>)))
           (slot-set! obj 'a (make-string 100000 #\a))
           (string-length (ref obj 'a)))))

(test* "make big instance" 100000
       (with-clean-db (db *dbname*)
         (let ((obj (find-kahua-instance <big> "big")))
           (string-length (ref obj 'a)))))

(test-end)
