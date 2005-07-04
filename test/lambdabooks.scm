;; -*- coding: euc-jp ; mode: scheme -*-
;; test lambdabooks scripts.
;; this test isn't for modules, but the actual scripts.
;; $Id: lambdabooks.scm,v 1.3 2005/07/04 05:09:21 nobsun Exp $

(use srfi-2)
(use srfi-11)
(use gauche.test)
(use gauche.process)
(use gauche.net)
(use rfc.uri)
(use util.list)
(use text.tree)
(use kahua)
(use kahua.test.xml)
(use kahua.test.worker)

(use kahua.persistence) 
(use kahua.user)
(use file.util)

(test-start "lambdabooks test scripts")

(sys-system "rm -rf _tmp _work")
(sys-mkdir "_tmp" #o755)
(sys-mkdir "_work" #o755)
(sys-mkdir "_work/plugins" #o755)
(copy-file "../plugins/allow-module.scm"  "_work/plugins/allow-module.scm")

(define *config* "./test.conf")

(kahua-init *config*)

;;------------------------------------------------------------
;; Page pattern
;;
(define href-on '(@ (href ?&)))
(define href-off '?@)

(define (page-template header footer body)
  `(html (head (title "Lambda books"))
	 (body (@ (style "background-color: #ffffff"))
	       ,@header
	       ,@body
	       ,@footer)))

(define (header-template flg)
  (let ((link-to-top (if flg href-on href-off)))
    `((a ,link-to-top (img ?@))
      (br)
      (img ?@))))

(define (footer-template flg)
  (let ((link-to-top (if flg href-on href-off)))
    `((img ?@)
      (div (@ (style "text-align: right"))
	   "Powered by "
	   (a ,link-to-top "Kahua")))))

(define (body-template side-bar contents)
  (if side-bar
      `((table 
	 (tr ,@side-bar
	     ,@contents)))
      contents))

(define (side-bar-pattern user role navi)
  (define on href-on)
  (define off href-off)
  (cond ((eq? user 'anonymous)
	 (let-values (((link-1 link-2 link-3)
		       (cond ((eq? navi 'login)
			      (values on off off))
			     ((eq? navi 'books)
			      (values off on off))
			     ((eq? navi 'partnership)
			      (values off off on))
			     (else
			      (values off off off)))))
	   `((td (@ (valign "top"))
		 (table 
		  (tr (td (a ,link-1 "ログイン/新規登録")))
		  (tr (td (a ,link-2 "Books")))
		  (tr (td (a ,link-3 "Partnership"))))))))
	((eq? role 'admin) 
	 (let-values (((link-1 link-2 link-3 link-4)
			(cond ((eq? navi 'logout)
			       (values on off off off))
			      ((eq? navi 'books)
			       (values off on off off))
			      ((eq? navi 'partnership)
			       (values off off on off))
			      ((eq? navi 'user-management)
			       (values off off off on))
			      (else
			       (values off off off off)))))
	   `((td (@ (valign "top"))
		 (table 
		  (tr (td (table
			       (tr (td "Welcome, guest"))
			       (tr (td ?@ (a ,link-1 "logout"))))))
		  (tr (td (a ,link-2 "Books")))
		  (tr (td (a ,link-3 "Partnership")))
		  (tr (td (a ,link-4 "[ユーザ管理」"))))))))
	(else
	 (let-values (((link-1 link-2 link-3)
			(cond ((eq? navi 'logout)
			       (values on off off))
			      ((eq? navi 'books)
			       (values off on off))
			      ((eq? navi 'partnership)
			       (values off off on))
			      (else
			       (values off off off)))))
	   `((td (@ (valign "top"))
		 (table 
		  (tr (td (table
			       (tr (td "Welcome, guest"))
			       (tr (td ?@ (a ,link-1 "logout"))))))
		  (tr (td (a ,link-2 "Books")))
		  (tr (td (a ,link-3 "Partnership"))))))))))

(define (top-page-contents-pattern book-select)
  (let-values (((link-1 link-2)
		(cond ((eq? book-select '1st)
		       (values href-on href-off))
		      ((eq? book-select '2nd)
		       (values href-off href-on))
		      (else
		       (values href-off href-off)))))
    `((td (@ (valign "top"))
	  (h2 "最新ニュース")
	  (p "本サイトは、Kahuaフレームワークのデモサイトです。")
	  (h2 "新刊情報")
	  (p "サイト開設記念特別キャンペーン価格にて販売中!")
	  (table 
	   (tr (td (b "Structure and Interpretation of Computer Programs")))
	   (tr (td (@ (style "text-align: right"))
		  "Hans Abelson, Gerald Jay Sussman著"))
	   (tr (td (@ (style "text-align: right"))
		   "キャンペーン価格：5,600円"))
	   (tr (td (@ (style "text-align: right"))
		   (a ,link-1
		      "詳しい情報を見る...")))
	   (tr (td (b "The Little Schemer")))
	   (tr (td (@ (style "text-align: right"))
		   "Daniel P Friedman, Mattias Felleisen著"))
	   (tr (td (@ (style "text-align: right"))
		   "キャンペーン価格：2,000円"))
	   (tr (td (@ (style "text-align: right"))
		   (a ,link-2 "詳しい情報を見る..."))))))))
  
(define (book-list-page-contents-pattern page-num book-select)
  (define on href-on)
  (define off href-off)
  (cond ((= page-num 1)
	 (let-values (((bk1 bk2 bk3 bk4 bk5 next)
		       (cond ((eq? book-select 1) 
			      (values on off off off off off))
			     ((eq? book-select 2)
			      (values off on off off off off))
			     ((eq? book-select 3)
			      (values off off on off off off))
			     ((eq? book-select 4)
			      (values off off off on off off))
			     ((eq? book-select 5)
			      (values off off off off on off))
			     ((eq? book-select 'next)
			      (values off off off off off on))
			     (else
			      (values off off off off off off)))))
           `((td (@ (valign "top"))
		 (h2 "書籍リスト")
		 (table 
		  (tr (td ?@ (a ,bk1 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk2 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk3 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk4 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk5 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*)))
		 (p (a ,next "[次の5項目]"))))))
	((= page-num 2)
	 (let-values (((bk1 bk2 bk3 bk4 bk5 prev next)
		       (cond ((eq? book-select 1) 
			      (values on off off off off off off))
			     ((eq? book-select 2)
			      (values off on off off off off off))
			     ((eq? book-select 3)
			      (values off off on off off off off))
			     ((eq? book-select 4)
			      (values off off off on off off off))
			     ((eq? book-select 5)
			      (values off off off off on off off))
			     ((eq? book-select 'prev)
			      (values off off off off off on off))
			     ((eq? book-select 'next)
			      (values off off off off off off on))
			     (else
			      (values off off off off off off off)))))
           `((td (@ (valign "top"))
		 (h2 "書籍リスト")
		 (table 
		  (tr (td ?@ (a ,bk1 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk2 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk3 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk4 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk5 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*)))
		 (p (a ,prev "[前の5項目]") (a ,next "[次の5項目]"))))))
	((= page-num 3)
	 (let-values (((bk1 bk2 bk3 prev)
		       (cond ((eq? book-select 1) 
			      (values on off off off))
			     ((eq? book-select 2)
			      (values off on off off))
			     ((eq? book-select 3)
			      (values off off on off))
			     ((eq? book-select 'prev)
			      (values off off off on))
			     (else
			      (values off off off off)))))
           `((td (@ (valign "top"))
		 (h2 "書籍リスト")
		 (table 
		  (tr (td ?@ (a ,bk1 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk2 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*))
		  (tr (td ?@ (a ,bk3 ?*)))
		  (tr (td ?@ ?*) (td ?@ ?*)))
		 (p (a ,prev "[前の5項目]"))))))))

(define (book-page-contents-pattern user link)
  (cond ((eq? user 'anonymous)
	 (let-values (((read-review write-review books-list)
		       (cond ((eq? link 'read) 
			      (values 
			       `(p ?* (a ,href-on "[読者レビューを読む]"))
			       `(p "(" (a ,href-off "ログイン/新規登録")
				   "すると、この本についてのレビューを書くことができます)")
			       href-off))
			     ((eq? link 'write) 
			      (values 
			       `(p ?*)
			       `(p "(" (a ,href-on "ログイン/新規登録")
				   "すると、この本についてのレビューを書くことができます)")
			       href-off))
			     ((eq? link 'list) 
			      (values `(p ?*) `(p ?*) href-on))
			     (else (values `(p ?*) `(p ?*) href-off)))))
	   `((td (@ (valign "top"))
		 (h2 ?*)
		 (table (tr (th "著者") (td ?*))
			(tr (th "出版社") (td ?*))
			(tr (th "価格") (td ?*)))
		 (hr)
		 (p ?*)
		 (hr)
		 ,read-review
		 ,write-review
		 (p ?@ (a ,books-list "[書籍一覧へ]"))))))
	(else
	 (let-values (((read-review write-review books-list)
		       (cond ((eq? link 'read) 
			      (values 
			       `(p ?* (a ,href-on "[読者レビューを読む]"))
			       `(p "(" (a ,href-off "ログイン/新規登録")
				   "すると、この本についてのレビューを書くことができます)")
			       href-off))
			     ((eq? link 'write) 
			      (values 
			       `(p ?*)
			       `(p (a ,href-on 
				      "[この本についてのレビューを書く]"))
			       href-off))
			     ((eq? link 'list) 
			      (values `(p ?*) `(p ?*) href-on))
			     (else (values `(p ?*) `(p ?*) href-off)))))
	   `((td (@ (valign "top"))
		 (h2 ?*)
		 (table (tr (th "著者") (td ?*))
			(tr (th "出版社") (td ?*))
			(tr (th "価格") (td ?*)))
		 (hr)
		 (p ?*)
		 (hr)
		 ,read-review
		 ,write-review
		 (p ?@ (a ,books-list "[書籍一覧へ]"))))))))

(define (login-page-contents-pattern flg)
  (let-values (((login register to-top)
		(cond ((eq? flg 'login)
		       (values '(@ (action ?&) ?*)
			       href-off
			       href-off))
		      ((eq? flg 'register)
		       (values '?@ href-on href-off))
		      ((eq? flg 'to-top)
		       (values '?@ href-off href-on))
		      (else
		       (values '?@ href-off href-off)))))

    `((p ?*)
      (form ,login
	    (table (tr (th "ユーザー名")
		       (td (input ?@)))
		   (tr (th "パスワード")
		       (td (input ?@)))
		   (tr (th)
		       (td (input ?@)))))
      (p "ユーザー登録がお済みでない方は、"
	 (a ,register "新規ユーザー登録")
	 "で登録を行うか、"
	 (a ,to-top "ログインせずにサイトを利用する")
	 "ことができます。(ログインしない場合は、一部の機能が使えません)"))))

;;------------------------------------------------------------
;; Run lambdabooks
(test-section "kahua-server lambdabooks.kahua")

(sys-system "gosh -I../src -I../examples lambdabooks.init -c ./test.conf")

(with-worker
 (w `("gosh" "-I../src" "-I../examples" "../src/kahua-server.scm"
      "-c" ,*config* "../examples/lambdabooks/lambdabooks.kahua"))
    
 (test* "run lambdabooks.kahua" #t (worker-running? w))

 (test* "start lambdabooks"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f 'books)
                                      (top-page-contents-pattern #f)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "book list page (1)"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f #f)
                                      (book-list-page-contents-pattern 1 'next)
                                      ))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))


 (test* "book list page (2)"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f #f)
                                      (book-list-page-contents-pattern 2 'next)
                                      ))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "book list page (3)"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f #f)
                                      (book-list-page-contents-pattern 3 'prev)
                                      ))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "book list page (2) again"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f #f)
                                      (book-list-page-contents-pattern 2 'prev)
                                      ))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "book list page (1) again"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f #f)
                                      (book-list-page-contents-pattern 1 1)
                                      ))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "sicp page"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f #f)
                                      (book-page-contents-pattern
                                       'anonymous 'list)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "book list page (1) again"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'anonymous #f 'login)
                                      (book-list-page-contents-pattern
                                       1 #f)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "login page"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template #f
                                      (login-page-contents-pattern 'login)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "top page after login as guest"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'guest #f #f)
                                      (top-page-contents-pattern '1st)))
        (call-worker/gsid w
                          '()
                          '(("logname" "guest") ("pass" "guest"))
                          (lambda (h b) (tree->string b)))
        (make-match&pick w))

 (test* "book page as guest"
        (page-template (header-template #f)
                       (footer-template #f)
                       (body-template (side-bar-pattern 'guest #f #f)
                                      (book-page-contents-pattern 'guest 'write)))
        (call-worker/gsid w '() '() (lambda (h b) (tree->string b)))
        (make-match&pick w))
 )

(test-end)
