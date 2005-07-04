;; -*- coding: euc-jp ; mode: scheme -*-
;; MySQL’¥Ð’¥Ã’¥¯’¥¨’¥ó’¥É’¤Î’¥Æ’¥¹’¥È
;; $Id: persistence-dbi-mysql.scm,v 1.2 2005/07/04 05:09:21 nobsun Exp $

;; Notes:
;;  * ’¥Æ’¥¹’¥È’¥±’¡¼’¥¹’¼«’ÂÎ’¤Ïpersistence.scm’¤Î’¤â’¤Î’¤ò’»È’¤¦’¡£
;;  * mysqld’¤¬’Áö’¤Ã’¤Æ’¤ª’¤ê’¡¢’´Ä’¶­’ÊÑ’¿ô$USER’¤Î’¥¢’¥«’¥¦’¥ó’¥È’¤Ç’¥Ñ’¥¹’¥ï’¡¼’¥É’Ìµ’¤·’¤Ç
;;    ’¥í’¥°’¥¤’¥ó’¤Ç’¤­’¡¢'test'’¥Ç’¡¼’¥¿’¥Ù’¡¼’¥¹’¤¬’»È’¤¨’¤ë’¤³’¤È’¤ò’Á°’Äó’¤È’¤¹’¤ë’¡£

(use gauche.collection)
(use dbi)
(define *user* (sys-getenv "USER"))
(define *dbname* #`"mysql:,|*user*|::db=test")

;; ’Á°’²ó’¤Î’¥Æ’¥¹’¥È’¤Ç’ºî’¤é’¤ì’¤¿’¥Æ’¡¼’¥Ö’¥ë’¤¬’»Ä’¤Ã’¤Æ’¤¤’¤ì’¤Ð’¤½’¤ì’¤ò’¥¯’¥ê’¥¢’¤·’¤Æ’¤ª’¤¯
(cleanup-db "mysql" *user* "" "db=test")

(load "./persistence.scm")
