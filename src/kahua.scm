;; integrate kahua-elua related modules
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua.scm,v 1.1 2003/12/11 05:39:11 nobsun Exp $

(define-module kahua
  (extend kahua.config
          kahua.gsid
	  kahua.html
          kahua.persistence
          kahua.user
          kahua.session
          kahua.server)
  )

(select-module kahua)

(provide "kahua")


