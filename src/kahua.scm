;; integrate Kahua related modules
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua.scm,v 1.7 2004/02/20 10:46:30 tahara Exp $

(define-module kahua
  (extend kahua.config
          kahua.util
          kahua.partcont
          kahua.gsid
          kahua.persistence
          kahua.user
          kahua.session
          kahua.server
          kahua.developer
	  kahua.elem
          kahua.plugin
          kahua.sandbox
	  )
  )

(select-module kahua)

(provide "kahua")


