;; integrate Kahua related modules
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua.scm,v 1.2 2004/01/16 09:40:20 shiro Exp $

(define-module kahua
  (extend kahua.config
          kahua.gsid
          kahua.persistence
          kahua.user
          kahua.session
          kahua.server)
  )

(select-module kahua)

(provide "kahua")


