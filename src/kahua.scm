;; integrate Kahua related modules
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: kahua.scm,v 1.4 2004/02/02 09:15:23 tahara Exp $

(define-module kahua
  (extend kahua.config
          kahua.util
          kahua.gsid
          kahua.persistence
          kahua.user
          kahua.session
          kahua.server
          kahua.developer)
  )

(select-module kahua)

(provide "kahua")


