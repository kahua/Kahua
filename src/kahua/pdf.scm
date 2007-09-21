;; integrate PDF related modules
;;
;;  Copyright (c) 2003-2004 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003-2004 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: pdf.scm,v 1.1 2004/03/01 08:21:15 tahara Exp $

(define-module kahua.pdf
  (extend kahua.pdf.interp
          kahua.pdf.typeset
	  )
  )

(select-module kahua.pdf)

(provide "kahua/pdf")

