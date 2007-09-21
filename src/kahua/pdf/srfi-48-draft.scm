#|
Title

Intermediate Format Strings

Author

Ken Dickey

Status

This SRFI is currently in ``draft'' status. To see an explanation of each
status that a SRFI can hold, see here. It will remain in draft status until
2004/02/25, or as amended. To provide input on this SRFI, please mail to
<srfi-48@srfi.schemers.org>. See instructions here to subscribe to the list.
You can access previous messages via the archive of the mailing list.

 ・ Received: 2003/11/24
 ・ Draft: 2003/11/25-2004/02/25
 ・ Revised: 2003/12/04
 ・ Revised: 2003/12/28

Abstract

This document specifies Format Strings, a method of interpreting a Scheme
string which contains a number of format directives that are replaced with
other string data according to the semantics of each directive. This SRFI
extends SRFI-28 in being more generally useful but is less general than
advanced format strings in that it does not allow, aside from ~F, for
controlled positioning of text within fields.

Issues

Some may disagree with specific escape options or return values. For those who
desire complex options as implemented by SLIB or Common Lisp's FORMAT, an
upwards compatible "Advanced Format" SRFI should be proposed.

In particular, the reference implementation given here does not accept numeric
arguments (aside from ~F). Hence it does not support SRFI-29.

It is highly desireable that baseline library code be small, attempt to
eliminiate heap allocation and bound stack usage. This is especially important
in embedded systems. This can be accomplished by writing directly to a port,
rather than a string, by not supporting ~W or ~F, and by replacing (display
(number->string n r) p) with a carefully written (display:number->string n r
p) which does not build intermediate strings.

As this is intermediate format, it was felt that ~F and ~W are too highly
useful to elide. The ~H option is helpful to users, allows for programattic
query, and makes clear which format directives are supported.

Rationale

Inheriting from MacLisp, nearly all Lisp and Scheme implementations support
some form of FORMAT function with support for various numbers of format
directives. By agreeing to the options here, we raise the bar for portable
code.

The reference implementation is R5RS compliant and easy to port. In not
requiring advanced features (aside from ~W and ~F) small implementations are
possible. E.g. the reference code does not use side effects (assignment) and
is less than a third the source size of the latest SLIB implementation of
FORMAT (less than a tenth if ~F support is elided).

The optional port argument allows for compatibility with older code written
for, e.g. scheme48, MIT Scheme, T, et cetera, which required a port argument.
It is also useful in cases where a synoptic implementation of Scheme and
CommonLisp is maintained.

Specification

format [port] format-string [obj ...]


    Accepts a format template (a Scheme String), and processes it, replacing
    any format directives in order with one or more characters, the characters
    themselves dependent on the semantics of the format directive encountered.
    Each directive may consume one obj. It is an error if fewer or more obj
    values are provided than format directives that require them.

    When a port is specified it must be either an output port or a boolean. If
    an output-port is specified, the formatted output is output into that
    port. If the port argument is #t, output is to the current-output-port. If
    the port is #f or no port is specified, the output is returned as a
    string. If the port is specified and is #t or an output-port, the result
    of the format function is unspecified.

    It is unspecified which encoding is used (e.g. ASCII, EBCDIC, UNICODE). A
    given implementation must specify which encoding is used. The
    implementation may or may not allow the encoding to be selected or
    changed.

    It is an error if an format directive consumes an obj argument and that
    argument does not confirm to a required type as noted in the table below.

    It is permissible, but highly discouraged, to implement pretty-print as
    (define pretty-print write).

    An format directive is a two character sequence in the string where the
    first character is a tilde '~'. Directive characters are case-independent,
    i.e. upper and lower case characters are interpreted the same. Each
    directive code's meaning is described in the following table:

    DIRECTIVE MNEMONIC      ACTION                                  CONSUMES?
    ~a        Any           (display obj) for humans                yes
    ~s        Slashified    (write obj) for parsers                 yes
    ~w        WriteCircular (write-with-shared-structure obj) like  yes
                            ~s, but handles recursive structures
    ~d        Decimal       the obj is a number which is output in  yes
                            decimal radix
    ~x        heXadecimal   the obj is a number which is output in  yes
                            hexdecimal radix
    ~o        Octal         the obj is a number which is output in  yes
                            octal radix
    ~b        Binary        the obj is a number which is output in  yes
                            binary radix
    ~c        Character     the single charater obj is output by    yes
                            write-char
    ~y        Yuppify       the list obj is pretty-printed to the   yes
                            output
                            the obj is another format-string and
    ~?        Indirection   the following obj is a list of          yes
                            arguments; format is called recursively
                            the same as ~? for backward
    ~K        Indirection   compatability with some existing        yes
                            implementations
                            ~w,dF outputs a number with width w and
    ~[w[,d]]F Fixed         d digits after the decimal; ~wF outputs yes
                            a string or number with width w.
    ~~        Tilde         output a tilde                          no
    ~t        Tab           output a tab character                  no
    ~%        Newline       output a newline character              no
                            output a newline character if it is
    ~&        Freshline     known that the previous output was not  no
                            a newline
    ~_        Space         a single space character is output      no
                            outputs one line of call synopsis, one
    ~h        Help          line of comment, and one line of        no
                            synopsis for each format directive,
                            starting with the directive (e.g. "~t")


    The ~F, fixed format, directive requires some elucidation.

    ~wF is useful for strings or numbers. Where the string (or number->string
    of the number) has fewer characters than the integer width w, the string
    is padded on the left with space characters.

    ~w,dF is typically used only on numbers. For strings, the d specifier is
    ignored. For numbers, the integer d specifies the number of decimal digits
    after the decimal place. Both w and d must be zero or positive.

    If d is specified, the number is processed as if added to 0.0, i.e. it is
    converted to an inexact value.

    (format "~8,2F" 1/3) => "    0.33"

    If no d is specified, the number is not coerced to inexact.
    (format "~6F" 32) => "    32"

    Digits are padded to the right with zeros
    (format "~8,2F" 32) => "   32.00"

    If the number it too large to fit in the width specified, a string longer
    than the width is returned
    (format "~1,2F" 4321) => "4321.00"

    If the number is complex, d is applied to both real and imaginal parts
    (format "~1,2F" (sqrt -3.9)) => "0.00+1.97i"

    For very large or very small numbers, the point where exponential notation
    is used is implementation defined.

    (format "~8F" 32e5) => "   3.2e6" or "3200000.0"


Examples

(format "~h")
; =>
"(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- This implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next arg is a format-string and the following arg a list of arguments
~K      [Indirection]   same as ~?
"

(format "Hello, ~a" "World!")
; => "Hello, World!"

(format "Error, list is too short: ~s" '(one "two" 3))
; => "Error, list is too short: (one \"two\" 3))"

(format "test me")
; => "test me"

(format "~a ~s ~a ~s" 'this 'is "a" "test")
; => "this is a \"test\""

(format #t "#d~d #x~x #o~o #b~b~%" 32 32 32 32)
;; Prints:   #d32 #x20 #o40 #b100000
; => <unspecified>

(format "~a ~? ~a" 'a "~s" 'new 'test)
; =>"a new test"

(format #f "~&1~&~&2~&~&~&3~%")
; =>
"
1
2
3
"

(format #f "~a ~? ~a ~%" 3 " ~s ~s " '(2 2) 3)
; =>
"3  2 2  3
"

(format "~w" (let ( (c '(a b c)) ) (set-cdr! (cddr c) c) c))
; => "#1=(a b c . #1#)"

(format "~8,2F" 32)
; => "   32.00"

(format "~8,3F" (sqrt -3.8))
; => "0.000+1.949i"

(format "~8,2F" 3.4567e11)
; => " 3.45e11"

(format "~6,3F" 1/3)
; => " 0.333"

(format "~4F" 12)
; => "  12"

(format "~8,3F" 123.3456)
; => " 123.346"

 (format "~6,3F" 123.3456)
; => "123.346"

 (format "~2,3F" 123.3456)
; => "123.346"

(format "~8,3F" "foo")
; => "     foo"


Implementation

The implementation below requires SRFI-6 (Basic string ports), SRFI-23 (Error
reporting mechanism) and SRFI-38 (External Representation for Data With Shared
Structure).
|#
;; IMPLEMENTATION DEPENDENT options

(define ascii-tab   (integer->char  9))  ;; NB: assumes ASCII encoding
(define dont-print  (if (eq? #t #f) 1))
;;(define DONT-PRINT (string->symbol ""))
;;(define DONT-PRINT (void))
(define pretty-print   write) ; ugly but permitted
;;(require 'srfi-38)  ;; write-with-shared-structure


;; FORMAT

(define (format . args)
  (cond
   ((null? args)
    (error "FORMAT: required format-string argument is missing")
    )
   ((string? (car args))
    (apply format (cons #f args)))
   ((< (length args) 2)
    (error (format #f "FORMAT: too few arguments ~s" (cons 'format args)))
    )
   (else
    (let ( (output-port   (car  args))
           (format-string (cadr args))
           (args          (cddr args))
         )
      (letrec ( (port
                 (cond ((output-port? output-port) output-port)
                       ((eq? output-port #t) (current-output-port))
                       ((eq? output-port #f) (open-output-string))
                       (else (error
                              (format #f "FORMAT: bad output-port argument: ~s"
                                      output-port)))
                ) )
                (return-value
                 (if (eq? output-port #f)    ;; if format into a string
                     (lambda () (get-output-string port)) ;; then return the string
                     (lambda () dont-print)) ;; else do something harmless
                 )
             )

         (define (round* n scale) ;; assert scale < 0
           (let ( (one (expt 10 (- scale))) )
             (/ (round (* n one)) one)))

         (define (string-index str c)
           (let ( (len (string-length str)) )
             (let loop ( (i 0) )
               (cond ((= i len) #f)
                     ((eqv? c (string-ref str i)) i)
                     (else (loop (+ i 1)))))))

         (define (string-grow str len char)
           (let ( (off (- len (string-length str))) )
             (if (positive? off)
               (string-append (make-string off char) str)
               str)))

         (define (string-pad-right str len char)
           (let ( (slen (string-length str)) )
             (cond ((< slen len)
                    (string-append str (make-string (- len slen) char)))
                   ((> slen len)
                    (substring str 0 len))
                   (else str))))

         (define (format-fixed number-or-string width digits) ; returns a string
           (cond
            ((string? number-or-string)
             (string-grow number-or-string width #\space)
             )
            ((number? number-or-string)
             (let ( (real (real-part number-or-string))
                    (imag (imag-part number-or-string))
                  )
               (cond
                ((not (zero? imag))
                 (string-grow
                  (string-append (format-fixed real 0 digits)
                                 (if (negative? imag) "" "+")
                                 (format-fixed imag 0 digits)
                                 "i")
                  width
                  #\space)
                 )
                (digits
                 (let* ( (rounded-number (exact->inexact (round* real (- digits))))
                         (rounded-string (number->string rounded-number))
                         (dot-index (string-index  rounded-string #\.))
                         (exp-index (string-index  rounded-string #\e))
                         (length    (string-length rounded-string))
                         (pre-string
                          (cond
                           (exp-index
                            (if dot-index
                                (substring rounded-string 0 (+ dot-index 1))
                                (substring rounded-string 0 (+ exp-index 1)))
                            )
                           (dot-index
                            (substring rounded-string 0 (+ dot-index 1))
                            )
                           (else
                            rounded-string))
                          )
                         (exp-string
                          (if exp-index (substring rounded-string exp-index length) "")
                          )
                         (frac-string
                          (if exp-index
                              (substring rounded-string (+ dot-index 1) exp-index)
                              (substring rounded-string (+ dot-index 1) length))
                          )
                       )
;;                 (format #t "~%@@DEBUG: str=~s: pre=~s frac=~s exp=~s" rounded-string pre-string frac-string exp-string)
                   (string-grow
                    (string-append pre-string
                                   (if dot-index "" ".")
                                   (string-pad-right frac-string digits #\0)
                                   exp-string)
                    width
                    #\space)
                 ))
                (else ;; no digits
                 (string-grow (number->string real) width #\space)))
             ))
            (else
             (error
              (format "FORMAT: ~F requires a number or a string, got ~s" number-or-string)))
            ))

         (define documentation-string
"(format []  [...]) --  is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- This implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next arg is a format-string and the following arg a list of arguments
~K      [Indirection]   same as ~?
"
          )

         (define (format-help format-strg arglist)

          (letrec (
             (length-of-format-string (string-length format-strg))

             (anychar-dispatch
              (lambda (pos arglist last-was-newline)
                (if (>= pos length-of-format-string)
                  arglist ; return unused args
                  (let ( (char (string-ref format-strg pos)) )
                    (cond
                     ((eq? char #\~)
                      (tilde-dispatch (+ pos 1) arglist last-was-newline))
                     (else
                      (write-char char port)
                      (anychar-dispatch (+ pos 1) arglist #f)
                      ))
                    ))
             )) ; end anychar-dispatch

             (tilde-dispatch
              (lambda (pos arglist last-was-newline)
                (cond
                 ((>= pos length-of-format-string)
                  (write-char #\~ port) ; tilde at end of string is just output
                  arglist ; return unused args
                  )
                 (else
                  (case (char-upcase (string-ref format-strg pos))
                    ((#\A)       ; Any -- for humans
                     (let ( (whatever (car arglist)) )
                       (display whatever port)
                       (anychar-dispatch (+ pos 1) (cdr arglist) (eqv? whatever #\newline))
                     ))
                    ((#\S)       ; Slashified -- for parsers
                     (let ( (whatever (car arglist)) )
                        (write whatever port)
                        (anychar-dispatch (+ pos 1) (cdr arglist) (eqv? whatever #\newline))
                     ))
                    ((#\W)
                     (let ( (whatever (car arglist)) )
                        (write-with-shared-structure whatever port)  ;; srfi-38
                        (anychar-dispatch (+ pos 1) (cdr arglist) (eqv? whatever #\newline))
                     ))
                    ((#\D)       ; Decimal
                     (display (number->string (car arglist) 10) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\X)       ; HeXadecimal
                     (display (number->string (car arglist) 16) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\O)       ; Octal
                     (display (number->string (car arglist)  8) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\B)       ; Binary
                     (display (number->string (car arglist)  2) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\C)       ; Character
                     (write-char (car arglist) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) (eq? (car arglist) #\newline))
                     )
                    ((#\~)       ; Tilde
                     (write-char #\~ port)
                     (anychar-dispatch (+ pos 1) arglist #f)
                     )
                    ((#\%)       ; Newline
                     (newline port)
                     (anychar-dispatch (+ pos 1) arglist #t)
                     )
                    ((#\&)      ; Freshline
                     (if (not last-was-newline) ;; (unless last-was-newline ..
                         (newline port))
                     (anychar-dispatch (+ pos 1) arglist #t)
                     )
                    ((#\_)       ; Space
                     (write-char #\space port)
                     (anychar-dispatch (+ pos 1) arglist #f)
                     )
                    ((#\T)       ; Tab -- IMPLEMENTATION DEPENDENT ENCODING
                     (write-char ASCII-TAB port)
                     (anychar-dispatch (+ pos 1) arglist #f)
                     )
                    ((#\Y)       ; Pretty-print
                     (pretty-print (car arglist) port)  ;; IMPLEMENTATION DEPENDENT
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\F)
                     (display (format-fixed (car arglist) 0 #f) port)
                     (anychar-dispatch (+ pos 1) (cdr arglist) #f)
                     )
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ;; gather "~w[,d]F" w and d digits
                     (let loop ( (index (+ pos 1))
                                 (w-digits (list (string-ref format-strg pos)))
                                 (d-digits '())
                                 (in-width? #t)
                               )
                       (if (>= index length-of-format-string)
                           (error
                            (format "FORMAT: improper numeric format directive in ~s" format-strg))
                           (let ( (next-char (string-ref format-strg index)) )
                             (cond
                              ((char-numeric? next-char)
                               (if in-width?
                                   (loop (+ index 1)
                                         (cons next-char w-digits)
                                         d-digits
                                         in-width?)
                                   (loop (+ index 1)
                                         w-digits
                                         (cons next-char d-digits)
                                         in-width?))
                               )
                              ((char=? next-char #\F)
                               (let ( (width  (string->number (list->string (reverse w-digits))))
                                      (digits (if (zero? (length d-digits))
                                                  #f
                                                  (string->number (list->string (reverse d-digits)))))
                                    )
                                 (display (format-fixed (car arglist) width digits) port)
                                 (anychar-dispatch (+ index 1) (cdr arglist) #f))
                               )
                              ((char=? next-char #\,)
                               (if in-width?
                                   (loop (+ index 1)
                                         w-digits
                                         d-digits
                                         #f)
                                   (error
                                    (format "FORMAT: too many commas in directive ~s" format-strg)))
                               )
                              (else
                               (error (format "FORMAT: ~~w.dF directive ill-formed in ~s" format-strg))))))
                     ))
                    ((#\? #\K)       ; indirection -- take next arg as format string
                     (cond           ;  and following arg as list of format args
                      ((< (length arglist) 2)
                       (error
                        (format "FORMAT: less arguments than specified for ~?: ~s" arglist))
                       )
                      ((not (string? (car arglist)))
                       (error
                        (format "FORMAT: ~? requires a string: ~s" (car arglist)))
                       )
                      (else
                       (format-help (car arglist) (cadr arglist))
                       (anychar-dispatch (+ pos 1) (cddr arglist) #f)
                     )))
                    ((#\H)      ; Help
                     (display documentation-string port)
                     (anychar-dispatch (+ pos 1) arglist #t)
                     )
                    (else
                     (error (format "FORMAT: unknown tilde directive: ~s"
                                    (string-ref format-strg pos))))
                    )))
                )) ; end tilde-dispatch
             ) ; end letrec

             ; format-help main
             (anychar-dispatch 0 arglist #f)
            )) ; end format-help

        ; format main
        (let ( (unused-args (format-help format-string args)) )
          (if (not (null? unused-args))
              (error
               (format "FORMAT: unused arguments ~s" unused-args)))
          (return-value))

      )) ; end letrec, if
))) ; end format

#|
Copyright

Copyright (C) Kenneth A Dickey (2003). All Rights Reserved.

This document and translations of it may be copied and furnished to others,
and derivative works that comment on or otherwise explain it or assist in its
implementation may be prepared, copied, published and distributed, in whole or
in part, without restriction of any kind, provided that the above copyright
notice and this paragraph are included on all such copies and derivative
works. However, this document itself may not be modified in any way, such as
by removing the copyright notice or references to the Scheme Request For
Implementation process or editors, except as needed for the purpose of
developing SRFIs in which case the procedures for copyrights defined in the
SRFI process must be followed, or as required to translate it into languages
other than English.

The limited permissions granted above are perpetual and will not be revoked by
the authors or their successors or assigns.

This document and the information contained herein is provided on an "AS IS"
basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL WARRANTIES, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE
INFORMATION HEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Author: Ken Dickey

Editor: Francisco Solsona
Last modified: Tue Dec 30 10:40:09 CST 2003
|#