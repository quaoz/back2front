; place compiled output in build dir
(if (not (file-exists? "build"))
  (mkdir "build"))
(cd "./build")

; include tests
(source-directories '("../tests" "../src"))
(load "../tests/tests-driver.scm")
(load "../tests/tests-1.1-req.scm")
(load "../tests/tests-1.2-req.scm")
(load "../tests/tests-1.3-req.scm")
(load "../tests/tests-1.4-req.scm")
(load "../tests/tests-1.5-req.scm")

; scheme constants
(define fx_shift       2)
(define fx_mask     #x03)
(define fx_tag      #x00)
(define bool_mask   #xBF)
(define bool_f      #x2F)
(define bool_t      #x6F)
(define empty_list  #x3F)
(define char_shift     8)
(define char_mask   #x3F)
(define char_tag    #x0F)
(define wordsize       4)

; compiler parts
(load "../src/immediates.scm")
(load "../src/primatives/unary.scm")
(load "../src/conditionals.scm")
(load "../src/primatives/binary.scm")

; boilerplate
(define (emit-expr si expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(primcall? expr) (emit-primcall si expr)]
    [(if? expr) (emit-if si expr)]
    [else (error 'emit-expr (format "~s is not an expression" expr))]))

(define (emit-function-header f)
  (emit "   .globl ~a" f)
  (emit-label f))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-program x)
  (emit "   .text")
  (emit "   .balign 4")
  (emit-function-header "scheme_entry")
  (emit "   bl _scheme_entry")
  (emit "   ret")
  (emit-function-header "_scheme_entry")
  (emit-expr (- wordsize) x)
  (emit "   ret"))
