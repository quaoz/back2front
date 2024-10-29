; place compiled output in build dir
(if (not (file-exists? "build"))
    (mkdir "build"))
(cd "./build")

; include tests
(source-directories '("../tests" "../src"))
(load "../tests/tests-driver.scm")
(load "../tests/tests-1.1-req.scm")
(load "../tests/tests-1.2-req.scm")

; scheme constants
(define fx_shift       2)
(define fx_mask     #x03)
(define fx_tag      #x00)
(define bool_f      #x2F)
(define bool_t      #x6F)
(define empty_list  #x3F)
(define char_shift     8)
(define char_mask   #x0F)
(define char_tag    #x0F)
(define wordsize       4)

; 30 bits for number, 2 for tag
(define fixnum-bits (- (* wordsize 8) fx_shift))

; highest and lowest numbers we can represent with fixnum-bits
(define fx_upper (sub1 (expt 2 (- fixnum-bits 1))))
(define fx_lower (- (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
    (and (integer? x) (exact? x) (<= fx_lower x fx_upper)))

(define (immediate? x)
    (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (immediate-rep x)
    (cond
     [(fixnum? x) (ash x fx_shift)]
     [(boolean? x) (if x bool_t bool_f)]
     [(null? x) empty_list]
     [(char? x) (bitwise-ior (ash (char->integer x) char_shift) char_tag)]
     [else #f]
    ))

(define (emit-program x)
  (unless (immediate? x) (error 'emit-program (format "'~s' not an immediate" x)))
  (emit "	.text")
  (emit "   .balign 4")
  (emit "	.globl _scheme_entry")
  (emit "_scheme_entry:")
  (emit "	mov w0, ~s" (immediate-rep x))
  (emit "	ret"))
