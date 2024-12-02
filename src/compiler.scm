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

;;; immediates ;;;

; 30 bits for number, 2 for tag
(define fixnum-bits (- (* wordsize 8) fx_shift))

; highest and lowest numbers we can represent with `fixnum-bits`
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
    [else #f]))

(define (move r v)
    ; 0-16 bits
    (if (<= (abs v) #xFFFF)
        (emit "   mov x~s, ~s" r v))

    ; 16-32 bits
    (if (> (abs v) #xFFFF)
        (begin
            (emit "   movz x~s, ~s" r (bitwise-and v #xFFFF))
            (emit "   movk x~s, ~s, lsl #16" r (bitwise-and (ash v (- 16)) #xFFFF))))

    ; 32-48 bits
    (if (> (abs v) #xFFFFFFFF)
        (emit "   movk x~s, ~s, lsl #32" r (bitwise-and (ash v (- 32)) #xFFFF)))

    ; 48-64 bits
    (if (> (abs v) #xFFFFFFFFFFFF)
        (emit "   movk x~s, ~s, lsl #48" r (bitwise-and (ash v (- 48)) #xFFFF))))

(define (emit-immediate x)
    (move 0 (immediate-rep x)))

;;; unary primatives ;;;

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si arg* ...) b b* ...)
      (begin
        (putprop 'prim-name '*is-prim* #t)
        (putprop 'prim-name '*arg-count*
          (length '(arg* ...)))
        (putprop 'prim-name '*emitter*
          (lambda (si arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter (format "primitive ~s has no emitter" x))))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (= (getprop prim '*arg-count*) (length args)))

(define (emit-primcall si expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define (emit-csel-bool cond)
  (emit "   mov w1, ~s" bool_t)
  (emit "   mov w2, ~s" bool_f)
  (emit "   csel w0, w1, w2, ~a" cond))

(define-primitive ($fxadd1 si arg)
  (emit-expr si arg)
  (emit "   add w0, w0, ~s" (immediate-rep 1)))

(define-primitive ($fxsub1 si arg)
  (emit-expr si arg)
  (emit "   sub w0, w0, ~s" (immediate-rep 1)))

(define-primitive ($fixnum->char si arg)
  (emit-expr si arg)
  (emit "   lsl w0, w0, ~s" (- char_shift fx_shift))
  (emit "   orr w0, w0, ~s" char_tag))

(define-primitive ($char->fixnum si arg)
  (emit-expr si arg)
  (emit "   lsr w0, w0, ~s" (- char_shift fx_shift)))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit "   ands wzr, w0, ~s" fx_mask)
  (emit-csel-bool "eq"))

(define-primitive ($fxzero? si arg)
  (emit-expr si arg)
  (emit "   subs wzr, w0, ~s" fx_tag)
  (emit-csel-bool "eq"))

(define-primitive (null? si arg)
  (emit-expr si arg)
  (emit "   subs wzr, w0, ~s" empty_list)
  (emit-csel-bool "eq"))

(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (move 1 bool_t)
  (move 2 bool_f)
  (move 3 bool_mask)
  (emit "   and w0, w0, w3")
  (emit "   subs wzr, w0, w2")
  (emit "   csel w0, w1, w2, eq"))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit "   and w0, w0, ~s" char_mask)
  (emit "   subs wzr, w0, ~s" char_tag)
  (emit-csel-bool "eq"))

(define-primitive (not si arg)
  (emit-expr si arg)
  (move 1 bool_t)
  (move 2 bool_f)
  (emit "   subs wzr, w0, w2")
  (emit "   csel w0, w1, w2, eq"))

(define-primitive ($fxlognot si arg)
  (emit-expr si arg)
  (emit "   mvn w0, w0, lsr #~s" fx_shift)
  (emit "   lsl w0, w0, ~s" fx_shift))

;;; conditional expressions ;;;

(define unique-label
    (let ([count 0])
        (lambda ()
            (let ([L (format "L_~s" count)])
                (set! count (add1 count))
                L))))

(define (emit-if si expr)
    (let ([alt-label (unique-label)]
          [end-label (unique-label)])
    (emit-expr si (if-test expr))
    (move 1 bool_f)
    (emit "   cmp x1, x0")
    (emit "   B.eq ~a" alt-label)
    (emit-expr si (if-conseq expr))
    (emit "   B ~a" end-label)
    (emit-label alt-label)
    (emit-expr si (if-altern expr))
    (emit-label end-label)))

(define (if? expr)
    (and (list? expr) (eq? (car expr) 'if)))

(define (if-test expr) (cadr expr))
(define (if-conseq expr) (caddr expr))
(define (if-altern expr) (cadddr expr))

;;; binary primatives ;;;

; TODO: these push and pop implementations are wasteful as they store all values in a 16 byte slot, see below for possible improvements
; https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/using-the-stack-in-aarch64-implementing-push-and-pop
(define (push r)
    (emit "   str x~s, [sp, #-16]!" r))

(define (pop r)
    (emit "   ldr x~s, [sp], #16" r))

(define (stack-expr si arg1 arg2)
    (emit-expr si arg1)
    (push 0)
    (emit-expr (- si wordsize) arg2)
    (pop 1))

(define-primitive (fx+ si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   add x0, x0, x1"))

(define-primitive (fx- si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   sub x0, x1, x0"))

(define-primitive (fx* si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   lsr x1, x1, #~s" fx_shift)
    (emit "   lsr x0, x0, #~s" fx_shift)
    (emit "   mul x0, x0, x1")
    (emit "   lsl x0, x0, #~s" fx_shift))

(define-primitive (fxlogand si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   and x0, x0, x1"))

(define-primitive (fxlogor si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   orr x0, x0, x1"))

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
  (emit "   mvn w0, w0, lsr #~s" fx_shift)
  (emit "   lsl w0, w0, ~s" fx_shift))

(define-primitive (fx= si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   subs xzr, x0, x1")
    (emit-csel-bool "eq"))

(define-primitive (fx< si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   subs xzr, x0, x1")
    (emit-csel-bool "gt"))

(define-primitive (fx<= si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   subs xzr, x0, x1")
    (emit-csel-bool "ge"))

(define-primitive (fx> si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   subs xzr, x0, x1")
    (emit-csel-bool "lt"))

(define-primitive (fx>= si arg1 arg2)
    (stack-expr si arg1 arg2)
    (emit "   subs xzr, x0, x1")
    (emit-csel-bool "le"))

;;; program ;;;

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
