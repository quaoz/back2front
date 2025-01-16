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
