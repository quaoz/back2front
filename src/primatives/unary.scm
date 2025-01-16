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
