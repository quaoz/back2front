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
