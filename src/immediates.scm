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
