(if (not (file-exists? "build"))
    (mkdir "build"))

(cd "./build")

(source-directories '("../tests" "../src"))
(load "../tests/tests-driver.scm")
(load "../tests/tests-1.1-req.scm")

(define (emit-program x)
  (unless (integer? x) (error 'compile-program "not an integer"))
  (emit "	.text")
  (emit "   .balign 4")
  (emit "	.globl _scheme_entry")
  (emit "_scheme_entry:")
  (emit "	mov w0, ~s" x)
  (emit "	ret"))
