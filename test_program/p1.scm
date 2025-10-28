;; Mixed program for syntax checker stress test

(define good-constant 42)

(define bad-let
  (let ((x 1) (x))   ; malformed binding (missing RHS)
    (+ x y)))

(define duplicate-let
  (let ((x 2) (x 3)) ; duplicate variable
    (+ x 1)))

(define bad-cond
  (cond
    [else 99]
    [(#t) "not last else"]))

(define bad-case
  (case flag
    [a 1]                     ; datums not a list
    [(b c) (+ b c)]
    [else "done" 123]))        ; too many expressions

(define bad-letrec
  (letrec ((x 1) (y (trace-lambda y () y)))
    (+ x y)))

(define bad-lambda
  (lambda x y))                ; missing parentheses

(define broken-quote
  (quote #(1 2 3)))            ; vector inside quote not allowed

(define ok-lambda
  (lambda (a b)
    (if (> a b)
        (begin
          (display "ok"))
        (let* ((z (+ a b)))
          z))))

(define outer
  (letrec ((f (lambda (x) (if (= x 0) 1 (f (- x 1))))))
    (f 5)))

42   ; stray top-level literal
"abc" ; stray string literal
