

;; --- Basic variable definitions ---
(define a 10)
(define b (+ a 5))
(define c (* b 2))
(define d (/ c (- a 3)))

;; --- Simple function definitions ---
(define (square x) (* x x))
(define (add3 x y z) (+ x y z))

;; --- Nested lambda expressions ---
(define adder
  (lambda (x)
    (lambda (y)
      (+ x y))))

;; --- Recursive factorial ---
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

;; --- Fibonacci with cond ---
(define (fib n)
  (cond
    ((<= n 1) n)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

;; --- Intentional syntax mistake: missing else branch ---
(define (abs x)
  (if (< x 0)
      (- x)
      ))

;; --- Let binding test ---
(define (sum-to n)
  (let ((i 0)
        (s 0))
    (begin
      (set! s (+ s i))
      (set! i (+ i 1))
      (if (< i n)
          (sum-to i)
          s))))

;; --- Intentional malformed let binding (extra parentheses) ---
(define (bad-let)
  (let (((x 10) (y 20))) (+ x y)))

;; --- Letrec for mutual recursion ---
(define (even? n)
  (letrec ((is-even (lambda (x)
                      (if (= x 0)
                          #t
                          (is-odd (- x 1)))))
           (is-odd (lambda (x)
                     (if (= x 0)
                         #f
                         (is-even (- x 1))))))
    (is-even n)))

;; --- Intentional bad letrec: binding not a pair ---
(define (bad-letrec)
  (letrec (x 10) (+ x 1)))

;; --- Case expression ---
(define (weekday n)
  (case n
    ((1) 'Mon)
    ((2) 'Tue)
    ((3) 'Wed)
    ((4) 'Thu)
    ((5) 'Fri)
    ((6 7) 'Weekend)
    (else 'Invalid)))

;; --- Intentional bad case: non-list datums ---
(define (bad-case n)
  (case n
    (1 'one)
    (else 'unknown)))

;; --- Complex nesting: lambda inside let ---
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; --- Higher-order application ---
(define (map-square lst)
  (if (null? lst)
      '()
      (cons (square (car lst))
            (map-square (cdr lst)))))

;; --- Deeply nested begin block ---
(define (test-begin)
  (begin
    (display "Running...")
    (newline)
    (begin
      (display "Nested begin ok")
      (newline)
      (set! a (+ a 1))
      (set! b (+ b 2))
      (set! 10 5) ; <-- intentional invalid set!
      (begin
        (display "End of test")))))

;; --- Function with proper quoting ---
(define (show-list lst)
  (cond
    ((null? lst) '())
    (else (begin
            (display (car lst))
            (show-list (cdr lst))))))

;; --- Intentional: bad cond clause (empty test) ---
(define (cond-error x)
  (cond
    (() 1)
    (else 2)))

;; --- Nested structures and vectors ---
(define vec1 (vector 1 2 3 4))
(define (vec-sum v)
  (let loop ((i 0) (s 0))
    (if (= i (vector-length v))
        s
        (loop (+ i 1)
              (+ s (vector-ref v i))))))

;; --- Intentional: cyclic vector (for cycle test) ---
(define v2 (make-vector 1))
(vector-set! v2 0 v2)

;; --- Function calling several others ---
(define (main)
  (begin
    (display "Computing values...")
    (newline)
    (display (fact 5))
    (newline)
    (display (fib 6))
    (newline)
    (display (weekday 3))
    (newline)
    (display (sum-to 5))
    (newline)
    (display (even? 10))
    (newline)
    (display (vec-sum vec1))
    (newline)
    (bad-case 2)
    (bad-let)
    (bad-letrec)
    (cond-error 1)
    (bad-lambda)
    (test-begin)))

;; --- Intentional malformed define (missing parentheses around parameters) ---
(define run main main)

;; --- End of program ---
