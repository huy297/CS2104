;; basic literals and variables
42
#t
#f
#\a
"hello"
x

;;;
;; arithmetic and boolean operations
(+ 1 2 3)
(and #t (> 5 3))
(or #f (< 2 3))
(not (= 4 4))

;;;
;; simple if / unless expressions
(if (> 3 2) 1 0)
(unless #f (display "ok"))

;;;
;; begin block
(begin
  (display "hi")
  (newline)
  42)

;;;
;; lambda abstractions and applications
((lambda (x) (+ x 1)) 3)
(lambda (x y) (* x y))
(trace-lambda add (x y) (+ x y))

;;;
;; nested lambdas
(lambda (x)
  (lambda (y)
    (+ x y)))

;;;
;; let bindings
(let ((x 10) (y 20))
  (+ x y))
(let () 1)

;;;
;; let* bindings
(let* ((x 1) (y (+ x 1))) y)
(let* ((a 2)) (* a a))

;;;
;; letrec recursion
(letrec ((fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1)))))))
  (fact 5))

;;;
;; cond expression
(cond
  [(> 3 2) 'bigger]
  [(= 3 3) 'equal]
  [else 'smaller])

(cond
  [(positive? 3) => (lambda (x) x)]
  [else 0])

;;;
;; case expression
(case 3
  [(1 2) 'small]
  [(3 4) 'medium]
  [else 'large])

(case 'a
  [((a b c)) 'letter]
  [else 'unknown])

;;;
;; quote expression
(quote (1 2 3))
(quote "string")
(quote (a (b c) d))

;;;
;; begin + nested let
(begin
  (let ((x 1))
    (let ((y 2))
      (+ x y))))

;;;
;; top-level definitions
(define x 10)
(define (square n) (* n n))
(define (add a b) (+ a b))

;;;
;; multiple top-level forms (program)
(define pi 3.14159)
(define (area r) (* pi r r))
(area 10)

;;;
;; time expression
(time (+ 1 2))
(time (square 5))

;;;
;; complex nested example
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 6)

;;;
;; mixed conditionals and letrec
(letrec ((even? (lambda (n)
                  (if (= n 0)
                      #t
                      (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0)
                     #f
                     (even? (- n 1))))))
  (even? 10))

;;;
;; nested let + cond
(let ((x 5))
  (cond
    [(> x 10) 'big]
    [(= x 5) 'mid]
    [else 'small]))

;;;
;; chained begin and applications
(begin
  (display "Result: ")
  (newline)
  ((lambda (x) (* x 2)) 5))

;;;
;; valid mixed program with definitions
(define msg "Hello Scheme")
(define (say-hi) (display msg))
(begin (say-hi) (newline))

;;; ============================
;;; Long valid Scheme programs
;;; ============================

;;; 1. Recursive factorial with tracing and nested begin
(define (fact n)
  (trace-lambda fact (n)
    (begin
      (if (< n 2)
          1
          (* n (fact (- n 1)))))))

(fact 6)

;;;
;;; 2. Multiple nested let and conditionals
(let ((x 5)
      (y 10))
  (begin
    (if (> x y)
        (display "x larger")
        (begin
          (display "y larger")
          (newline)
          (let ((z (+ x y)))
            (if (even? z)
                (display "sum even")
                (display "sum odd")))))))

;;;
;;; 3. letrec with mutual recursion (even?/odd?)
(letrec ((even? (lambda (n)
                  (if (= n 0)
                      #t
                      (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0)
                     #f
                     (even? (- n 1))))))
  (begin
    (display (even? 10))
    (newline)
    (display (odd? 5))
    (newline)))

;;;
;;; 4. Nested cond with => operator
(cond
  [(= 0 0) => (lambda (x) (display "equal"))]
  [(> 1 2) (display "greater")]
  [else (display "fallback")])

;;;
;;; 5. Case with mixed datums and nested lambda
(case 'a
  [((a e i o u))
   ((lambda (x) (display "vowel: ")) 'a)]
  [((b c d))
   (begin
     (display "consonant")
     (newline))]
  [else
   (display "other")])

;;;
;;; 6. let* nested inside cond and lambda
((lambda (n)
   (let* ((a (* n n))
          (b (+ a n)))
     (cond
       [(> b 10) (display "large")]
       [(< b 5) (display "small")]
       [else (display "medium")]))))


;;;
;;; 7. Recursive Fibonacci with local helper (letrec)
(letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (fib 10))

;;; 9. Mixed lambda + application with embedded begin
((lambda (msg n)
   (begin
     (display msg)
     (newline)
     (if (> n 5)
         'big
         'small)))
 "testing..." 7)

;;;
;;; 10. Deeply nested letrec and case
(letrec ((analyze
          (lambda (x)
            (case x
              [(1 2 3) 'small]
              [(4 5 6) 'medium]
              [else 'large]))))
  (begin
    (display (analyze 5))
    (newline)
    (display (analyze 9))))

;;;
;;; 11. Quote expression and usage
(define quoted-list (quote (1 (2 (3 (4))))))
(begin
  (display "Quoted list: ")
  (display quoted-list))

