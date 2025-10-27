#0=(define x '(1 2 . #0#))
(+ 1 2)

;; This Scheme program has correct parentheses
;; but lots of semantic and logical errors for testing check-file.

;; --- Undefined variable ---
(+ foo 10)

;; --- Invalid function call (string as function) ---
("hello" 5)

;; --- Wrong number of arguments ---
(if 1 2 3 4)

;; --- Type mismatch in arithmetic ---
(+ "abc" 5)

;; --- Lambda with invalid parameter list ---
(lambda 123 (+ 1 2))

;; --- Let with wrong binding format ---
(let (x 1 y 2) (+ x y))

;; --- Nested expression using undefined operator ---
((maybe-call 2 3) (another 4))

;; --- Useless define: wrong form ---
(define (42 x) (+ x 1))

;; --- Empty begin ---
(begin)

;; --- Invalid cond structure ---
(cond [(#t 1) (else 2)])

;; --- Create cyclic value at runtime ---
(define cyc (list 1 2 3))
(set-cdr! (cdr cyc) cyc)
(check-expression cyc)



;; --- Final valid expression for contrast ---
(define good '(lambda (x) (+ x 1)))
