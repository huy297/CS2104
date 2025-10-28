;; ========== test harness ==========
(define *tests* '())
(define *passed* 0)
(define *failed* 0)

(define (add-test label thunk)
  (set! *tests* (append *tests* (list (cons label thunk)))))

(define (ok? x) (eq? x #t))
(define (fail? x) (eq? x #f))

(define (t-true label thunk)
  (add-test label (lambda ()
                    (let ([v (thunk)])
                      (if (ok? v)
                          (begin (set! *passed* (+ *passed* 1)) #t)
                          (begin (set! *failed* (+ *failed* 1))
                                 (printf "FAIL [~a]: expected #t, got ~s~n" label v)
                                 #f))))))

(define (t-false label thunk)
  (add-test label (lambda ()
                    (let ([v (thunk)])
                      (if (fail? v)
                          (begin (set! *passed* (+ *passed* 1)) #t)
                          (begin (set! *failed* (+ *failed* 1))
                                 (printf "FAIL [~a]: expected #f, got ~s~n" label v)
                                 #f))))))

(define (run-tests)
  (set! *passed* 0)
  (set! *failed* 0)
  (for-each (lambda (p) ((cdr p))) *tests*)
  (printf "========================================~n")
  (printf "Total: ~a, Passed: ~a, Failed: ~a~n"
          (+ *passed* *failed*) *passed* *failed*)
  (printf "========================================~n")
  (and (= *failed* 0) #t))

;; ========== basic atoms ==========
(t-true  "number"   (lambda () (check-expression 42)))
(t-true  "boolean"  (lambda () (check-expression #t)))
(t-true  "char"     (lambda () (check-expression #\a)))
(t-true  "string"   (lambda () (check-expression "hi")))
(t-true  "variable" (lambda () (check-expression 'x)))
(t-false "keyword-not-var" (lambda () (check-variable 'if)))

;; ========== time ==========
(t-true  "(time <expr>) ok"   (lambda () (check-expression '(time 1))))
(t-false "(time) arity bad"   (lambda () (check-expression '(time))))
(t-false "(time 1 2) arity"   (lambda () (check-expression '(time 1 2))))

;; ========== if / unless ==========
(t-true  "if ok"     (lambda () (check-expression '(if 1 2 3))))
(t-false "if arity"  (lambda () (check-expression '(if 1 2))))
(t-true  "unless ok" (lambda () (check-expression '(unless #t 0))))
(t-false "unless arity" (lambda () (check-expression '(unless #t))))

;; ========== quote / quotation ==========
(t-true  "quote atom"    (lambda () (check-expression '(quote 1))))
(t-true  "quote list"    (lambda () (check-expression '(quote (a (1 . 2) "x")))))
(t-false "quote arity"   (lambda () (check-expression '(quote))))
(t-false "quote arity2"  (lambda () (check-expression '(quote 1 2))))

;; ========== and / or ==========
(t-true  "and empty"   (lambda () (check-expression '(and))))
(t-true  "and some"    (lambda () (check-expression '(and 1 2 #t))))
(t-true  "or empty"    (lambda () (check-expression '(or))))
(t-true  "or some"     (lambda () (check-expression '(or #f 1))))

;; ========== begin ==========
(t-true  "begin ≥1"    (lambda () (check-expression '(begin 1 2 3))))
(t-false "begin zero"  (lambda () (check-expression '(begin))))

;; ========== lambda / formals ==========
(t-true  "lambda ()"      (lambda () (check-expression '(lambda () 1))))
(t-true  "lambda (x y)"   (lambda () (check-expression '(lambda (x y) (+ x y)))))
(t-true  "lambda (x . y)" (lambda () (check-expression '(lambda (x . y) y))))
(t-true  "lambda var"     (lambda () (check-expression '(lambda args args))))
(t-false "lambda dup"     (lambda () (check-expression '(lambda (x x) x))))
(t-false "lambda kw"      (lambda () (check-expression '(lambda (if) 1))))

;; ========== trace-lambda ==========
(t-true  "trace-lambda ok"
         (lambda () (check-expression '(trace-lambda f (x) x))))
(t-false "trace-lambda bad name"
         (lambda () (check-expression '(trace-lambda if (x) x))))

;; ========== application ==========
(t-true  "application simple"
         (lambda () (check-expression '(f 1 2 3))))
(t-true  "application compound op"
         (lambda () (check-expression '((lambda (x) x) 1))))
(t-true "application kw as op"
         (lambda () (check-expression '(if 1 2 3)))) ; đây là if, không phải application

;; ========== let ==========
(t-true  "let ok"
         (lambda () (check-expression '(let ((x 1) (y 2)) (+ x y)))))
(t-true  "let empty bindings"
         (lambda () (check-expression '(let () 42))))
(t-false "let duplicate vars"
         (lambda () (check-expression '(let ((x 1) (x 2)) x))))
(t-false "let binding shape"
         (lambda () (check-expression '(let ((x)) x))))
(t-false  "let multiple body expressions"
         (lambda () (check-expression '(let ((x 1) (y 2)) x y))))
(t-false "let no body"
         (lambda () (check-expression '(let ((x 1))))))
(t-false "let binding has >2 elements"
         (lambda () (check-expression '(let ((x 1 2)) x))))
(t-false "let keyword as binding name"
         (lambda () (check-expression '(let ((if 1)) if))))
(t-false "let bindings must be proper pairs"
         (lambda () (check-expression '(let ((x . 1)) x))))


;; ========== let* ==========
(t-true  "let* ok"
         (lambda () (check-expression '(let* ((x 1) (y x)) y))))
(t-true  "let* empty bindings allowed"
         (lambda () (check-expression '(let* () 42))))
(t-true  "let* variable shadowing"
         (lambda () (check-expression '(let* ((x 1) (x 2)) x))))
(t-false "let* bad binding"
         (lambda () (check-expression '(let* ((x)) x))))
(t-false "let* keyword cannot be a binding name"
         (lambda () (check-expression '(let* ((if 1)) if))))
(t-false "let* binding pair must have exactly 2 elements"
         (lambda () (check-expression '(let* ((x 1 2)) x))))
(t-false "let* requires at least one body expression"
         (lambda () (check-expression '(let* ((x 1))))))

;; ========== letrec ==========
(t-true  "letrec ok (lambda RHS)"
         (lambda () (check-expression '(letrec ((f (lambda (x) x))) (f 1)))))
(t-true  "letrec ok (trace-lambda RHS)"
         (lambda () (check-expression '(letrec ((f (trace-lambda f (x) x))) (f 1)))))
(t-true  "letrec empty bindings"
         (lambda () (check-expression '(letrec () 0))))
(t-false "letrec RHS not lambda"
         (lambda () (check-expression '(letrec ((f 1)) f))))
(t-false "letrec dup vars"
         (lambda () (check-expression '(letrec ((f (lambda (x) x))
                                               (f (lambda (y) y))) f))))
(t-false "letrec bindings must be a list"
         (lambda () (check-expression '(letrec 123 0))))
(t-false "letrec binding must be a 2-element list"
         (lambda () (check-expression '(letrec ((f)) 0))))
(t-false "letrec binding must not have >2 elements"
         (lambda () (check-expression '(letrec ((f (lambda (x) x) 1)) 0))))
(t-false "letrec binding must be a proper pair"
         (lambda () (check-expression '(letrec ((f . (lambda (x) x))) 0))))
(t-false "letrec binding name must be a symbol"
         (lambda () (check-expression '(letrec ((42 (lambda (x) x))) 0))))
(t-false "letrec binding name cannot be a keyword"
         (lambda () (check-expression '(letrec ((if (lambda (x) x))) 0))))
(t-false "letrec multiple body expressions not allowed"
         (lambda () (check-expression '(letrec ((f (lambda () 0))) f 1))))

;; ========== cond ==========
(t-true  "cond simple"
         (lambda () (check-expression '(cond [(> 1 0) 1] [else 2]))))
(t-true  "cond =>"
         (lambda () (check-expression '(cond [(> 1 0) => (lambda (x) x)] [else 0]))))
(t-true  "cond inconsequential clause"
         (lambda () (check-expression '(cond [(> 1 0)][else 0]))))
(t-false "cond else not last"
         (lambda () (check-expression '(cond [else 1] [(#t) 2]))))
(t-false "cond empty"
         (lambda () (check-expression '(cond))))
(t-false "cond clause must be a list"
         (lambda () (check-expression '(cond 1))))
(t-false "cond clause must be proper list"
         (lambda () (check-expression '(cond [((> 1 0)) . 1] [else 1]))))
(t-false "cond else requires at least one expression"
         (lambda () (check-expression '(cond [else]))))
(t-false "cond duplicate else not allowed"
         (lambda () (check-expression '(cond [else 1] [else 2]))))
(t-false "cond => must have exactly one target"
         (lambda () (check-expression
                     '(cond [(> 1 0) => (lambda (x) x) 3][else 1]))))
(t-false "cond else cannot use =>"
         (lambda () (check-expression
                     '(cond [else => (lambda (x) x)]))))


;; ========== case ==========
(t-true  "case ok"
         (lambda () (check-expression '(case 1 [((1 2)) 'a] [else 'b]))))
(t-true  "case list datum"
         (lambda () (check-expression
                     '(case '(1 2) [((1 2)) 'hit] [((1 3)) 'hit] [else 'miss]))))
(t-false "case empty"
         (lambda () (check-expression '(case 1))))
(t-false "case else not last"
         (lambda () (check-expression '(case 'x [else 1] [(() 2)]))))
(t-false "case clause datums must be a list"
         (lambda () (check-expression
                     '(case 1 [1 'a][else 1]))))
(t-false "case clause datums list must be proper"
         (lambda () (check-expression
                     '(case 1 [(1 . 2) 'a][else 1]))))
(t-false "case clause must have at least one consequent"
         (lambda () (check-expression
                     '(case 1 [(1 2)][else 1]))))
(t-false "case else must have at least one consequent"
         (lambda () (check-expression
                     '(case 1 [else]))))

;; ========== toplevel definition ==========
(t-true  "define simple"
         (lambda () (check-program '((define x 1) (define y (lambda () x))))))
(t-true "define sugar allowed"
         (lambda () (check-program '((define (f x) x)))))


;; ========== nested structures ==========
(t-true "nested if and let"
        (lambda ()
          (check-expression
           '(let ((x 1))
              (if (> x 0)
                  (let ((y 2)) (+ x y))
                  0)))))


;; ========== application edge cases ==========
(t-false "application numeric op"
         (lambda () (check-expression '(1 2 3))))
(t-false "application keyword op"
         (lambda () (check-expression '(if 1 2 3 4))))
(t-true  "application lambda op"
         (lambda () (check-expression '((lambda (x) (+ x 1)) 3))))

;; ========== malformed lambdas ==========
(t-false "lambda no body"
         (lambda () (check-expression '(lambda (x)))))
(t-false "lambda non-symbol arg"
         (lambda () (check-expression '(lambda (1) 1))))
(t-false "trace-lambda extra arg"
         (lambda () (check-expression '(trace-lambda f (x y) x y))))

;; ========== malformed let* and letrec ==========
(t-false "let* duplicate vars"
         (lambda () (check-expression '(let* ((x 1) (x 2)) x))))
(t-false "letrec non-function binding"
         (lambda () (check-expression '(letrec ((x 1) (y (lambda () 2))) x))))
(t-true  "letrec nested lambda ok"
         (lambda () (check-expression '(letrec ((f (lambda (x) (if x (f (- x 1)) x)))) (f 3)))))

;; ========== cond / case edge cases ==========
(t-false "cond else twice"
         (lambda () (check-expression '(cond [(> 1 0) 1] [else 2] [else 3]))))
(t-false "case duplicated else"
         (lambda () (check-expression '(case 1 [else 2] [else 3]))))

