;;; ----------------------------------------------------------------------------
;;; Flags (kept minimal; no line numbers / no diagnostics by default)
;;; ----------------------------------------------------------------------------

(define check-silently #t)

;;; ----------------------------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------------------------

(define (proper-list-of-given-length? v n)
  (let loop ([v v] [i 0])
    (cond
      [(null? v) (= i n)]
      [(pair? v) (loop (cdr v) (+ i 1))]
      [else #f])))

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v) (- i 1)))))])
      (and (>= n 0) (visit v n)))))

(define keyword?
  (lambda (w)
    (and (symbol? w)
         (memq w '(define time if cond else case and or
                    let let* letrec begin unless quote
                    lambda trace-lambda)))))

(define distinct-symbols?
  (lambda (xs)
    (letrec ([loop
              (lambda (xs seen)
                (or (null? xs)
                    (and (not (memq (car xs) seen))
                         (loop (cdr xs)
                               (cons (car xs) seen)))))])
      (loop xs '()))))

;;; ----------------------------------------------------------------------------
;;; Cycle detection (for top-level program only)
;;; ----------------------------------------------------------------------------

(define atomic-value?
  (lambda (v)
    (or (number? v)
        (boolean? v)
        (char? v)
        (string? v)
        (symbol? v)
        (null? v))))

(define cyclic-value?
  (lambda (v)
    (letrec ([visit (lambda (v seen)
                      (cond
                        [(atomic-value? v) #f]
                        [(memq v seen) #t]
                        [(pair? v)
                         (let ([seen* (cons v seen)])
                           (or (visit (car v) seen*)
                               (visit (cdr v) seen*)))]
                        [else #f]))])
      (visit v '()))))

;;; ----------------------------------------------------------------------------
;;; Ground predicates & checks
;;; ----------------------------------------------------------------------------

(define is-number?    number?)
(define is-boolean?   boolean?)
(define is-character? char?)
(define is-string?    string?)

(define is-variable?
  (lambda (v) (and (symbol? v) (not (keyword? v)))))

(define check-number    (lambda (n) #t))
(define check-boolean   (lambda (b) #t))
(define check-character char?)
(define check-string    string?)
(define check-variable  is-variable?)

;;; ----------------------------------------------------------------------------
;;; Definitions
;;; ----------------------------------------------------------------------------

(define (is-definition? v)
  (and (pair? v)
       (eq? (car v) 'define)))

(define define-1 (lambda (v) (list-ref v 1)))
(define define-2 (lambda (v) (list-ref v 2)))

;; Only the grammar form (define <variable> <expression>) is allowed.
(define (check-definition v)
  (and (proper-list-of-given-length? v 3)
       (let ([lhs (cadr v)] [rhs (caddr v)])
         (and (check-variable lhs)
              (check-expression rhs)))))

;;; ----------------------------------------------------------------------------
;;; Core forms: time / if / unless / quote / and / or / begin
;;; ----------------------------------------------------------------------------

;; time
(define is-time?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'time))))

(define time-arg (lambda (v) (list-ref v 1)))

(define check-time-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (check-expression (time-arg v)))))

;; if
(define is-if?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'if))))

(define if-1 (lambda (v) (list-ref v 1)))
(define if-2 (lambda (v) (list-ref v 2)))
(define if-3 (lambda (v) (list-ref v 3)))

(define (check-if-expression v)
  (and (proper-list-of-given-length? v 4)
       (check-expression (if-1 v))
       (check-expression (if-2 v))
       (check-expression (if-3 v))))

;; unless
(define is-unless?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'unless))))

(define unless-1 (lambda (v) (list-ref v 1)))
(define unless-2 (lambda (v) (list-ref v 2)))

(define check-unless-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (check-expression (unless-1 v))
         (check-expression (unless-2 v)))))

;; quote
(define is-quote?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'quote))))

(define quote-1 (lambda (v) (list-ref v 1)))

(define (check-quote-expression v)
  (or (atomic-value? v)
      (and (pair? v)
           (check-quote-expression (car v))
           (check-quote-expression (cdr v)))))

;; and/or
(define is-and?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'and))))

(define and-operands cdr)

(define is-or?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'or))))

(define or-operands cdr)

;; begin (>= 1 expr)
(define is-begin?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'begin))))

(define begin-first cadr)
(define begin-rest  cddr)

(define check-begin-expression
  (lambda (v)
    (and (list-strictly-longer-than? v 1)
         (check-expression (begin-first v))
         (check-expressions (begin-rest v)))))

;;; ----------------------------------------------------------------------------
;;; Lambda / trace-lambda  (BNF has a single body expression)
;;; ----------------------------------------------------------------------------

(define is-lambda?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'lambda))))

(define lambda-formals (lambda (v) (cadr v)))
(define lambda-body    (lambda (v) (cddr v))) ; list of bodies (BNF uses exactly one)

;; Single, fused checker for formals (proper or improper), enforcing distinctness.
(define check-lambda-formals
  (lambda (fs)
    (letrec ([go
              (lambda (fs seen)
                (cond
                  [(null? fs) #t]
                  [(symbol? fs) (and (is-variable? fs)
                                     (not (memq fs seen)))]
                  [(pair? fs)
                   (and (is-variable? (car fs))
                        (not (memq (car fs) seen))
                        (go (cdr fs) (cons (car fs) seen)))]
                  [else #f]))])
      (go fs '()))))

(define check-lambda-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 3) ; BNF: exactly one body expression
         (check-lambda-formals (lambda-formals v))
         (check-expression (car (lambda-body v))))))

;; trace-lambda (BNF: (trace-lambda <var> <formals> <expression>))
(define is-trace-lambda?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'trace-lambda))))

(define trace-lambda-name    (lambda (v) (list-ref v 1)))
(define trace-lambda-formals (lambda (v) (list-ref v 2)))
(define trace-lambda-body    (lambda (v) (list-ref v 3)))

(define check-trace-lambda-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (check-variable (trace-lambda-name v))
         (check-lambda-formals (trace-lambda-formals v))
         (check-expression (trace-lambda-body v)))))

;;; ----------------------------------------------------------------------------
;;; let / let* / letrec
;;; ----------------------------------------------------------------------------

;; let
(define is-let?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'let))))

(define let-bindings (lambda (v) (list-ref v 1)))
(define let-body     (lambda (v) (list-ref v 2)))

(define check-let-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (let ([bs (let-bindings v)])
           (letrec ([chk
                     (lambda (bs seen)
                       (cond
                         [(null? bs) #t]
                         [(pair? bs)
                          (let ([b (car bs)])
                            (and (proper-list-of-given-length? b 2)
                                 (check-variable (car b))
                                 (not (memq (car b) seen))
                                 (check-expression (cadr b))
                                 (chk (cdr bs) (cons (car b) seen))))]
                         [else #f]))])
             (and (chk bs '())
                  (check-expression (let-body v))))))))

;; let*
(define is-letstar?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'let*))))

(define letstar-bindings (lambda (v) (list-ref v 1)))
(define letstar-body     (lambda (v) (list-ref v 2)))

(define check-letstar-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (letrec ([chk
                   (lambda (bs)
                     (cond
                       [(null? bs) #t]
                       [(pair? bs)
                        (let ([b (car bs)])
                          (and (proper-list-of-given-length? b 2)
                               (check-variable (car b))
                               (check-expression (cadr b))
                               (chk (cdr bs))))]
                       [else #f]))])
           (and (chk (letstar-bindings v))
                (check-expression (letstar-body v)))))))

;; letrec
(define is-letrec?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'letrec))))

(define letrec-bindings (lambda (v) (list-ref v 1)))
(define letrec-body     (lambda (v) (list-ref v 2)))

(define check-letrec-expression
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (let ([bs (letrec-bindings v)])
           (letrec ([chk
                     (lambda (bs seen)
                       (cond
                         [(null? bs) #t]
                         [(pair? bs)
                          (let* ([b   (car bs)]
                                 [ok  (proper-list-of-given-length? b 2)]
                                 [x   (and ok (car b))]
                                 [rhs (and ok (cadr b))])
                            (and ok
                                 (check-variable x)
                                 (not (memq x seen))
                                 (or (and (is-lambda? rhs)
                                          (check-lambda-expression rhs))
                                     (and (is-trace-lambda? rhs)
                                          (check-trace-lambda-expression rhs)))
                                 (chk (cdr bs) (cons x seen))))]
                         [else #f]))])
             (and (chk bs '())
                  (check-expression (letrec-body v))))))))

;;; ----------------------------------------------------------------------------
;;; cond / case
;;; ----------------------------------------------------------------------------

;; cond
(define is-cond?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'cond))))

(define cond-clauses cdr)

(define is-else-clause?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'else)
         (check-expression (cadr v)))))

(define is-cond-clause?
  (lambda (v)
    (cond
      [(proper-list-of-given-length? v 1)
       (check-expression (car v))]
      [(proper-list-of-given-length? v 2)
       (and (check-expression (car v))
            (check-expression (cadr v)))]
      [(proper-list-of-given-length? v 3)
       (and (check-expression (car v))
            (eq? (cadr v) '=>)
            (check-expression (caddr v)))]
      [else #f])))

(define check-cond-clauses
  (lambda (cs)
    (cond
      [(and (proper-list-of-given-length? cs 1)
            (is-else-clause? (car cs)))
       #t]
      [(pair? cs)
       (and (is-cond-clause? (car cs))
            (check-cond-clauses (cdr cs)))]
      [else #f])))

(define check-cond-expression
  (lambda (v)
    (check-cond-clauses (cond-clauses v))))

;; case
(define is-case?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'case))))

(define case-key     (lambda (v) (list-ref v 1)))
(define case-clauses (lambda (v) (cddr v)))

(define check-quotations
  (lambda (qs)
    (cond
      [(null? qs) #t]
      [(pair? qs) (and (check-quote-expression (car qs))
                       (check-quotations (cdr qs)))]
      [else #f])))

(define is-case-clause?
  (lambda (c)
    (and (proper-list-of-given-length? c 2)
         (check-quotations (car c))
         (check-expression (cadr c)))))

(define check-case-clauses
  (lambda (cs)
    (cond
      [(and (proper-list-of-given-length? cs 1)
            (is-else-clause? (car cs)))
       #t]
      [(pair? cs)
       (and (is-case-clause? (car cs))
            (check-case-clauses (cdr cs)))]
      [else #f])))

(define check-case-expression
  (lambda (v)
    (and (check-expression (case-key v))
         (let ([cs (case-clauses v)])
           (and (not (null? cs))
                (check-case-clauses cs))))))

;;; ----------------------------------------------------------------------------
;;; printf  (kept minimal and uniform)
;;; ----------------------------------------------------------------------------

(define is-printf?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'printf)
         (list-strictly-longer-than? v 1))))

(define check-printf-expression
  (lambda (v)
    (check-expressions (cdr v))))

;;; ----------------------------------------------------------------------------
;;; Applications / Expressions
;;; ----------------------------------------------------------------------------

(define check-expressions
  (lambda (es)
    (cond
      [(null? es) #t]
      [(pair? es) (and (check-expression (car es))
                       (check-expressions (cdr es)))]
      [else #f])))

;; Application: (<expression> . <expressions>)
;; Recognize anything whose head is NOT a reserved keyword symbol.
(define is-application?
  (lambda (v)
    (and (pair? v)
         (not (and (symbol? (car v)) (keyword? (car v)))))))

(define application-operator car)
(define application-operands cdr)

(define check-application
  (lambda (v vs)
    (and (check-expression v)
         (check-expressions vs))))

;;; ----------------------------------------------------------------------------
;;; Expression checker (BNF-structured; no cycle checks here)
;;; ----------------------------------------------------------------------------

(define (check-expression v)
  (cond
    [(is-number? v)    (check-number v)]
    [(is-boolean? v)   (check-boolean v)]
    [(is-character? v) (check-character v)]
    [(is-string? v)    (check-string v)]
    [(is-variable? v)  (check-variable v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'quote))
     (and (proper-list-of-given-length? v 2)
          (check-quote-expression (quote-1 v)))]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'time))
     (check-time-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'if))
     (check-if-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'unless))
     (check-unless-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'and))
     (check-expressions (and-operands v))]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'or))
     (check-expressions (or-operands v))]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'begin))
     (check-begin-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'lambda))
     (check-lambda-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'trace-lambda))
     (check-trace-lambda-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'let))
     (check-let-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'let*))
     (check-letstar-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'letrec))
     (check-letrec-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'cond))
     (check-cond-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'case))
     (check-case-expression v)]

    [(and (pair? v) (symbol? (car v))
          (eq? (car v) 'printf))
     (check-printf-expression v)]

    [(is-application? v)
     (check-application (application-operator v)
                        (application-operands v))]

    [else #f]))

;;; ----------------------------------------------------------------------------
;;; Toplevel / Program / File I/O
;;; ----------------------------------------------------------------------------

(define (check-toplevel-form v)
  (if (is-definition? v)
      (check-definition v)
      (check-expression v)))

(define (check-program v)
  (cond
    [(cyclic-value? v)
     (unless check-silently
       (printf "check-program -- cyclic input detected~n"))
     #f]
    [(null? v) #t]
    [(pair? v) (and (check-toplevel-form (car v))
                    (check-program (cdr v)))]
    [else
     (unless check-silently
       (printf "check-program -- unrecognized input: ~s~n" v))
     #f]))

(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (begin
          (unless check-silently
            (printf "not a string: ~s~n" filename))
          #f))))
