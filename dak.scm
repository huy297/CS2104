(define check-silently #t)

(define (proper-list-of-given-length? v n)
  (and (list? v) (= (length v) n)))

(define keyword?
  (lambda (w)
    (and (symbol? w)
         (memq w '(define time if cond else case and or
                    let let* letrec begin unless quote
                    lambda trace-lambda)))))

(define is-number?   (lambda (v) (number? v)))
(define is-boolean?  (lambda (v) (boolean? v)))
(define is-character?(lambda (v) (char? v)))
(define is-string?   (lambda (v) (string? v)))
(define is-variable? (lambda (v) (and (symbol? v) (not (keyword? v)))))

(define (is-definition? v)
  (and (pair? v)
       (eq? (car v) 'define)))
(define define-1 (lambda (v) (list-ref v 1)))
(define define-2 (lambda (v) (list-ref v 2)))

(define (check-definition v)
  (cond
    ;; (define var expr)
    [(and (proper-list-of-given-length? v 3)
          (symbol? (cadr v)))
     (and (check-variable (cadr v))
          (check-expression (caddr v)))]
    ;; (define (f args) expr)
    [(and (proper-list-of-given-length? v 3)
          (pair? (cadr v))
          (symbol? (caadr v)))
     (and (check-variable (caadr v))
          (check-lambda-formals (cdadr v))
          (check-expression (caddr v)))]
    [else #f]))

(define is-time?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'time))))
(define time-1 (lambda (v) (list-ref v 1)))

(define is-if?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (eq? (car v) 'if))))
(define if-1 (lambda (v) (list-ref v 1)))
(define if-2 (lambda (v) (list-ref v 2)))
(define if-3 (lambda (v) (list-ref v 3)))

(define is-unless?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'unless))))
(define unless-1 (lambda (v) (list-ref v 1)))
(define unless-2 (lambda (v) (list-ref v 2)))

(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'quote))))
(define quote-1 (lambda (v) (list-ref v 1)))

(define is-and? (lambda (v) (and (pair? v) (eq? (car v) 'and))))
(define and-operands cdr)
(define is-or?  (lambda (v) (and (pair? v) (eq? (car v) 'or))))
(define or-operands cdr)

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v) (- i 1)))))])
      (and (>= n 0) (visit v n)))))

(define is-begin?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'begin)
         (list-strictly-longer-than? v 1))))
(define begin-first cadr)
(define begin-rest  cddr)

(define is-lambda?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'lambda))))
(define lambda-formals (lambda (v) (list-ref v 1)))
(define lambda-body    (lambda (v) (list-ref v 2)))

(define is-trace-lambda?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (eq? (car v) 'trace-lambda))))
(define trace-lambda-name    (lambda (v) (list-ref v 1)))
(define trace-lambda-formals (lambda (v) (list-ref v 2)))
(define trace-lambda-body    (lambda (v) (list-ref v 3)))

(define distinct-symbols?
  (lambda (xs)
    (letrec ([loop (lambda (xs seen)
                     (cond [(null? xs) #t]
                           [(memq (car xs) seen) #f]
                           [else (loop (cdr xs) (cons (car xs) seen))]))])
      (loop xs '()))))

(define collect-formal-symbols
  (lambda (fs)
    (cond [(null? fs) '()]
          [(symbol? fs) (if (is-variable? fs) (list fs) 'bad)]
          [(pair? fs)
           (let ([a (car fs)] [d (cdr fs)])
             (if (is-variable? a)
                 (let ([rest (collect-formal-symbols d)])
                   (if (eq? rest 'bad) 'bad (cons a rest)))
                 'bad))]
          [else 'bad])))

(define check-lambda-formals
  (lambda (fs)
    (let ([vars (collect-formal-symbols fs)])
      (and (not (eq? vars 'bad))
           (distinct-symbols? vars)))))

(define check-lambda-expression
  (lambda (formals body)
    (and (check-lambda-formals formals)
         (check-expression body))))

(define check-trace-lambda-expression
  (lambda (name formals body)
    (and (check-variable name)
         (check-lambda-formals formals)
         (check-expression body))))

(define is-let?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'let))))
(define let-bindings (lambda (v) (list-ref v 1)))
(define let-body     (lambda (v) (list-ref v 2)))

(define check-let-bindings
  (lambda (bs xs)
    (cond
      [(null? bs) #t]
      [(pair? bs)
       (let ([b (car bs)])
         (and (proper-list-of-given-length? b 2)
              (check-variable (car b))
              (not (memq (car b) xs))
              (check-expression (cadr b))
              (check-let-bindings (cdr bs) (cons (car b) xs))))]
      [else #f])))

(define check-let-expression
  (lambda (bindings body)
    (and (check-let-bindings bindings '())
         (check-expression body))))

(define is-letstar?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'let*))))
(define letstar-bindings (lambda (v) (list-ref v 1)))
(define letstar-body     (lambda (v) (list-ref v 2)))

(define check-letstar-bindings
  (lambda (bs)
    (cond
      [(null? bs) #t]
      [(pair? bs)
       (let ([b (car bs)])
         (and (proper-list-of-given-length? b 2)
              (check-variable (car b))
              (check-expression (cadr b))
              (check-letstar-bindings (cdr bs))))]
      [else #f])))

(define check-letstar-expression
  (lambda (bindings body)
    (and (check-letstar-bindings bindings)
         (check-expression body))))

(define is-letrec?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'letrec))))
(define letrec-bindings (lambda (v) (list-ref v 1)))
(define letrec-body     (lambda (v) (list-ref v 2)))

(define check-letrec-bindings
  (lambda (bs)
    (cond
      [(null? bs) #t]
      [(pair? bs)
       (let* ([b (car bs)]
              [ok-shape (proper-list-of-given-length? b 2)]
              [x (and ok-shape (car b))]
              [rhs (and ok-shape (cadr b))])
         (and ok-shape
              (check-variable x)
              (or (and (is-lambda? rhs)
                       (check-lambda-expression (lambda-formals rhs)
                                                (lambda-body rhs)))
                  (and (is-trace-lambda? rhs)
                       (check-trace-lambda-expression (trace-lambda-name rhs)
                                                      (trace-lambda-formals rhs)
                                                      (trace-lambda-body rhs))))
              (let ([rest (cdr bs)])
                (and (distinct-symbols? (cons x (map car rest)))
                     (check-letrec-bindings rest)))))]
      [else #f])))

(define check-letrec-expression
  (lambda (bindings body)
    (and (list? bindings)
         (check-letrec-bindings bindings)
         (check-expression body))))

(define check-number   (lambda (n) #t))
(define check-boolean  (lambda (b) #t))
(define check-character(lambda (c) (char? c)))
(define check-string   (lambda (s) (string? s)))
(define check-variable (lambda (v) (and (symbol? v) (not (keyword? v)))))

;; ==== cond ====
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
    (check-cond-clauses (cond-clauses v)))
)

;; ==== case ====
(define is-case?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'case))))

(define case-key     (lambda (v) (list-ref v 1)))
(define case-clauses (lambda (v) (cddr v)))  ; phần sau <key>

(define check-quotations
  (lambda (qs)
    (cond
      [(null? qs) #t]
      [(pair? qs) (and (check-quote-expression (car qs))
                       (check-quotations (cdr qs)))]
      [else #f])))

(define is-case-clause?
  (lambda (c) ; ( <quotations> <expression> )
    (and (proper-list-of-given-length? c 2)
         (check-quotations (car c))
         (check-expression (cadr c)))))

;; <case-clauses> ::= (<else-clause>) | (<case-clause> . <case-clauses>)
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
           (and (list? cs)                 
                (not (null? cs))            
                (check-case-clauses cs))))))



