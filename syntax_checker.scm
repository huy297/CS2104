
;;; ---------------------------------------------------------------------------
;;; Flags
;;; ---------------------------------------------------------------------------

(define check-silently #f)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------
(define current-line 1)

(define (proper-list-of-given-length? v n)
  (and (list? v) (= (length v) n)))

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
    (letrec ([loop (lambda (xs seen)
                     (cond [(null? xs) #t]
                           [(memq (car xs) seen) #f]
                           [else (loop (cdr xs) (cons (car xs) seen))]))])
      (loop xs '()))))

;;; ---------------------------------------------------------------------------
;;; Cyclic detection 
;;; ---------------------------------------------------------------------------

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
    (letrec ([visit (lambda (v cs)
                      (cond
                        [(atomic-value? v) #f]
                        [(memq v cs) #t]
                        [(pair? v)
                         (let ([csp (cons v cs)])
                           (or (visit (car v) csp)
                               (visit (cdr v) csp)))]
                        [else #f]))])
      (visit v '()))))

;;; ---------------------------------------------------------------------------
;;; Ground predicates & checks
;;; ---------------------------------------------------------------------------

(define is-number?    (lambda (v) (number? v)))
(define is-boolean?   (lambda (v) (boolean? v)))
(define is-character? (lambda (v) (char? v)))
(define is-string?    (lambda (v) (string? v)))
(define is-variable?  (lambda (v) (and (symbol? v) (not (keyword? v)))))

(define check-number    (lambda (n) #t))
(define check-boolean   (lambda (b) #t))
(define check-character (lambda (c) (char? c)))
(define check-string    (lambda (s) (string? s)))
(define check-variable  (lambda (v) (and (symbol? v) (not (keyword? v)))))

;;; ---------------------------------------------------------------------------
;;; Definitions
;;; ---------------------------------------------------------------------------

(define (is-definition? v)
  (and (pair? v)
       (eq? (car v) 'define)))

(define define-1 (lambda (v) (list-ref v 1)))
(define define-2 (lambda (v) (list-ref v 2)))

;; (define var expr)  |  (define (f args) expr)
(define (check-definition v)
  (cond
    [(and (proper-list-of-given-length? v 3)
          (symbol? (cadr v)))
     (and (check-variable (cadr v))
          (check-expression (caddr v)))]
    [(and (proper-list-of-given-length? v 3)
          (pair? (cadr v))
          (symbol? (caadr v)))
     (and (check-variable (caadr v))
          (check-lambda-formals (cdadr v))
          (check-expression (caddr v)))]
    [else #f]))

;;; ---------------------------------------------------------------------------
;;; Core forms: time / if / unless / quote / and / or / begin
;;; ---------------------------------------------------------------------------

;; time
(define is-time?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'time))))
(define time-1 (lambda (v) (list-ref v 1)))
(define check-time-expression (lambda (v) (check-expression v)))

;; if
(define is-if?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (eq? (car v) 'if))))
(define if-1 (lambda (v) (list-ref v 1)))
(define if-2 (lambda (v) (list-ref v 2)))
(define if-3 (lambda (v) (list-ref v 3)))

(define check-if-expression
  (lambda (test consequent alternative)
    (and (check-expression test)
         (check-expression consequent)
         (check-expression alternative))))

;; unless
(define is-unless?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'unless))))
(define unless-1 (lambda (v) (list-ref v 1)))
(define unless-2 (lambda (v) (list-ref v 2)))

(define check-unless-expression
  (lambda (test consequent)
    (and (check-expression test)
         (check-expression consequent))))

;; quote
(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'quote))))
(define quote-1 (lambda (v) (list-ref v 1)))

(define check-quote-expression
  (lambda (v)
    (cond [(or (number? v) (boolean? v) (char? v)
               (string? v) (symbol? v) (null? v)) #t]
          [(pair? v)
           (and (check-quote-expression (car v))
                (check-quote-expression (cdr v)))]
          [else #f])))

;; and/or
(define is-and? (lambda (v) (and (pair? v) (eq? (car v) 'and))))
(define and-operands cdr)

(define is-or?  (lambda (v) (and (pair? v) (eq? (car v) 'or))))
(define or-operands cdr)

;; begin (>= 1 expr)
(define is-begin?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'begin)
         (list-strictly-longer-than? v 1))))
(define begin-first cadr)
(define begin-rest  cddr)

;;; ---------------------------------------------------------------------------
;;; Lambda / trace-lambda 
;;; ---------------------------------------------------------------------------

(define is-lambda?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'lambda)
         (pair? (cdr v))
         (pair? (cddr v)))))

(define lambda-formals (lambda (v) (cadr v)))
;;
(define lambda-body    (lambda (v) (cddr v)))

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

;; bodies
(define check-lambda-expression
  (lambda (formals bodies)
    (and (check-lambda-formals formals)
         (check-expressions bodies))))

;; trace-lambda
(define is-trace-lambda?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (eq? (car v) 'trace-lambda))))
(define trace-lambda-name    (lambda (v) (list-ref v 1)))
(define trace-lambda-formals (lambda (v) (list-ref v 2)))
(define trace-lambda-body    (lambda (v) (list-ref v 3)))

(define check-trace-lambda-expression
  (lambda (name formals body)
    (and (check-variable name)
         (check-lambda-formals formals)
         (check-expression body))))

;;; ---------------------------------------------------------------------------
;;; let / let* / letrec
;;; ---------------------------------------------------------------------------

;; let
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

;; let*
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

;; letrec
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

;;; ---------------------------------------------------------------------------
;;; cond / case
;;; ---------------------------------------------------------------------------

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
           (and (list? cs)
                (not (null? cs))
                (check-case-clauses cs))))))

;;; ---------------------------------------------------------------------------
;;; printf 
;;; ---------------------------------------------------------------------------

(define is-printf?
  (lambda (v)
    (and (pair? v)
         (eq? (car v) 'printf)
         (>= (length v) 2))))

(define check-printf-expression
  (lambda (v)
    (let ([args (cdr v)])
      (and (check-expressions args) #t))))

;;; ---------------------------------------------------------------------------
;;; Application / Expressions
;;; ---------------------------------------------------------------------------

(define check-expressions
  (lambda (es)
    (cond [(null? es) #t]
          [(pair? es) (and (check-expression (car es))
                           (check-expressions (cdr es)))]
          [else #f])))

(define is-application?
  (lambda (v)
    (and (pair? v)
         (let ([w (car v)])
           (if (symbol? w) (not (keyword? w)) #t)))))

(define application-operator car)
(define application-operands cdr)

(define check-application
  (lambda (v vs)
    (and (check-expression v)
         (check-expressions vs))))

;; every
(define (every p xs)
  (cond
    [(null? xs) #t]
    [(pair? xs) (and (p (car xs)) (every p (cdr xs)))]
    [else #f]))

;;

(define (check-expression v)
  (letrec ([visit
            (lambda (v visited)
              (cond
                [(memq v visited)
                 #t] ; have visited
                [(cyclic-value? v)
                 (begin
                   (printf "[Line ~a] Cyclic structure detected in ~s~n" current-line v)
                   #t)]
                [(is-number? v)    (check-number v)]
                [(is-boolean? v)   (check-boolean v)]
                [(is-character? v) (check-character v)]
                [(is-string? v)    (check-string v)]
                [(is-variable? v)  (check-variable v)]
                [(is-time? v)
                 (visit (time-1 v) (cons v visited))]
                [(is-if? v)
                 (check-if-expression (if-1 v) (if-2 v) (if-3 v))]
                [(is-and? v)
                 (check-expressions (and-operands v))]
                [(is-or? v)
                 (check-expressions (or-operands v))]
                [(is-begin? v)
                 (and (check-expression (begin-first v))
                      (check-expressions (begin-rest v)))]
                [(is-unless? v)
                 (check-unless-expression (unless-1 v) (unless-2 v))]
                [(is-quote? v)
                 (check-quote-expression (quote-1 v))]
                [(is-lambda? v)
                 (check-lambda-expression (lambda-formals v)
                                          (lambda-body v))]
                [(is-trace-lambda? v)
                 (check-trace-lambda-expression (trace-lambda-name v)
                                                (trace-lambda-formals v)
                                                (trace-lambda-body v))]
                [(is-let? v)
                 (check-let-expression (let-bindings v) (let-body v))]
                [(is-letstar? v)
                 (check-letstar-expression (letstar-bindings v)
                                           (letstar-body v))]
                [(is-letrec? v)
                 (check-letrec-expression (letrec-bindings v)
                                          (letrec-body v))]
                [(is-cond? v)
                 (check-cond-expression v)]
                [(is-case? v)
                 (check-case-expression v)]
                [(is-application? v)
                 (let ([visited* (cons v visited)])
                   (check-application
                     (application-operator v)
                     (application-operands v)))]
                [else
                 (begin
                   (unless check-silently
                     (printf "[Line ~a] check-expression -- unrecognized input: ~s~n" current-line v))
                   #f)]))])
    (visit v '())))



;;; ------


;;; ---

;;; ---------------------------------------------------------------------------
;;; Toplevel / Program / File I/O
;;; ---------------------------------------------------------------------------

(define check-toplevel-form
  (lambda (v)
    (if (is-definition? v)
        (check-definition v)
        (check-expression v))))

(define check-program
  (lambda (v)
    (cond [(null? v) #t]
          [(pair? v) (and (check-toplevel-form (car v))
                          (check-program (cdr v)))]
          [else
           (begin
             (unless check-silently
               (printf "check-program -- unrecognized input: ~s~n" v))
             #f)])))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (p)
      (let ([line 1])
        (letrec
            ([visit
              (lambda (acc)
                (let ([ch (peek-char p)])
                  (cond
                    ;; EOF
                    [(eof-object? ch)
                     (reverse acc)]

                    ;; newline -> tăng line
                    [(char=? ch #\newline)
                     (read-char p)
                     (set! line (+ line 1))
                     (visit acc)]

                    ;; comment -> skip đến hết dòng
                    [(char=? ch #\;)
                     (let skip ([c (read-char p)])
                       (if (and (not (eof-object? c))
                                (not (char=? c #\newline)))
                           (skip (read-char p))
                           (begin
                             (set! line (+ line 1))
                             (visit acc))))]

                    ;; đọc 1 expression
                    [else
                     (let ([expr (read p)])
                       (visit (cons (cons line expr) acc)))])))])  ; <-- kết thúc lambda visit
          (visit '()))))))  ; <-- kết thúc letrec và let


(define (check-file filename)
  (if (string? filename)
      (let ([forms (read-file filename)])
        (for-each
         (lambda (line-form)
           (set! current-line (car line-form))
           (check-toplevel-form (cdr line-form)))
         forms))
      (begin
        (printf "not a string: ~s~n" filename)
        #f)))

