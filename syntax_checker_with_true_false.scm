
;;; ---------------------------------------------------------------------------
;;; Flags
;;; ---------------------------------------------------------------------------

(define check-silently #f)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------
(define current-line 1)

(define make-error
  (lambda (msg)
    msg))


(define append-two
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append-two (cdr a) b)))))

(define append-errors
  (lambda (xss)
    (if (null? xss)
        '()
        (append-two (car xss) (append-errors (cdr xss))))))


;(define (proper-list-of-given-length? v n)
;  (and (list? v)
;       (= (length v) n)))

(define (proper-list-of-given-length? v n)
  (or (and (zero? n) (null? v))
      (and (pair? v)
           (proper-list-of-given-length? (cdr v) (sub1 n)))))


(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit
              (lambda (v i)
                (and (pair? v)
                     (or (= i 0)
                         (visit (cdr v) (- i 1)))))])
      (and (>= n 0)
           (visit v n)))))


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


;;; ---------------------------------------------------------------------------
;;; Cycle detection 
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

(define is-number?
  (lambda (v)
    (number? v)))

(define is-boolean?
  (lambda (v)
    (boolean? v)))

(define is-character?
  (lambda (v)
    (char? v)))

(define is-string?
  (lambda (v)
    (string? v)))

(define is-variable?
  (lambda (v)
    (and (symbol? v)
         (not (keyword? v)))))

(define check-number
  (lambda (n)
    #t))

(define check-boolean
  (lambda (b)
    #t))

(define check-character
  (lambda (c)
    (char? c)))

(define check-string
  (lambda (s)
    (string? s)))

(define check-variable
  (lambda (v)
    (and (symbol? v)
         (not (keyword? v)))))


;;; ---------------------------------------------------------------------------
;;; Definitions
;;; ---------------------------------------------------------------------------

(define (is-definition? v)
  (and (pair? v)
       (eq? (car v) 'define)))

(define define-1
  (lambda (v)
    (list-ref v 1)))

(define define-2
  (lambda (v)
    (list-ref v 2)))


;; (define var expr)  
(define (check-definition v)
  (cond
    [(proper-list-of-given-length? v 3)
     (let ([lhs (cadr v)] [rhs (caddr v)])
       (cond
         [(check-variable lhs)
          (check-expression rhs)]
         [(and (pair? lhs)
               (check-variable (car lhs))
               (check-lambda-formals (cdr lhs)))
          (check-expression rhs)]
         [else #f]))]
    [else #f]))



;;; ---------------------------------------------------------------------------
;;; Core forms: time / if / unless / quote / and / or / begin
;;; ---------------------------------------------------------------------------

;; time
(define is-time?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'time))))

(define time-1
  (lambda (v)
    (list-ref v 1)))

(define check-time-expression
  (lambda (v)
    (check-expression v)))


;; if
(define is-if?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (eq? (car v) 'if))))

(define if-1
  (lambda (v)
    (list-ref v 1)))

(define if-2
  (lambda (v)
    (list-ref v 2)))

(define if-3
  (lambda (v)
    (list-ref v 3)))


(define (check-if-expression test consequent alternative)
  (let ([ok1 (check-expression test)]
        [ok2 (check-expression consequent)]
        [ok3 (check-expression alternative)])
    (and ok1 ok2 ok3)))


;; unless
(define is-unless?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'unless))))

(define unless-1
  (lambda (v)
    (list-ref v 1)))

(define unless-2
  (lambda (v)
    (list-ref v 2)))


(define check-unless-expression
  (lambda (test consequent)
    (and (check-expression test)
         (check-expression consequent))))

;; quote
(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'quote))))

(define quote-1
  (lambda (v)
    (list-ref v 1)))

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
         (eq? (car v) 'begin)
         (list-strictly-longer-than? v 1))))

(define begin-first cadr)
(define begin-rest  cddr)

;;; ---------------------------------------------------------------------------
;;; Lambda / trace-lambda 
;;; ---------------------------------------------------------------------------
;(define is-lambda? (lambda (v) (and (pair? v) (eq? (car v) 'lambda) (pair? (cdr v)) (pair? (cddr v)))))


(define is-lambda?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'lambda))))


(define lambda-formals
  (lambda (v)
    (cadr v)))

;; separates logically related definitions
(define lambda-body
  (lambda (v)
    (cddr v)))


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

(define trace-lambda-name
  (lambda (v)
    (list-ref v 1)))

(define trace-lambda-formals
  (lambda (v)
    (list-ref v 2)))

(define trace-lambda-body
  (lambda (v)
    (list-ref v 3)))


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
        
(define let-bindings
  (lambda (v)
    (list-ref v 1)))

(define let-body
  (lambda (v)
    (list-ref v 2)))


(define check-let-bindings
  (lambda (bs)
    (letrec ([go (lambda (bs xs)
                   (cond
                     [(null? bs) #t]
                     [(pair? bs)
                      (let ([b (car bs)])
                        (and (proper-list-of-given-length? b 2)
                             (check-variable (car b))
                             (not (memq (car b) xs))
                             (check-expression (cadr b))
                             (go (cdr bs) (cons (car b) xs))))]
                     [else #f]))])
      (go bs '()))))

(define check-let-expression
  (lambda (bindings body)
    (and (check-let-bindings bindings)  
         (check-expression body))))

;; let*
(define is-letstar?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'let*))))

(define letstar-bindings
  (lambda (v)
    (list-ref v 1)))

(define letstar-body
  (lambda (v)
    (list-ref v 2)))


(define check-letstar-bindings
  (lambda (bs)
    (letrec ((go
              (lambda (bs acc)
                (cond
                  ((null? bs) acc)
                  ((pair? bs)
                   (let ((b (car bs)))
                     (if (proper-list-of-given-length? b 2)
                         (go (cdr bs)
                             (append-two acc
                               (append-two
                                 (check-variable (car b))
                                 (check-expression (cadr b)))))
                         (go (cdr bs)
                             (append-two acc (list (make-error "let*: malformed binding")))))))
                  (else
                   (append-two acc (list (make-error "let*: bindings must be a proper list"))))))))
      (go bs '()))))

(define check-letstar-expression
  (lambda (bindings body)
    (append-two
      (check-letstar-bindings bindings)
      (check-expression body))))

;; letrec
(define is-letrec?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (eq? (car v) 'letrec))))

(define letrec-bindings
  (lambda (v)
    (list-ref v 1)))

(define letrec-body
  (lambda (v)
    (list-ref v 2)))


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
    (and (check-letrec-bindings bindings)
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
    (letrec ([go (lambda (cs)
                   (cond
                     [(null? (cdr cs))
                      (or (is-else-clause? (car cs))
                          (is-cond-clause? (car cs)))]
                     [else
                      (and (is-cond-clause? (car cs))
                           (not (is-else-clause? (car cs)))
                           (go (cdr cs)))]))])
      (and (pair? cs) (go cs)))))


(define check-cond-expression
  (lambda (v)
    (check-cond-clauses (cond-clauses v))))

;; case
(define is-case?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'case))))
(define case-key
  (lambda (v)
    (list-ref v 1)))

(define case-clauses
  (lambda (v)
    (cddr v)))


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
    (pair? v)))


(define application-operator car)
(define application-operands cdr)

(define check-application
  (lambda (v vs)
    (and (check-expression v)
         (check-expressions vs))))

;;

(define check-expression
  (lambda (v)
    (cond
      [(is-number? v)    (check-number v)]
      [(is-boolean? v)   (check-boolean v)]
      [(is-character? v) (check-character v)]
      [(is-string? v)    (check-string v)]
      [(is-variable? v)  (check-variable v)]
      [(is-time? v)      (check-time-expression (time-1 v))]
      [(is-if? v)        (check-if-expression (if-1 v) (if-2 v) (if-3 v))]
      [(is-and? v)       (check-expressions (and-operands v))]
      [(is-or? v)        (check-expressions (or-operands v))]
      [(is-begin? v)     (and (check-expression (begin-first v))
                              (check-expressions (begin-rest v)))]
      [(is-unless? v)    (check-unless-expression (unless-1 v) (unless-2 v))]
      [(is-quote? v)     (check-quote-expression (quote-1 v))]
      [(is-lambda? v)    (check-lambda-expression (lambda-formals v) (lambda-body v))]
      [(is-trace-lambda? v)
       (check-trace-lambda-expression (trace-lambda-name v)
                                      (trace-lambda-formals v)
                                      (trace-lambda-body v))]
      [(is-let? v)       (check-let-expression (let-bindings v) (let-body v))]
      [(is-letstar? v)   (check-letstar-expression (letstar-bindings v) (letstar-body v))]
      [(is-letrec? v)    (check-letrec-expression (letrec-bindings v) (letrec-body v))]
      [(is-cond? v)     (check-cond-expression v)]
      [(is-case? v)     (check-case-expression v)]

   ;;   [(is-printf? v)
   ;; (check-printf-expression v)]


      [(is-application? v)
       (check-application (application-operator v) (application-operands v))]
      [else
       (begin
         (unless check-silently
           (printf "check-expression -- unrecognized input: ~s~n" v))
         #f)])))



;;; ------


;;; ---

;;; ---------------------------------------------------------------------------
;;; Toplevel / Program / File I/O
;;; ---------------------------------------------------------------------------

(define check-toplevel-form
  (lambda (v)
    (if (is-definition? v)
        (if (check-definition v)
            '()
            (list (make-error "Malformed definition")))
        (if (check-expression v)
            '()
            (list (make-error "Malformed expression"))))))


(define check-program
  (lambda (v)
    (cond
      [(cyclic-value? v)
       (list (make-error "[Program] Cyclic structure detected"))]

      [(null? v)
       '()]

      [(pair? v)
       (append-two
         (check-toplevel-form (car v))
         (check-program (cdr v)))]

      [else
       (list (make-error "check-program -- unrecognized input (not a proper list)"))])))


(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit
                  (lambda ()
                    (let ([in (read p)])
                      (if (eof-object? in)
                          '()
                          (cons in (visit)))))])
          (visit))))))


(define check-file
  (lambda (filename)
    (if (string? filename)
        (let* ([program (read-file filename)]
               [errors (check-program program)])
          (if (null? errors)
              (begin
                (unless check-silently
                  (printf "BNF check passed: no syntax errors.~n"))
                #t)
              (begin
                (for-each (lambda (e) (printf "~a~n" e)) errors)
                #f)))
        (begin
          (printf "Filename is not a string: ~s~n" filename)
          #f))))

