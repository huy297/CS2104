
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

(define make-error-v
  (lambda (msg v)
    (string-append
      (make-error msg)
      (if (or (null? v) (eq? v '()))
          ""
          (string-append ": " (format "~s" v))))))



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


(define ok               
  (lambda () '()))

(define err              
  (lambda (msg) (list (make-error msg))))

(define cat           
  (lambda (a b) (append-two a b)))


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
    (if (number? n)
        '()
        (list (make-error (string-append "Expected number, got " (format "~s" n)))))))

(define check-boolean
  (lambda (b)
    (if (boolean? b)
        '()
        (list (make-error (string-append "Expected boolean, got " (format "~s" b)))))))

(define check-character
  (lambda (c)
    (if (char? c)
        '()
        (list (make-error (string-append "Expected character, got " (format "~s" c)))))))

(define check-string
  (lambda (s)
    (if (string? s)
        '()
        (list (make-error (string-append "Expected string, got " (format "~s" s)))))))

(define check-variable
  (lambda (v)
    (if (and (symbol? v) (not (keyword? v)))
        '()
        (list (make-error (string-append "Expected variable (non-keyword symbol), got " (format "~s" v)))))))


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
    [(not (proper-list-of-given-length? v 3))
     (list (make-error (string-append "Malformed definition, got " (format "~s" v))))]

    [else
     (let ([lhs (cadr v)]
           [rhs (caddr v)])
       (cond
         [(symbol? lhs)
          (append-two
            (check-variable lhs)
            (check-expression rhs))]

         [(and (pair? lhs))
          (append-errors
            (list
              (check-variable (car lhs))
              (check-lambda-formals (cdr lhs))
              (check-expression rhs)))]

         ;; invalid lhs
         [else
          (list (make-error
                  (string-append "Invalid definition head in " (format "~s" v))))]))]))




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
  (append-errors
    (list
      (check-expression test)
      (check-expression consequent)
      (check-expression alternative))))



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
    (append-two
      (check-expression test)
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
  (cond
    [(atomic-value? v)
     '()]
    [(pair? v)
     (append-two
       (check-quote-expression (car v))
       (check-quote-expression (cdr v)))]
    [else
     (list (make-error (string-append "Malformed quotation: " (format "~s" v))))]))



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
      (cond
        ;; malformed formals → 'bad from collect-formal-symbols
        [(eq? vars 'bad)
         (list (make-error "Malformed lambda formals"))]
        ;; duplicate parameters
        [(not (distinct-symbols? vars))
         (list (make-error "Duplicate formal parameters in lambda"))]
        ;; valid
        [else
         '()]))))


;; bodies
(define check-lambda-expression
  (lambda (formals bodies)
    (append-two
      (check-lambda-formals formals)
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
    (append-errors
      (list
        (check-variable name)
        (check-lambda-formals formals)
        (check-expression body)))))


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
    (letrec ((go
              (lambda (bs xs acc)
                (cond
                  ((null? bs) acc)
                  ((pair? bs)
                   (let ((b (car bs)))
                     (if (proper-list-of-given-length? b 2)
                         (let ((x (car b))
                               (rhs (cadr b)))
                           (let ((acc1 (if (check-variable x)
                                           acc
                                           (append-two acc (list (make-error (string-append "let: invalid variable, got " (format "~s" x))))))))
                             (let ((acc2 (if (memq x xs)
                                             (append-two acc1 (list (make-error (string-append "let: duplicate variable, got " (format "~s" x)))))
                                             acc1)))
                               (go (cdr bs)
                                   (cons x xs)
                                   (append-two acc2 (check-expression rhs))))))
                         (go (cdr bs)
                             xs
                             (append-two acc (list (make-error (string-append "let: malformed binding, got " (format "~s" bs)))))))))
                  (else
                   (append-two acc (list (make-error (string-append "let: bindings must be a proper list " (format "~s" bs))))))))))
      (go bs '() '()))))

(define check-let-expression
  (lambda (bindings body)
    (append-two
      (check-let-bindings bindings)
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
                             (append-two acc (list (make-error-v "let*: malformed binding" b)))))))
                  (else
                   (append-two acc (list (make-error-v "let*: bindings must be a proper list" b))))))))
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


(define check-letrec-bindings-go
  (lambda (xs seen acc)
    (cond
      ((null? xs) acc)
      ((not (pair? xs))
       (append-two acc (list (make-error-v "letrec: bindings must be a proper list" xs))))
      (else
       (let ((b (car xs)))
         (if (proper-list-of-given-length? b 2)
             (let ((x (car b))
                   (rhs (cadr b)))
               (let ((acc1 (if (memq x seen)
                               (append-two acc (list (make-error-v "letrec: duplicate variable" xs)))
                               (append-two acc (check-variable x)))))
                 (let ((acc2 (if (or (is-lambda? rhs) (is-trace-lambda? rhs))
                                 acc1
                                 (append-two acc1 (list (make-error-v "letrec: rhs must be lambda/trace-lambda" xs))))))
                   (check-letrec-bindings-go (cdr xs) (cons x seen) acc2))))
             (check-letrec-bindings-go (cdr xs) seen
                                       (append-two acc (list (make-error-v "letrec: malformed binding" xs))))))))))

;; public checker
(define check-letrec-bindings
  (lambda (bs)
    (check-letrec-bindings-go bs '() '())))



(define check-letrec-expression
  (lambda (bindings body)
    (append-two
      (check-letrec-bindings bindings)
      (check-expression body))))




;;; ---------------------------------------------------------------------------
;;; cond / case
;;; ---------------------------------------------------------------------------

;; cond
;; -------- cond (shallow, collect-errors) --------

(define is-cond?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'cond))))

(define cond-clauses cdr)

;; Boolean SHAPE predicates only (no descent, no error lists)
(define is-else-clause?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (eq? (car v) 'else))))

(define is-cond-clause?
  (lambda (v)
    (or (proper-list-of-given-length? v 1)                      ; [e]
        (and (proper-list-of-given-length? v 2)                 ; [e e] (but not else)
             (not (eq? (car v) 'else)))
        (and (proper-list-of-given-length? v 3)                 ; [e => e]
             (eq? (cadr v) '=>)))))

;; Internal helper: enforce "else last" and last-clause shape specifics
(define check-cond-clauses-go
  (lambda (xs)
    (if (null? (cdr xs))                                        ; last clause
        (let ((last (car xs)))
          (cond
            ;; last is an else-clause: must be [else e] (no =>, at least one expr)
            ((pair? last)
             (cond
               ((and (eq? (car last) 'else)
                     (= (length last) 1))
                (list (make-error-v "cond else requires at least one expression" xs)))
               ((and (eq? (car last) 'else)
                     (= (length last) 3)
                     (eq? (cadr last) '=>))
                (list (make-error-v "cond else cannot use =>" xs)))
               ((is-else-clause? last) '())
               ((is-cond-clause? last) '())
               (else (list (make-error-v "cond: malformed last clause" xs)))))
            (else (list (make-error-v "cond: malformed last clause" xs)))))
        ;; non-last: must be cond-clause and NOT else
        (let ((c (car xs)))
          (if (and (is-cond-clause? c)
                   (not (is-else-clause? c)))
              (check-cond-clauses-go (cdr xs))
              (list (make-error-v "cond: malformed clause (else must be last)" xs)))))))

(define check-cond-clauses
  (lambda (cs)
    (cond
      ((not (pair? cs))
       (list (make-error-v "cond: expected non-empty list of clauses" cs)))
      ((not (list? cs))
       (list (make-error-v "cond: clauses must be a proper list" cs)))
      (else
       (check-cond-clauses-go cs)))))

(define check-cond-expression
  (lambda (v)
    (check-cond-clauses (cond-clauses v))))

;; case
;; -------- case (shallow, collect-errors) --------

(define is-case?
  (lambda (v)
    (and (pair? v) (eq? (car v) 'case))))

(define case-key
  (lambda (v)
    (list-ref v 1)))

(define case-clauses
  (lambda (v)
    (cddr v)))

(define check-quote-expression
  (lambda (v)
    (cond
      ((atomic-value? v) '())
      ((pair? v) '())
      (else (list (make-error-v "quote: unsupported datum" v))))))

;; helper: datums must be a proper list (possibly empty)
;; helper: datums must be a proper list (possibly empty)
(define ok-datums-list?
  (lambda (qs)
    (cond
      ((null? qs) '())             ; empty is ok
      ((list? qs) '())             ; proper list ok
      ((pair? qs) (list (make-error-v "case: clause datums list must be proper" qs)))
      (else        (list (make-error-v "case: clause datums must be a list" qs))))))

;; go through clauses, shallow checking only
(define check-case-clauses-go
  (lambda (xs)
    (if (null? (cdr xs))                           ; last clause
        (let ((last (car xs)))
          (cond
            ;; shape ok
            ((and (pair? last) (proper-list-of-given-length? last 2))
             (if (eq? (car last) 'else)
                 (check-expression (cadr last))
                 (append-two
                   (ok-datums-list? (car last))
                   (check-expression (cadr last)))))
            (else (list (make-error-v "case: malformed last clause" last)))))
        ;; non-last clause
        (let ((c (car xs)))
          (cond
            ((and (pair? c)
                  (proper-list-of-given-length? c 2)
                  (not (eq? (car c) 'else)))   ; else must be last
             (append-two
               (ok-datums-list? (car c))
               (append-two
                 (check-expression (cadr c))
                 (check-case-clauses-go (cdr xs)))))
            (else (list (make-error-v "case: malformed clause (else must be last)" c))))))))




(define check-case-clauses
  (lambda (cs)
    (cond
      ((not (pair? cs)) (list (make-error-v "case: expected non-empty list of clauses" cs)))
      ((not (list? cs)) (list (make-error-v "case: clauses must be a proper list" cs)))
      (else (check-case-clauses-go cs)))))

(define check-case-expression
  (lambda (v)
    (let ((key (case-key v))
          (clauses (case-clauses v)))
      (append-two
        (check-expression key)
        (cond
          ((not (pair? clauses))
           (list (make-error "case: expected non-empty list of clauses")))
          ((not (list? clauses))
           (list (make-error "case: clauses must be a proper list")))
          (else
           (check-case-clauses clauses)))))))





;;; ---------------------------------------------------------------------------
;;; Application / Expressions
;;; ---------------------------------------------------------------------------

(define check-expressions
  (lambda (es)
    (cond
      ((null? es) '())
      ((pair? es)
       (append-two
         (check-expression (car es))
         (check-expressions (cdr es))))
      (else
       (list (make-error-v "expressions: expected proper list" es))))))



(define is-application?
  (lambda (v)
    (pair? v)))


(define application-operator car)
(define application-operands cdr)

(define check-application
  (lambda (v vs)
    (append-two
      (check-expression v)
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
      [(is-application? v)
       (check-application (application-operator v) (application-operands v))]
      [else
       (list (make-error (string-append "check-expression: unrecognized input " (format "~s" v))))])))



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
    (cond
      [(cyclic-value? v)
       (list (make-error-v "[Program] Cyclic structure detected" v))]

      [(not (list? v))
       (list (make-error-v "check-program -- program must be a proper list" v))]

      [(null? v) '()]

      [(pair? v)
       (append-two
         (check-toplevel-form (car v))
         (check-program (cdr v)))]

      [else
       (list (make-error-v "check-program -- unrecognized input (not a proper list)" v))])))



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

