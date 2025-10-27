(define (check-expression v)
  (letrec (
           ;; === Helper để đệ quy duyệt expression ===
           [visit
            (lambda (v visited)
              (cond
                ;; --- Cycle detection ---
                [(memq v visited)
                 #t] ; đã check rồi, bỏ qua tránh loop
                [(cyclic-value? v)
                 (begin
                   (printf "[Line ~a] [warning] cyclic structure detected in ~s~n"
                           current-line v)
                   #t)]

                ;; --- Literals ---
                [(is-number? v)    (check-number v)]
                [(is-boolean? v)   (check-boolean v)]
                [(is-character? v) (check-character v)]
                [(is-string? v)    (check-string v)]
                [(is-variable? v)  (check-variable v)]

                ;; --- Time / If / Boolean forms ---
                [(is-time? v)
                 (visit (time-1 v) (cons v visited))]
                [(is-if? v)
                 (check-if-expression (if-1 v) (if-2 v) (if-3 v))]
                [(is-and? v)
                 (check-expressions (and-operands v))]
                [(is-or? v)
                 (check-expressions (or-operands v))]

                ;; --- Begin / Unless / Quote ---
                [(is-begin? v)
                 (and (check-expression (begin-first v))
                      (check-expressions (begin-rest v)))]
                [(is-unless? v)
                 (check-unless-expression (unless-1 v) (unless-2 v))]
                [(is-quote? v)
                 (check-quote-expression (quote-1 v))]

                ;; --- Lambda / Trace-Lambda ---
                [(is-lambda? v)
                 (check-lambda-expression (lambda-formals v)
                                          (lambda-body v))]
                [(is-trace-lambda? v)
                 (check-trace-lambda-expression (trace-lambda-name v)
                                                (trace-lambda-formals v)
                                                (trace-lambda-body v))]

                ;; --- Let-family forms ---
                [(is-let? v)
                 (check-let-expression (let-bindings v) (let-body v))]
                [(is-letstar? v)
                 (check-letstar-expression (letstar-bindings v)
                                           (letstar-body v))]
                [(is-letrec? v)
                 (check-letrec-expression (letrec-bindings v)
                                          (letrec-body v))]

                ;; --- Cond / Case ---
                [(is-cond? v)
                 (check-cond-expression v)]
                [(is-case? v)
                 (check-case-expression v)]

                ;; --- NEW: Handle define forms ---
                [(is-define? v)
                 (check-define-expression v visited)]

                ;; --- Application ---
                [(is-application? v)
                 (let ([visited* (cons v visited)])
                   (check-application
                    (application-operator v)
                    (application-operands v)))]

                ;; --- Không nhận diện được ---
                [else
                 (begin
                   (unless check-silently
                     (printf "[Line ~a] check-expression -- unrecognized input: ~s~n"
                             current-line v))
                   #f)]))]

           ;; === Helper: define detection ===
           [is-define?
            (lambda (v)
              (and (pair? v)
                   (eq? (car v) 'define)))]

           ;; === Helper: check define syntax ===
           [check-define-expression
            (lambda (v visited)
              (cond
                ;; (define <symbol> <expr>)
                [(and (= (length v) 3)
                      (symbol? (cadr v)))
                 (visit (caddr v) (cons v visited))]

                ;; (define (<fn> <params> ...) <body>)
                [(and (pair? (cadr v))
                      (symbol? (caadr v)))
                 (let ([params (cdadr v)]
                       [body   (caddr v)])
                   (if (every symbol? params)
                       (visit body (cons v visited))
                       (begin
                         (printf "[Line ~a] [error] invalid parameters in define: ~s~n"
                                 current-line v)
                         #f)))]

                ;; --- Invalid define form ---
                [else
                 (printf "[Line ~a] [error] invalid define syntax: ~s~n"
                         current-line v)
                 #f]))]

           ;; === Helper every ===
           [every
            (lambda (p? lst)
              (if (null? lst)
                  #t
                  (and (p? (car lst))
                       (every p? (cdr lst)))))])
    ;; Start traversal
    (visit v '())))
