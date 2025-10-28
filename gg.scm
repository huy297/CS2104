(begin
              (define z 10)
              (cond [(> z 5) (begin (set! z (- z 1)) z)]
                    [else 0]))