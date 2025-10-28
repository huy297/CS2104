(define random-char-lowercase
  (lambda ()
    (integer->char (+ (char->integer #\a) (random 26)))))

(define unit-tests-for-add
  (lambda (candidate)
    (and (let ([x (random 100)]
               [y (random 100)])
           (= (candidate x y)
              (candidate y x)))
         (let ([x (random 100)]
               [y (random 100)]
               [z (random 100)])
           (= (candidate (candidate x y) z)
              (candidate x (candidate y z)))))))

(define add_revisited
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1 n2)
                      (if (= n1 0)
                          n2
                          (+ 1 (visit (- n1 1) n2))))])
      (visit n1 n2))))

(define add_revisited_alt
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1)
                      (if (= n1 0)
                          n2
                          (+ 1 (visit (- n1 1)))))])
      (visit n1))))

(define add_revisited_visit
   (lambda (n1 n2)
     (if (= n1 0)
         n2
         (+ 1 (add_revisited_visit (- n1 1) n2)))))

(define add_revisited_lifted
  (lambda (n1 n2)
    (add_revisited_visit n1 n2)))

(define unit-tests-for-one_or_the_other
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '(#t #f))
         (equal? (candidate 1)
                 '(#f #t))
         (equal? (candidate 8)
                 '(#t #f))
         (equal? (candidate 9)
                 '(#f #t))
         ;;;
         )))

(define one_or_the_other
  (lambda (n)
    (letrec ([even? (lambda (n)
                      (if (= n 0)
                          #t
                          (odd? (- n 1))))]
             [odd? (lambda (n)
                     (if (= n 0)
                         #f
                         (even? (- n 1))))])
      (list (even? n) (odd? n)))))

(define one_or_the_other_alt
  (lambda (n)
    (letrec ([visit (lambda (n b)
                      ...)])
      (list (visit n #t) (visit n #f)))))

(define 246?
  (lambda (x)
    (if (memq x '(2 4 6))
        #t
        #f)))

(define 246?_alt
  (lambda (x)
    (case x
      [(2 4 6)
       #t]
      [else
       #f])))