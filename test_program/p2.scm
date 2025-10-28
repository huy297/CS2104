(case x
  ((1 2 3) 'small)
  ((4 5) 'medium)
  (else 'large))

(case 2
  ((1) 'one)
  ((2) 'two)
  (else 'other))

(case (+ 1 2)
  ((1 2) (display "low"))
  ((3 4) (begin (display "high") (newline)))
  (else (display "unknown")))

(case a
  ((x y)
   (case b
     ((1) 'inner-one)
     (else 'inner-other)))
  (else 'outer))

(case val
  ((1) 'ok)
  ((2 3) 'ok2))

(case 'apple
  (("apple" "banana") 'fruit)
  (("carrot") 'vegetable)
  (else 'unknown))

(case 'a
  (('a 'b 'c) 'letters)
  (('x 'y) 'others)
  (else 'default))

