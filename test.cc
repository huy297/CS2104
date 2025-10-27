;; Test file with multiple errors and tricky cases
;; Should trigger different errors with accurate [Line n] reporting

;; --- Normal expression ---
(define a '(+ 1 2))

;; --- Invalid syntax: missing parenthesis ---
(define b '(+ 1 2)     ; <- missing closing )
(define c '(1 2 3))    ; next line should still read fine

;; --- Undefined variable ---
(+ x 10)

;; --- Nested structure with cyclic reference ---
(define cyc (list 'a 'b 'c))
(set-cdr! (cdr cyc) cyc) ; creates cycle at runtime

;; --- Invalid type ---
(define str "hello")
(+ str 3)

;; --- Completely unrecognized expression ---
#@(something weird)   ; not valid Scheme syntax

;; --- Valid expression again (should not trigger error) ---
(define ok '(lambda (x) (+ x 1)))

;; --- Cyclic literal ---
(define badlit #0=(+ 1 . #0#))
(check-expression badlit)
