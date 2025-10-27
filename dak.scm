;; This Scheme program has correct parentheses
;; but lots of semantic and logical errors for testing check-file.

;; --- Undefined variable ---
(+ foo 10)

;; --- Invalid function call (string as function) ---
("hello" 5)

;; --- Type mismatch in arithmetic ---
(+ "abc" 5)

