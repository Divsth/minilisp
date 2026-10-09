; 1. Arithmetic and Nested Expressions
(+ 10 (* 3 4))

; 2. Variable Definitions
(define pi 3)
(define radius 5)
(* pi (* radius radius))

; 3. Conditionals
(if (> radius 0) "positive radius" "non-positive radius")

; 4. First-Class Functions and Closures
(define makeMultiplier (lambda (factor) (lambda (n) (* factor n))))
(define triple (makeMultiplier 3))
(triple 10)

; 5. List Primitives
(define numbers '(1 2 3 4 5))
(car numbers)
(cdr numbers)
(cons 0 numbers)

; 6. Recursive List Processing
(define length (lambda (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))))
(length numbers)