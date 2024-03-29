#lang racket

(require rackunit "interp.rkt" "parser.rkt" "ast.rkt")

(module+ test
  ;; Test for simple let with multiple bindings
  (check-equal? (interp-err (parse-prog '((let ((x 1) (y 2)) (+ x y))))) 3)
  
  ;; Test for let with expressions in bindings
  (check-equal? (interp-err (parse-prog '((let ((x 1) (y (add1 x))) (+ x y))))) 3)
  
  ;; Test for let with shadowing
  (check-equal? (interp-err (parse-prog '((let ((x 1)) (let ((x (add1 x))) (add1 x)))))) 3)
  
  ;; Test for let with invalid binding (error case)
  (check-equal? (interp-err (parse-prog '((let ((x 1) (x (add1 x))) (add1 x))))) (Err "Duplicate identifier in let bindings"))
  
  ;; Additional tests for your implementation
  ;; Add more tests as needed to ensure your implementation is correct
)
