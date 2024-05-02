#lang racket

(require rackunit "interp.rkt" "parser.rkt" "ast.rkt")

(module+ test
  ;; Test for let with bindings of different sizes (error case)
  (check-equal? (interp-err (parse-prog '((let ((x 1) (y 2 3)) (+ x y))))) (Err "All bindings must have the same size"))
  ;; Additional tests for your implementation
  ;; Add more tests as needed to ensure your implementation is correct
)
