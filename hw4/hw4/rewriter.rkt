#lang racket

;; Provides the cond->if and currify functions.
(provide cond->if currify)

;; Imports necessary modules for abstract syntax trees and parsing capabilities.
(require "ast.rkt" "parser.rkt")

;; Function to transform 'cond' expressions into 'if' expressions recursively.
(define (cond->if e)
  (match e
    [(Cond cs x) (cond->if-assist cs x)]  ; Handle 'cond' structures with assistance function.
    [(BinOp op e1 e2) (BinOp op (cond->if e1) (cond->if e2))]  ; Process binary operations recursively.
    [(UnOp op e2) (UnOp op (cond->if e2))]  ; Process unary operations recursively.
    [(If e1 e2 e3) (If (cond->if e1) (cond->if e2) (cond->if e3))]  ; Transform 'if' expressions recursively.
    [(App e1 e2) (App (cond->if e1) (cond->if e2))]  ; Transform function applications recursively.
    [(Lam params es) (Lam (cond->if params) (cond->if es))]  ; Transform lambda expressions recursively.
    [(list x) (list (cond->if x))]  ; Process lists containing expressions recursively.
    [e e]))  ; Return the expression as is if it does not match any pattern.

;; Function to transform function definitions and calls to their currified form.
(define (currify e)
  (match e
    [(App f args) (curry-args (curry-application f args) args)]  ; Curry applications of functions.
    [(Lam params es) (curry-application e '())]  ; Curry lambda functions.
    [(BinOp op e1 e2) (BinOp op (currify e1) (currify e2))]  ; Recursively curry binary operations.
    [(UnOp op e2) (UnOp op (currify e2))]  ; Recursively curry unary operations.
    [(If e1 e2 e3) (If (currify e1) (currify e2) (currify e3))]  ; Recursively curry 'if' expressions.
    [(Cond cs x) (Cond (map (λ (c) (list (currify (first c)) (currify (second c)))) cs) (currify x))]  ; Recursively curry 'cond' expressions.
    [(Let x e2 e3) (Let x (currify e2) (currify e3))]  ; Recursively curry 'let' bindings.
    [(Var x) e]  ; Return the variable as is if it does not require currification.
    [(list x) (map currify (list x))]  ; Process lists containing expressions recursively.
    [e e]))  ; Return the expression as is if it does not match any pattern.

;; Helper function to assist in transforming 'cond' clauses into nested 'if' expressions.
(define (cond->if-assist clauses x)
  (match clauses
    [(Cond cs x) (cond->if (Cond cs x))]  ; Recursively transform 'cond' expressions.
    [(list (list p a) rest) (If (cond->if p) (cond->if a) (cond->if-assist rest x))]  ; Create nested 'if' from 'cond' clauses.
    [(list p a) (If (cond->if p) (cond->if a) (cond->if-assist '() x))]  ; Handle the last clause in 'cond' expression.
    [else x]))  ; Return else part if present.

;; Function to curry a function application.
(define (curry-application f args)
  (match f
    [(Lam (list param rest) body) (Lam (list param) (curry-application (Lam (list rest) body) args))]  ; Curry multi-parameter lambdas.
    [(Lam param body) f]  ; Return lambda if it's already a single-parameter lambda.
    [(App e as) (curry-args (curry-application e as) as)]  ; Curry nested function applications.
    [f f]))  ; Return function as is if no transformation is necessary.

;; Recursively apply arguments to a curried function.
(define (curry-args curried args)
  (match args
    [(list a rest) (curry-args (App curried (list a)) (currify rest))]  ; Curry each argument in the application.
    ['() (App curried '())]  ; Return the curried function application with no further arguments.
    [(list a) (App curried (list a))]  ; Apply a single argument to the curried function.
    [a (App curried (list a))]))  ; Apply a single argument to the curried function.
