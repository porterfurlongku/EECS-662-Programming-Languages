#lang racket

(require "ast.rkt")

(provide parse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?) (Val s)]
    [(? boolean?) (Val s)]
    [(list (? unop? u) e) (UnOp u (parse e))]
    [(list (? binop? b) e1 e2) (BinOp b (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3) (If (parse e1) (parse e2) (parse e3))]
    [`(cond ,@clauses) ;added cond
     (Cond (map (lambda (clause)
                  (match clause
                    [`(else ,e) (list 'else (parse e))]
                    [(list p a) (list (parse p) (parse a))]))
                clauses))]
    [_ (error "Parse error!")]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero? not -))) ;added not and -

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and or %))) ; added or and %

