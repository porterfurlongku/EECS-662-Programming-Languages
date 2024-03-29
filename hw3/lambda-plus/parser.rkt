#lang racket

(require "ast.rkt")

(provide parse-prog)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)              (Val s)]
    [(? boolean?)              (Val s)]
    [(? symbol?)               (Var s)]
    [(list (? unop? u) e)      (UnOp u (parse e))]
    [(list (? binop? b) e1 e2) (BinOp b (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3)         (If (parse e1) (parse e2) (parse e3))]
    [`(let (,@bindings) ,body) (let ([parsed-bindings (map (lambda (b) (list (car b) (parse (cadr b)))) bindings)])
                                  (if (has-duplicates? (map car parsed-bindings))
                                      (error "Parse error: Duplicate identifier in let bindings")
                                      (Let parsed-bindings (parse body))))]
    [`(lambda (,@xs) ,e)       (Lam xs (parse e))]
    [`(λ (,@xs) ,e)            (Lam xs (parse e))]
    [(cons e es)               (App (parse e) (map parse es))]
    [_                         (error "Parse error!")]))

;; S-Expr -> Defns
(define (parse-defn s)
  (match s
    [`(define (,(? symbol? f) ,@xs) ,e) (Defn f xs (parse e))]
    [`(define ,(? symbol? x) ,e)        (DefnV x (parse e))]
    [_ (error "Parse error in definition!")]))

;; List S-Expr -> Prog
(define (parse-prog s)
  (match s
    [(cons e '())     (Prog '() (parse e))]
    [(cons defn rest) (match (parse-prog rest)
                             [(Prog d e) (Prog (cons (parse-defn defn) d) e)])]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero?)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and)))

;; Check for duplicates in a list
(define (has-duplicates? lst)
  (not (= (length lst) (length (remove-duplicates lst)))))
