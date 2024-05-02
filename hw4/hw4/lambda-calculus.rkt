#lang racket

;; Provides essential functions for parsing, unparsing, and transforming lambda calculus expressions.
(provide parse unparse free? alpha-reduce beta-reduce)

;; Definitions of data structures using prefab structs for variable, lambda, and application expressions.
(struct Var (x) #:prefab)  ; Represents a variable in lambda calculus.
(struct Lam (x e) #:prefab)  ; Represents a lambda function with a parameter x and body e.
(struct App (f arg) #:prefab)  ; Represents a function application with function f and argument arg.

;; Parses a symbolic expression into its corresponding lambda calculus data structure.
(define (parse s)
  (match s
    [(? symbol?) (Var s)]  ; If s is a symbol, return a variable structure.
    [`(λ (,x) ,e) (Lam x (parse e))]  ; Handle standard lambda notation.
    [`(lambda (,x) ,e) (Lam x (parse e))]  ; Handle alternate lambda notation.
    [(list f arg) (App (parse f) (parse arg))]))  ; Handle function application.

;; Converts lambda calculus expressions back into readable symbolic expressions.
(define (unparse e)
  (match e
    [(Var x) x]  ; Variable x is returned as is.
    [(Lam x e) (list 'λ (list x) (unparse e))]  ; Lambda expression is formatted with λ.
    [(App f arg) (list (unparse f) (unparse arg))]))  ; Application is formatted as a list.

;; Determines if an identifier is free in a given expression.
(define (free? bound id e)
   (match e
    [(Var x) (and (eq? x id) (not (member x bound)))]  ; Check if x is the identifier and not bound.
    [(Lam x body) (free? (cons x bound) id body)]  ; Extend the bound context and check the body.
    [(App f arg) (or (free? bound id f) (free? bound id arg))]  ; Check both function and argument.
    [_ #f]))  ; For any other form, return false.

;; Alpha-reduces an expression by renaming a variable.
(define (alpha-reduce M x z)
(match M
    [(Var v) (if (eq? v x) (Var z) M)]  ; Rename the variable if it matches x.
    [(Lam v body) (if (eq? v x) (Lam v body) (Lam v (alpha-reduce body x z)))]  ; Recurse into the body unless v is x.
    [(App f arg) (App (alpha-reduce f x z) (alpha-reduce arg x z))]  ; Apply alpha-reduction to both function and argument.
    [_ M]))  ; Return as is for any other forms.

;; Computes the set of free variables in an expression.
(define (free-variables exp bound)
  (match exp
    [(Var x) (if (member x bound) '() (list x))]  ; If x is bound, return empty list; otherwise, list x.
    [(Lam x body) (free-variables body (cons x bound))]  ; Extend bound and recurse.
    [(App f arg) (union (free-variables f bound) (free-variables arg bound))]  ; Union of free variables from function and argument.
    [_ '()]))  ; Return empty list for other forms.

;; Helper function to union two lists without duplicates.
(define (union a b)
  (remove-duplicates (append a b)))

;; Beta-reduces an expression by substituting a variable with another expression.
(define (beta-reduce M x N)
  (match M
    [(Var v) (if (eq? v x) N M)]  ; Substitute variable v with N if it matches x.
    [(Lam v body)
     (if (eq? v x) 
         (Lam v body)  ; Do nothing if v is the variable being substituted.
         (let* ((free-in-N (free-variables N '()))
                (fresh-v (if (member v free-in-N) (gensym) v)))  ; Generate a fresh variable if necessary.
           (Lam fresh-v (beta-reduce (alpha-reduce body v fresh-v) x (alpha-reduce N x fresh-v)))))]  ; Perform beta-reduction with alpha-reduction.
    [(App f arg)
     (App (beta-reduce f x N) (beta-reduce arg x N))]  ; Apply beta-reduction recursively to function and argument.
    [_ M]))  ; Return M as is for any other forms.
