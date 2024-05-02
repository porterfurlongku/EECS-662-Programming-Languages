#lang racket

(require "ast.rkt" "parser.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (interp-prog e)))

;; interp :: Defn -> Env -> Expr -> Val
(define (interp D E e)
  (match e
    [(Val v)         v]
    [(Var x)         (lookup D E x)]
    [(UnOp u e)      (interp-unop D E u e)]
    [(BinOp b e1 e2) (interp-binop D E b e1 e2)]
    [(If e1 e2 e3)   (interp-if D E e1 e2 e3)]
    [(Let bindings body)   (interp D (store-bindings D E bindings) body)]
    [(Let* bindings body)  (interp D (store-bindings-sequential D E bindings) body)]
    [(Lam xs e)      (interp-lam D E xs e)]
    [(App e es)      (interp-app D E e es)]))

;; interp-lam :: Defn -> Env -> Vars -> Expr -> Val
(define (interp-lam D E xs body)
  (lambda (aargs)
    (if (= (length xs) (length aargs))
        (let ([extended-env (append (zip xs aargs) E)])
          (interp D extended-env body))
        (raise (Err "Arity mismatch")))))

;; interp-app :: Defn -> Env -> Expr -> Exprs -> Val
(define (interp-app D E f es)
    (let ([fn   (interp D E f)]
          [args (map (λ (arg)
                       (interp D E arg)) es)])
         (fn args)))

;; interp-prog :: Prog -> Val
(define (interp-prog prog)
  (match prog
    [(Prog D e) (interp D '() e)]))

;; interp-unop :: Defn -> Env -> UnOp -> Val
(define (interp-unop D E u e)
  (match u
    ['add1  (match (interp D E e)
              [(? integer? i) (add1 i)]
              [_              (raise (Err "add1 expects int"))])]
    ['sub1  (match (interp D E e)
              [(? integer? i) (sub1 i)]
              [_              (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp D E e)
              [0 #t]
              [_ #f])]))

;; interp-binop :: Defn -> Env -> BinOp -> Expr -> Expr -> Val
(define (interp-binop D E b e1 e2)
  (match b
    ['+ (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _)                             (raise (Err "+ requires int"))])]

    ['- (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _)                             (raise (Err "- requires int"))])]

    ['* (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _)                             (raise (Err "* requires int"))])]

    ['/ (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _)                             (raise (Err "/ requires int"))])]

    ['<= (match* ((interp D E e1) (interp D E e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _)                             (raise (Err "<= requires int"))])]

    ['and (match (interp D E e1)
            [#f #f]
            [?  (interp D E e2)])]))

;; interp-if :: Defn -> Env -> Expr -> Expr -> Expr -> Val
(define (interp-if D E e1 e2 e3)
  (match (interp D E e1)
    [#f (interp D E e3)]
    [_  (interp D E e2)]))

(define zip (lambda (l1 l2) (map list l1 l2)))

;; store :: Env -> Symbol -> Val -> Env
(define (store E x v)
  (cons (list x v) E))

;; lookup :: Defn -> Env -> Symbol -> Val
(define (lookup D E x)
  ; lookup the environment first, then the list of definitions
  (match E
    ['()                      (lookup-defn D D x)]
    [(cons (list y val) rest) (if (eq? x y) val
                                  (lookup D rest x))]))

;; lookup-defn :: Defn -> Defn -> Symbol -> Val
(define (lookup-defn D defns x)
  (match defns
    ['() (raise (Err (string-append "Unbound identifier: " (symbol->string x))))]
    [(cons (Defn f xs body) rest)
     (if (eq? f x)
         (λ (aargs)
           (if (= (length aargs) (length xs))
               (interp D (zip xs aargs) body)
               (raise (Err "Arity mismatch"))))
         (lookup-defn D rest x))]
    [(cons (DefnV y e) rest)
     (if (eq? x y)
         (interp D '() e)
         (lookup-defn D rest x))]))

;; Stores bindings sequentially for a 'Let*' expression.
;; Each binding is added to the environment before the next binding is evaluated.
(define (store-bindings-sequential D E bindings)
  (match bindings
    ['() E]  ;; Base case: no more bindings, return the environment.
    [(cons head tail)
     (store-bindings-sequential D
                                (cons (list (get-variable-name (car head)) 
                                            (interp D E (cdr head)))
                                      E)
                                tail)]))

;; Extracts the variable name from a Var structure.
(define (get-variable-name v)
  (match v
    [`#s(Var ,x) x]
    [`(#s(Var ,x)) x]))

;; Checks if a variable is already defined in the environment.
(define (is-defined? D E x)
  (match E
    ['() #f]  ;; Base case: empty environment, variable not defined.
    [(cons (list y val) rest) (if (eq? x y) #t (is-defined? D rest x))]))

;; Stores bindings for a 'Let' expression.
;; All bindings are stored in the environment before evaluating the body.
(define (store-bindings D E bindings)
  (match bindings
    ['() E]  ;; Base case: no more bindings, return the environment.
    [bindings (append (store-new-bindings D E '() bindings) E)]))

;; Stores new bindings, ensuring no duplicates.
(define (store-new-bindings D E-old E-new bindings)
  (match bindings
    ['() E-new]  ;; Base case: no more bindings, return the new environment.
    [(cons head tail) 
     (if (is-defined? D E-new (get-variable-name (car head)))
         (raise (Err (string-append "let: duplicate identifier in: " (symbol->string (get-variable-name (car head))))))
         (store-new-bindings D E-old 
                             (cons (list (get-variable-name (car head)) 
                                         (interp D E-old (cdr head)))
                                   E-new) 
                             tail))]))