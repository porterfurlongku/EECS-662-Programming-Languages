#lang racket

(require "ast.rkt" "parser.rkt")

(provide tc)

(define (tc TE e)
  (match e
    [(Val v)         (tc-val v)]
    [(Var x)         (lookup TE x)]
    [(UnOp u e)      (tc-unop TE u e)]
    [(BinOp b e1 e2) (tc-binop TE b e1 e2)]
    [(If e1 e2 e3)   (tc-if TE e1 e2 e3)]
    [(Let x t e1 e2) (tc-let TE x t e1 e2)]
    [(Lam xts t e)   (tc-lam TE xts t e)]
    [(App e es)      (tc-app TE e es)]
    [(Seq es)        (tc-seq TE es)]
    [(New e)         (tc-new TE e)]
    [(Deref e)       (tc-deref TE e)]
    [(Set! e1 e2)    (tc-set! TE e1 e2)]))

(define (tc-val v)
  (match v
    [(? integer?) (T 'int)]
    [(? boolean?) (T 'bool)]
    [_            (error "Unexpected value")]))

(define (tc-unop TE u e)
  (match* (u (tc TE e))
    [('add1  (T 'int)) (T 'int)]
    [('sub1  (T 'int)) (T 'int)]
    [('zero? (T 'int)) (T 'bool)]
    [(_      _)        (error "Type error!")]))

(define (tc-binop TE b e1 e2)
  (match* (b (tc TE e1) (tc TE e2))
    [('+   (T 'int)  (T 'int))  (T 'int)]
    [('-   (T 'int)  (T 'int))  (T 'int)]
    [('*   (T 'int)  (T 'int))  (T 'int)]
    [('/   (T 'int)  (T 'int))  (T 'int)]
    [('<=  (T 'int)  (T 'int))  (T 'bool)]
    [('and (T 'bool) (T 'bool)) (T 'bool)]
    [(_    _         _)         (error "Type error!")]))

(define (tc-if TE e1 e2 e3)
  (match* ((tc TE e1) (tc TE e2) (tc TE e3))
    [((T 'bool) t2 t3) (union t2 t3)]
    [(_         _  _)  (error "Type error!")]))

(define (tc-let TE x t e1 e2)
  (if (equal? (tc TE e1) t)
      (tc (store TE x t) e2)
      (error "Type error!")))

(define (tc-lam TE xts t e)
  (if (equal? (tc (append xts TE) e) t)
      (FnT (map last xts) t)
      (error "Type error!")))

(define (tc-app TE e es)
  (match (tc TE e)
    [(FnT args ret) (if (equal? args (map (λ (e) (tc TE e)) es))
                        ret
                        (error "Type error!"))]
    [_              (error "Type error!")]))

(define (tc-seq TE es)
  (error "TODO"))

(define (tc-new TE e)
  (error "TODO"))

(define (tc-deref TE e)
  (error "TODO"))

(define (tc-set! TE e1 e2)
  (error "TODO"))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define (union t1 t2)
  (if (equal? t1 t2)
      t1
      (UnionT t1 t2)))

(define (store TE x t)
  (cons (list x t) TE))

(define (lookup TE x)
  (match TE
    ['()                      (raise (Err (string-append "Unbound identifier: "
                                                         (symbol->string x))))]
    [(cons (list y val) rest) (if (equal? x y) val (lookup rest x))]))

(module+ test
  (require rackunit)

  (check-equal? (tc '() (parse '(let ((x : (ref int) (new 5)))
                                  x))) (parse-type '(ref int))))
