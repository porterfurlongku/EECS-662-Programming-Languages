#lang racket

(require "ast.rkt" "parser.rkt" "type.rkt")

(provide interp-err)

(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (begin
      (tc '() e)
      (interp '() (make-hash) e))))

(define (interp E S e)
  (match e
    [(Val v)          v]
    [(Var x)          (lookup E x)]
    [(UnOp u e)       (interp-unop E S u e)]
    [(BinOp b e1 e2)  (interp-binop E S b e1 e2)]
    [(If e1 e2 e3)    (interp-if E S e1 e2 e3)]
    [(Let x t e1 e2)  (interp (store E x (interp E S e1)) S e2)]
    [(Lam xts t e)    (interp-lam E S (map first xts) e)]
    [(App e es)       (interp-app E S e es)]
    [(Seq es)         (last (map (λ (e) (interp E S e)) es))]
    [(New e)          (interp-new E S e)]
    [(Deref e)        (interp-deref E S e)]
    [(Set! e1 e2)     (interp-set! E S e1 e2)]))

(define (interp-new E S e)
  (let ([loc (gensym)]
        [v   (interp E S e)])
    (begin
      (hash-set! S loc v)
      loc)))

(define (interp-deref E S e)
  (let ([loc (interp E S e)])
    (hash-ref S loc)))

(define (interp-set! E S e1 e2)
  (let ([loc (interp E S e1)]
        [v   (interp E S e2)])
    (begin
      (hash-set! S loc v)
      v)))

(define (interp-lam E S xs body)
  (λ (aargs)
    (interp (append (zip xs aargs) E) S body)))

(define (interp-app E S f es)
    (let ([fn   (interp E S f)]
          [args (map (λ (arg) (interp E S arg)) es)])
         (fn args)))

(define (interp-unop E S u e)
  (match u
    ['add1  (match (interp E S e)
              [(? integer? i) (add1 i)]
              [_              (raise (Err "add1 expects int"))])]
    ['sub1  (match (interp E S e)
              [(? integer? i) (sub1 i)]
              [_              (raise (Err "sub1 expects int"))])]
    ['zero? (match (interp E S e)
              [0 #t]
              [_ #f])]))

(define (interp-binop E S b e1 e2)
  (match b
    ['+ (match* ((interp E S e1) (interp E S e2))
          [((? integer? i1) (? integer? i2)) (+ i1 i2)]
          [(_ _)                             (raise (Err "+ requires int"))])]

    ['- (match* ((interp E S e1) (interp E S e2))
          [((? integer? i1) (? integer? i2)) (- i1 i2)]
          [(_ _)                             (raise (Err "- requires int"))])]

    ['* (match* ((interp E S e1) (interp E S e2))
          [((? integer? i1) (? integer? i2)) (* i1 i2)]
          [(_ _)                             (raise (Err "* requires int"))])]

    ['/ (match* ((interp E S e1) (interp E S e2))
          [((? integer? i1) (? integer? i2)) (if (eq? i2 0)
                                                 (raise (Err "division by 0 not allowed"))
                                                 (quotient i1 i2))]
          [(_ _)                             (raise (Err "/ requires int"))])]

    ['<= (match* ((interp E S e1) (interp E S e2))
          [((? integer? i1) (? integer? i2)) (<= i1 i2)]
          [(_ _)                             (raise (Err "<= requires int"))])]

    ['and (match (interp E S e1)
            [#f #f]
            [?  (interp E S e2)])]))

(define (interp-if E S e1 e2 e3)
  (match (interp E S e1)
    [#f (interp E S e3)]
    [_  (interp E S e2)]))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define (store E x v)
  (cons (list x v) E))

(define (lookup E x)
  (match E
    ['()                      (raise (Err (string-append "Unbound identifier: "
                                                         (symbol->string x))))]
    [(cons (list y val) rest) (if (eq? x y) val (lookup rest x))]))
