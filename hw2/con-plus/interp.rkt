#lang racket

(require rackunit "ast.rkt" "parser.rkt")

(provide interp)

;; interp :: Expr -> Int
(define (interp e)
  (match e
    [(Val v) v]
    [(UnOp u e) (interp-unop u e)]
    [(BinOp b e1 e2) (interp-binop b e1 e2)]
    [(Cond clauses) (interp-cond clauses)]
    [(If e1 e2 e3) (interp-if e1 e2 e3)]))

(define (interp-unop u e)
  (match u
    ['add1 (add1 (interp e))]
    ['sub1 (sub1 (interp e))]
    ['zero? (match (interp e)
              [0 #t]
              [_ #f])]
    ['not (match (interp e) ;not
             [#f #t]
             [_ #f])]
    ['- (- 0 (interp e))])) ; - 

(define (interp-binop b e1 e2)
  (match b
    ['+ (+ (interp e1) (interp e2))]
    ['- (- (interp e1) (interp e2))]
    ['* (* (interp e1) (interp e2))]
    ['/ (quotient (interp e1) (interp e2))]
    ['<= (<= (interp e1) (interp e2))]
    ['and (match (interp e1)
            [#f #f]
            [? (interp e2)])]
    ['or (match (interp e1) ; or
           [#f (interp e2)]
           [_ (interp e1)])]
    ['% (remainder (interp e1) (interp e2))])) ; %

(define (interp-if e1 e2 e3)
  (match (interp e1)
    [#f (interp e3)]
    [_  (interp e2)]))

(define (interp-cond clauses) ;cond clauses 
  (match clauses
    [(list)
     (error "No else clause in cond")]
    [(cons (list 'else e) _)
     (interp e)]
    [(cons (list p a) rest)
     (if (interp p) (interp a) (interp-cond rest))]
    [else
     (error "Invalid cond clause")]))



(module+ test
  (check-eqv? (interp (parse '(+ 42 (sub1 34)))) 75)
  (check-eqv? (interp (parse '(zero? (- 5 (sub1 6))))) #t)
  (check-eqv? (interp (parse '(if (zero? 0) (add1 5) (sub1 5)))) 6)
  (check-eqv? (interp (parse '(or #f 42))) 42)
  (check-eqv? (interp (parse '(- 5))) -5) ;test -
  (check-eqv? (interp (parse '(not #t))) #f) ; test not
  (check-eqv? (interp (parse '(% 10 3))) 1); test %
  (check-eqv? (interp (parse '(cond [(zero? (- 6 5)) 1] ;example from hw2 instructions
                                    [(<= 6 7)        2]
                                    [else            3]))) 2)
  (check-eqv? (interp (parse '(cond [(zero? (- 5 5)) 1]
                                    [(<= 7 6)        2]
                                    [else            3]))) 1)
  (check-eqv? (interp (parse '(cond [(zero? (- 6 6)) 1]
                                    [else            3]))) 1)
  (check-eqv? (interp (parse '(cond [(zero? (- 7 6)) 1]
                                    [else            3]))) 3))
