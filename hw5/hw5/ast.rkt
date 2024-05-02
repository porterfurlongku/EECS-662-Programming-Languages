#lang racket

(provide Val UnOp BinOp If Err Err?
         Let Var App Lam Seq New Deref Set!
         T UnionT ParamT FnT)

; type Values :=
;   | (Val v)
;   | (Lam xs e)
(struct Val   (v)        #:prefab)
(struct Lam   (xts t e)     #:prefab)

; type Expr :=
;   | Values
;   | (Var   x)
;   | (UnOp  u e)
;   | (BinOp u e)
;   | (If    e e e)
;   | (Let   x e e)
;   | (App   e e)
;   | (Seq   es)
;   | (New   e)
;   | (Deref e)
;   | (Set e1 e2)
(struct Var   (x)         #:prefab)
(struct UnOp  (u e)       #:prefab)
(struct BinOp (b e1 e2)   #:prefab)
(struct If    (e1 e2 e3)  #:prefab)
(struct Let   (x t e1 e2) #:prefab)
(struct App   (x args)    #:prefab)
(struct Seq   (es)        #:prefab)
(struct New   (e)         #:prefab)
(struct Deref (e)         #:prefab)
(struct Set!  (e1 e2)     #:prefab)

(struct T (t)          #:prefab) ; int | bool
(struct UnionT (t1 t2) #:prefab) ; T U T
(struct ParamT (b p)   #:prefab) ; B<P>
(struct FnT (args ret) #:prefab) ; T -> T

(struct Err   (err)      #:prefab)
