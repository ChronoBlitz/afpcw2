> data Prog 		= Assign Name Expr
>					| If Expr Prog Prog
>					| While Expr Prog
>					| Seqn [Prog] deriving (Show)

> data Expr 		= Val Int | Var Name | App Op Expr Expr deriving (Show)
> type Name 		= Char
> data Op 			= Add | Sub | Mul | Div deriving (Show)

> fac 				:: Int -> Prog
> fac n 			= Seqn [Assign 'A' (Val 1), Assign 'B' (Val n),
>					While (Var 'B') (Seqn
>						[Assign 'A' (App Mul (Var 'A') (Var 'B')),
>						Assign 'B' (App Sub (Var 'B') (Val 1))])]

Virtual Machine stuffzzz

> type Stack 		= [Int]
> type Mem 			= [(Name,Int)]

> type Code 		= [Inst]
> data Inst 		= PUSH Int
>					| PUSHV Name
>					| POP Name
>					| DO Op
>					| JUMP Label
>					| JUMPZ Label
>					| LABEL Label deriving (Show)
> type Label 		= Int

--> comp 				:: Prog -> Code 
--> comp (Assign name expr) = [PUSH expr, POP name]
--> comp (If expr prog1 prog2) = 
--> comp (While expr prog) =
--> comp (Seqn progs) = 

> exprCode :: Expr -> [Inst]
> exprCode (Val n) = [PUSH n]
> exprCode (Var n) = [PUSHV n]
> exprCode (App op x1 x2) = exprCode x1 ++ (exprCode x2 ++ [DO op])

