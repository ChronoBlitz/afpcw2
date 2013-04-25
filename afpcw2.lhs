> data Prog = Assign Name Expr
>		| If Expr Prog Prog
>		| While Expr Prog
>		| Seqn [Prog] deriving (Show)

> data Expr = Val Int | Var Name | App Op Expr Expr deriving (Show)
> type Name = Char
> data Op = Add | Sub | Mul | Div deriving (Show)

> fac :: Int -> Prog
> fac n = Seqn [Assign 'A' (Val 1), Assign 'B' (Val n),
>		While (Var 'B') (Seqn
>		[Assign 'A' (App Mul (Var 'A') (Var 'B')),
>		Assign 'B' (App Sub (Var 'B') (Val 1))])]

Virtual Machine stuffzzz

> type Stack = [Int]
> type Mem = [(Name,Int)]

> type Code = [Inst]
> data Inst = PUSH Int
>			| PUSHV Name
>			| POP Name
>			| DO Op
>			| JUMP Label
>			| JUMPZ Label
>			| LABEL Label
> type Label = Int

> comp :: Prog -> Code
> comp (Assign name expr) = [PUSH expr, POP name]
> comp (If expr prog1 prog2) = [
> comp (While expr prog) =
> comp (Seqn progs) =
