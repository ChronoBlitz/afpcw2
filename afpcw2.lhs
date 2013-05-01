G52AFP Coursework 2 - Monadic Compiler
   
James Key
psyjpk@nottingham.ac.uk
Joe Staines
psyjs2@nottingham.ac.uk

---------------------------------------------------------------------------------

> import Control.Monad.State
> import Data.List


> data Prog				= Assign Name Expr
>					| If Expr Prog Prog
>					| While Expr Prog
>					| Seqn [Prog] deriving (Show)

> data Expr				= Val Int | Var Name | App Op Expr Expr deriving (Show)
> type Name				= Char
> data Op				= Add | Sub | Mul | Div deriving (Show, Eq)

> fac					:: Int -> Prog
> fac n				= Seqn [Assign 'A' (Val 1), Assign 'B' (Val n),
>					While (Var 'B') (Seqn
>						[Assign 'A' (App Mul (Var 'A') (Var 'B')),
>						Assign 'B' (App Sub (Var 'B') (Val 1))])]

> double				:: Int -> Prog
> double n				= Seqn [Assign 'A' (Val n), Assign 'A' (App Mul (Var 'A') (Val 2))]

> type Stack				= [Int]
> type Mem					= [(Name,Int)]

> type Code					= [Inst]
> data Inst					= PUSH Int
>					| PUSHV Name
>					| POP Name
>					| DO Op
>					| JUMP Label
>					| JUMPZ Label
>					| LABEL Label deriving (Show, Eq)
> type Label				= Int

-------------- START comp FUNCTION SECTION --------------

> type LabelState			= State Int

> label				:: LabelState Int -- Int -> (Inst, Int)
> label				= do	n <- get
>						put (n+1)
>						return $ n


> comp2				:: Prog -> LabelState Code 
> comp2 (Assign name expr)		= return ((exprCode expr) ++ [POP name])
> comp2 (If expr prog1 prog2)		= do	a <- label
>						b <- label
>						pr1 <- comp2 prog1
>						pr2 <- comp2 prog2
>						return ((exprCode expr) ++ [JUMPZ a] ++ pr1 
>							++ [JUMP b] ++ [LABEL a] ++ pr2 ++ 
>							[LABEL b])
> comp2 (While expr prog)		= do	a <- label
>						b <- label
>						pr1 <- comp2 prog
>						return ([LABEL a] ++ (exprCode expr) ++ [JUMPZ b] 
>							++ pr1 ++ [JUMP a] ++ [LABEL b])
> comp2 (Seqn progs)			= do	a <- seqnRec progs
>						return a

> seqnRec				:: [Prog] -> LabelState Code
> seqnRec []				= return []
> seqnRec (x:xs)			= do	pr1 <- comp2 x
>						prRec <- seqnRec xs
>						return (pr1 ++ prRec)

> exprCode				:: Expr -> [Inst]
> exprCode (Val n)			= [PUSH n]
> exprCode (Var n)			= [PUSHV n]
> exprCode (App op x1 x2)		= exprCode x1 ++ (exprCode x2 ++ [DO op])

> comp					:: Prog -> Code
> comp prog				= evalState (comp2(prog)) 0

----------- START exec FUNCTION SECTION ------------

> eval					:: Inst -> (Stack,Mem) -> (Stack,Mem)
> eval (PUSH int) ( xs, mem )		= (int : xs, mem)
> eval (PUSHV name) ( xs, mem )	= do	let val = lookup name mem
>						case val of
>							Nothing -> (xs,  mem)
>							Just x -> (x:xs, mem)
> eval (POP name) ( (x:xs), mem )	= do	let val = lookup name mem
>						case val of
>							Nothing -> (xs,(name, x) : mem)
>							Just y	-> (xs, (updateKey mem name x))
>
> eval (DO op) ( xs , mem )		= ((evalOp op xs), mem)
> eval _ smem				= smem

> updateKey				:: Mem -> Name -> Int -> Mem
> updateKey [] _ _			= []
> updateKey ((a,b):cs) k i 
>		| a == k		= (k,i):cs
>		| otherwise		= (a,b):updateKey cs k i

> stepRec				:: Code -> (Int, (Stack, Mem)) -> (Int, (Stack, Mem))
> stepRec c (int, stmem)
>		| length c == int	= (int, stmem)
>		| otherwise		= stepRec c (step c (int, stmem))

> step					:: Code -> (Int, (Stack, Mem)) -> (Int, (Stack, Mem))
> step c (i, (s, m))			= case c !! i of 
>		(JUMP int)		-> (search c int, (s, m)) -- you'd be looking for [LABEL i]
>		(JUMPZ int)		-> if ((head s) == 0) then (search c int, (s, m)) else (i+1, (s, m))
>		x			-> (i+1, eval x (s, m))		

> search				:: Code -> Int -> Int
> search (x:code) int 
>		| x == (LABEL int)	= 0 
>		| otherwise		= 1+ search code int

> evalOp				:: Op -> Stack -> Stack
> evalOp Add (x:y:xs)			= (y + x) : xs
> evalOp Sub (x:y:xs)			= (y - x) : xs
> evalOp Mul (x:y:xs)			= (y * x) : xs
> evalOp Div (x:y:xs)			= (y `div` x) : xs

> exec					:: Code -> Mem
> exec c				= snd(snd(stepRec c (0, ([],[]))))
