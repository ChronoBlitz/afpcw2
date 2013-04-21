G52AFP Coursework 2 - Monadic Compiler
   
James Key
psyjpk@nottingham.ac.uk
Joe Staines
psyjs2@nottingham.ac.uk

----------------------------------------------------------------------

Imperative language
Consider an imperative language in which a program is either an assignment,
a conditional, a while loop, or a sequence of programs:

> 	data Prog = Assign Name Expr
>		| If Expr Prog Prog
>		| While Expr Prog
>		| Seqn [Prog]

In turn, an expression is either an integer value, a variable name, or the
application of an operator to two argument expressions:

>	data Expr = Val Int | Var Name | App Op Expr Expr
>	type Name = Char
>	data Op = Add | Sub | Mul | Div

Note that the logical value False is represented by the integer zero, and True
is represented by any other integer. For example, a program that computes
the factorial of a non-negative integer n can be defined as follows:

>	fac :: Int -> Prog
>	fac n = Seqn [Assign 'A' (Val 1);
>	Assign 'B' (Val n);
>	While (Var 'B') (Seqn
>	[Assign 'A' (App Mul (Var 'A') (Var 'B'));
>	Assign 'B' (App Sub (Var 'B') (Val 1))])]

Virtual machine
Now consider a virtual machine that operates using a stack of integers, and a
memory that maps variable names to their current integer values:

> type Stack = [Int ]
> type Mem = [(Name;Int)]

Code for the machine comprises a list of instructions, each of which either
pushes an integer onto the stack, pushes the value of a variable, pops the top
of the stack into a variable, performs an operation on the stack, jumps to a
label, pops the stack and jumps if this value is zero, or is simply a label:

> 	type Code = [Inst ]
> 	data Inst = PUSH Int
>		| PUSHV Name
>		| POP Name
>		| DO Op
>		| JUMP Label
>		| JUMPZ Label
>		| LABEL Label
> 	type Label = Int

Exercise
Define a function comp :: Prog -> Code that translates a program into ma-
chine code, using a state monad to handle the generation of fresh labels. For
example, the result of comp (fac 10) should be as follows:
[PUSH 1;POP 'A';
PUSH 10;POP 'B';
LABEL 0;
PUSHV 'B';JUMPZ 1;
PUSHV 'A';PUSHV 'B'; DO Mul;POP 'A';
PUSHV 'B';PUSH 1; DO Sub;POP 'B';
JUMP 0;
LABEL 1]

>	comp :: Prog -> Code

Exercise
Define a function exec :: Code -> Mem that executes code produced by your
compiler, returning the final contents of the memory. For example, the result
of exec (comp (fac 10)) should be as follows:
[('A';3628800);('B';0)]

>	exec :: Code -> Mem

Bonus
Define a function that attempts to optimise programs to execute in fewer
steps. Hint: think about program-level vs code-level transformation, and/or
extending the machine with extra instructions.
Revise your compiler to use the writer monad to handle the production of
the resulting code. Hint: you will need to use the writer monad transformer,
supplied with the state monad as an argument. (More difficult)