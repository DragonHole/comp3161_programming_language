-- from lecture, not for the assignment1

module MinHS where

data Type = IntType | BoolType | FuncType Type Type
   deriving (Eq, Show)

data Expr = Num Int 
          | BoolLit Bool
          | Plus Expr Expr
          | Minus Expr Expr
          | Divide Expr Expr
          | Less Expr Expr
          | Equal Expr Expr
          | If Expr Expr Expr
          | Apply Expr Expr --FuncType Expr? ('Apply Func Val')
       -- | Recfun Type Type String String Expr
          | Recfun Type Type (Expr->Expr->Expr) -- arg1: input type, arg2: output type
                        -- expr1: input1, substitute for func body, expr2: input2, substitute for argument name, expr3: output expression 
       -- | Var String -- cuz we're in HOAS now, just use meta language's(haskell) var 
          | Tag String -- for identifying function names and variable name (internal use)
          | TypeTag Type


------------- PRETTY PRINTER --------------
-- printing individual elements 
prettyPrintTy :: Type -> String
-- print the types (see it as the base cases)
prettyPrintTy IntType = "Int"
prettyPrintTy BoolType = "Bool"
prettyPrintTy (FuncType a b) = "("++prettyPrintTy a ++ " -> " ++ prettyPrintTy b ++ ")"

-- print the expressions 
-- first arg: the number of variables i have introduced so far, for creating unique var name
prettyPrint :: Int -> Expr -> String
prettyPrint n (Num a) = show a
prettyPrint n (BoolLit b) = show b
prettyPrint n (Plus e1 e2) = "(" ++ prettyPrint n e1 ++ "+" ++ prettyPrint n e2 ++ ")"
prettyPrint n (Minus e1 e2) = "(" ++ prettyPrint n e1 ++ "-" ++ prettyPrint n e2 ++ ")"
prettyPrint n (Divide e1 e2) = "(" ++ prettyPrint n e1 ++ "/" ++ prettyPrint n e2 ++ ")"
prettyPrint n (Less e1 e2) = "(" ++ prettyPrint n e1 ++ "<" ++ prettyPrint n e2 ++ ")"
prettyPrint n (Equal e1 e2) = "(" ++ prettyPrint n e1 ++ "==" ++ prettyPrint n e2 ++ ")"
prettyPrint n (If e1 e2 e3) = 
   "(if " ++ prettyPrint n e1 ++ 
   " then " ++ prettyPrint n e2 ++ 
   " else " ++ prettyPrint n e3 ++ 
   ")"
prettyPrint n (Apply e1 e2) = "(" ++ prettyPrint n e1 ++ " " ++ prettyPrint n e2 ++ ")"
prettyPrint n (Recfun t1 t2 f) = "(recfun " ++ 
                                 funcName ++ 
                                 " :: (" ++ 
                                 prettyPrintTy t1 ++ 
                                 " -> " ++ 
                                 prettyPrintTy t2 ++ ") " ++ 
                                 argName ++
                                 " = " ++
                                 prettyPrint (n+2) (f (Tag funcName) (Tag argName)) ++
                                 ")"
   where 
      funcName = "f" ++ show n
      argName = "arg" ++ show (n+1) -- could be n+1 to make it clearer? maybe
prettyPrint n (Tag a) = a 

-- Examples 
divByFive :: Expr
divByFive = Recfun IntType IntType (\f x -> If (Less x (Num 5)) (Num 0) (Plus (Num 1) (Apply f (Minus x (Num 5)))))

avg :: Expr
avg = Recfun IntType (FuncType IntType IntType) (\f x ->
      Recfun IntType IntType (\f' y ->
         Divide (Plus x y) (Num 2)))

-- can do something like...
-- prettyPrintTy $ typeCheck avg


---------------------- TYPE CHECKER -----------------------
typeCheck :: Expr -> Type -- outputs the type the expression should have
typeCheck (Num a) = IntType
typeCheck (BoolLit a) = BoolType
typeCheck (TypeTag a) = a
typeCheck (Plus e1 e2) = case (typeCheck e1, typeCheck e2) of
   (IntType, IntType) -> IntType
   _ -> error "Type error for Plus expression"
typeCheck (Minus e1 e2) = case (typeCheck e1, typeCheck e2) of
   (IntType, IntType) -> IntType
   _ -> error "Type error for Minus expression"
typeCheck (Divide e1 e2) = case (typeCheck e1, typeCheck e2) of
   (IntType, IntType) -> IntType
   _ -> error "Type error for Divide expression"
typeCheck (Less e1 e2) = case (typeCheck e1, typeCheck e2) of
   (IntType, IntType) -> BoolType
   _ -> error "Type error for Less expression"
typeCheck (Equal e1 e2) = case (typeCheck e1, typeCheck e2) of
   (IntType, IntType) -> BoolType
   (BoolType, BoolType) -> BoolType
   _ -> error "Type error for Equal expression"
typeCheck (If c t e) = case (typeCheck c, typeCheck t, typeCheck e) of
   (BoolType, tt, te) -> if tt == te then tt else error "If branches have different types!"
   _ -> error "If condition should be BoolType"
typeCheck (Apply e1 e2) = case (typeCheck e1, typeCheck e2) of
   (FuncType t1 t2, at) -> if at == t1 then t2 else error "arg type should equal function input type"
typeCheck (Recfun t1 t2 f) = if typeCheck (f (TypeTag (FuncType t1 t2)) (TypeTag t1)) == t2 then
   FuncType t1 t2 else error "Function body doesn't match type signature"






--------------------------C MACHINE--ABSTRACT MACHINE----------------
data Value = IntV Int | BoolV Bool | FuncV (Expr -> Expr -> Expr)

-- each frame contains everything EXCEPT the square hole 
data Frame = PlusF_e2 Expr | PlusF_v1 Value
           | MinusF_e2 Expr | MinusF_v1 Value
           | LessF_e2 Expr | LessF_v1 Value
           | EqualF_e2 Expr | EqualF_v1 Value
           | DivideF_e2 Expr | DivideF_v1 Value
           | IfF Expr Expr 
           | ApplyF_e2 Expr | ApplyF_fxe Value -- this 'Value' is a function

type Stack = [Frame]

-- Operator overloading
data State = (:>) Stack Expr     -- Evaluate state
           | (:<) Stack Value    -- Return state 

-- C-Machine state transitions
step :: State -> State

step (s :> Num n) = s :< IntV n

step (s :> Plus e1 e2) = (PlusF_e2 e2:s) :> e1
step ((PlusF_e2 e2:s) :< v1) = (PlusF_v1 v1:s) :> e2
step ((PlusF_v1 (IntV v1):s) :< (IntV v2)) = s :< IntV (v1+v2)

step (s :> Minus e1 e2) = (MinusF_e2 e2:s) :> e1
step ((MinusF_e2 e2:s) :< v1) = (MinusF_v1 v1:s) :> e2
step ((MinusF_v1 (IntV v1):s) :< (IntV v2)) = s :< IntV (v1-v2)

step (s :> Divide e1 e2) = (DivideF_e2 e2:s) :> e1
step ((DivideF_e2 e2:s) :< v1) = (DivideF_v1 v1:s) :> e2
step ((DivideF_v1 (IntV v1):s) :< (IntV v2)) = s :< IntV (v1 `div` v2)

step (s :> Less e1 e2) = (LessF_e2 e2:s) :> e1
step ((LessF_e2 e2:s) :< v1) = (LessF_v1 v1:s) :> e2
step ((LessF_v1 (IntV v1):s) :< (IntV v2)) = s :< BoolV (v1 < v2)

step (s :> Equal e1 e2) = (EqualF_e2 e2:s) :> e1
step ((EqualF_e2 e2:s) :< v1) = (EqualF_v1 v1:s) :> e2
step ((EqualF_v1 (IntV v1):s) :< (IntV v2)) = s :< BoolV (v1 == v2)
step ((EqualF_v1 (BoolV v1):s) :< (BoolV v2)) = s :< BoolV (v1 == v2)

step (s :> If ec et ee) = (IfF et ee :s) :> ec
step ((IfF et ee :s) :< BoolV True) = s :> et
step ((IfF et ee :s) :< BoolV False) = s :> ee

-- ignored t1 and t2
step (s :> Recfun t1 t2 abs) = s :< (FuncV abs)

step (s :> Apply e1 e2) = ((ApplyF_e2 e2):s) :> e1
step ((ApplyF_e2 e2:s) :< f) = ((ApplyF_fxe f):s) :> e2
step ((ApplyF_fxe (FuncV f):s) :< v) = s :> f (uneval (FuncV f)) (uneval v)

-- helper
uneval :: Value -> Expr
uneval (IntV n) = Num n
uneval (BoolV b) = BoolLit b
uneval (FuncV f) = Recfun undefined undefined f 
-- the t1 and t2 args are part of the static semantics, it is ignored here is dynamic semantics

-- hack for printing value
prettyValue :: Value -> String
prettyValue = prettyPrint 0 . uneval

-- example: prettyValue (run (Apply divByFive (Num17)))

run :: Expr -> Value
run e = let 
   initialState = [] :> e
   loop :: State -> Value
   loop ([] :< v) = v
   -- loop s = loop (step s) 
   loop s = let 
      s1 = step s in
         loop s1
   in
      loop initialState