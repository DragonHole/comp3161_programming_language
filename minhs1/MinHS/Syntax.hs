module MinHS.Syntax where

import Data.List

type Id = String

type Program = [Bind]

data Exp
    = Var Id  -- variable is defined using an identifier(string)
    | Prim Op -- primitive operations
    | Con Id  -- constructor is defined using an identifier(string)

    | Num Integer

    | App Exp Exp
    | If Exp Exp Exp
    | Let [Bind] Exp
    | Recfun Bind
    | Letrec [Bind] Exp
    deriving (Read,Show,Eq)

-- each Bind binds a var of type `Type` to an identifier`Id`(string), to the expression `exp`
-- but for functions we ignore the Type?

data Bind = Bind Id Type [Id] Exp
  deriving (Read,Show,Eq)

data Op = Add
        | Sub
        | Mul
        | Quot
        | Rem  -- remainder
        | Neg  -- negate
        | Gt
        | Ge
        | Lt
        | Le
        | Eq
        | Ne
        | Head
        | Tail
        | Null
        deriving (Show, Eq, Read)

data Type = Arrow Type Type
          | TypeApp Type Type
          | TypeCon TyCon
          deriving (Read, Show, Eq, Ord)

-- stands for `type constructors`
data TyCon = Unit
           | Bool
           | Int
           | List
           deriving (Read, Show, Eq, Ord)

-- for add, minus etc. 
binApply :: Exp -> Exp -> Exp -> Exp
binApply e1 e2 e3 = App (App e1 e2) e3

binTyApp :: Type -> Type -> Type -> Type
binTyApp t1 t2 t3 = TypeApp (TypeApp t1 t2) t3
