module Semantics where

{- HOAS representation -}
--从syntax.hs copy

data HOASExp =
  NumHOAS Int
  | TimesHOAS HOASExp HOASExp
  | PlusHOAS HOASExp HOASExp
  | LetHOAS HOASExp (HOASExp -> HOASExp) -- a function that takes in Exp and outputs Exp
    -- we use haskell lambda expression to represent HOAS abstraction(sort of substitution)

--  | FreeHOAS String -- only used for convenience when converting from HOAS.
{-  deriving (Eq,Show)  doesn't work
    - Functions aren't considered comparable using == in Haskell.
    - Functions aren't considered pretty-printable either
 -}

example = LetHOAS (NumHOAS 3) (\x -> TimesHOAS x (NumHOAS 3))
-- x=3, return x*3 = 9

type Value = Int
bigStep :: HOASExp -> Value

-- 按照semantics lecture的rules写
bigStep (NumHOAS i) = i
bigStep (PlusHOAS e1 e2) = 
    let v1 = bigStep e1
        v2 = bigStep e2
    in (v1+v2)
bigStep (TimesHOAS e1 e2) = 
    let v1 = bigStep e1
        v2 = bigStep e2
    in (v1*v2)
bigStep (LetHOAS e1 f) = 
    let v1 = bigStep e1
        v2 = bigStep (f (NumHOAS v1))
    in v2


smallStep :: HOASExp -> HOASExp
-- base case first
smallStep (PlusHOAS (NumHOAS m) (NumHOAS n)) = NumHOAS (m+n)
-- inductive cases
smallStep (PlusHOAS (NumHOAS m) e2) = PlusHOAS (NumHOAS m) (smallStep e2) -- left(e1) has been evaluated
-- smallStep (PlusHOAS e1 e2) = PlusHOAS (smallStep e1) e2
smallStep (PlusHOAS e1 e2) = 
    let e1' = smallStep e1 in
        PlusHOAS e1' e2

-- now for times 
smallStep (TimesHOAS (NumHOAS m) (NumHOAS n)) = NumHOAS (m*n)
-- inductive cases
smallStep (TimesHOAS (NumHOAS m) e2) = TimesHOAS (NumHOAS m) (smallStep e2) -- left(e1) has been evaluated
-- smallStep (TimesHOAS e1 e2) = TimesHOAS (smallStep e1) e2
smallStep (TimesHOAS e1 e2) = 
    let e1' = smallStep e1 in
        TimesHOAS e1' e2

-- now for let
-- base case first (last one in slide)
smallStep (LetHOAS (NumHOAS n) f) = f (NumHOAS n)
smallStep (LetHOAS e1 f) =
    let e1' = smallStep e1 in
        f e1'

-- driver function for the small-step semantics
driver :: HOASExp -> Value
driver (NumHOAS n) = n
driver e = driver (smallStep e)