import Data.List(elemIndex)
import Data.Char(isSpace,isDigit)

{-
  The inference rules go directly from strings to parse trees.
  We'll instead go from token lists to parse trees.

  To go all the way, we just have to: parser . lexer
   (. is Haskell function composition)
 -}

-- Token means Token
-----------------------------THE LEXER-----------------------------------
data Token = 
  LParen
  | RParen
  | IntLit Int
  | TimesSign
  | PlusSign
  deriving (Eq,Show)
{- deriving Show: "make Token pretty-printable"
   deriving Eq:   "make Token comparable with the == (equality) operator"
 -}

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = let
                (digits,rest) = span isDigit (c:cs)
                in IntLit (read digits) : lexer rest
             | otherwise =
                (case c of
                    '(' -> LParen
                    ')' -> RParen
                    '*' -> TimesSign
                    '+' -> PlusSign
                    _ -> error "Unrecognised character"
                ) : lexer cs




-----------------------------THE PARSER-----------------------------------
-- types of expression 
data ArithExp =  NumE Int
            | TimesE ArithExp ArithExp
            | PlusE ArithExp ArithExp
            deriving (Eq,Show)

{- parse ts will:
    - parse an ArithExpmetic expression from a *prefix* of ts
    - also return the leftovers
 -}
-- THE BELOW RULES COME FROM SYNTAX LECTURE
parseAtom :: [Token] -> (ArithExp,[Token])
parsePExp :: [Token] -> (ArithExp,[Token])
parseSExp :: [Token] -> (ArithExp,[Token])
{- The following clause implements the inference rule
           i Int
    ______________________
   i Atom <--> (Num i) AST
 -}
-- parseAtom 的目的是产出Atom，不然的话就报错
parseAtom (IntLit i:rest) = (NumE i, rest)
parseAtom (LParen:rest) = case parseSExp rest of
    (e, RParen:rest') -> (e, rest')
    _ -> error "atom construction: expected close paranthesis"
parseAtom _ = error("Invalid atom construction")
{- Observation: both inference rules for PExp require us to find
   an Atom at the left.
 -}
parsePExp tokens = case parseAtom tokens of
    (a, TimesSign:rest) -> case parsePExp rest of
        (b, rest') -> (TimesE a b, rest')
    (a, rest) -> (a, rest) 

-- parsePExp ts =
--   case parseAtom ts of
--     -- If the first pattern matches, we're in the inference rule for times
--     (e1,TimesSign:ts2) ->
--       let (e2, ts3) = parsePExp ts2
--       in (TimesE e1 e2, ts3)
--     -- If not, we're in the inference rule for atoms, and nothing more to do
--     (e1,ts2) -> (e1, ts2)

parseSExp ts =
  case parsePExp ts of
    (e1,PlusSign:ts2) ->
      let (e2, ts3) = parseSExp ts2
      in (PlusE e1 e2, ts3)
    (e1,ts2) -> (e1, ts2)
-- 这个是旧老师写的
-- parsePExp tokens = case parsePExp tokens of
--     (a, PlusSign:rest) -> case parsePExp rest of
--         (b, rest') -> (PlusE a b, rest')
--     (a, rest) -> (a, rest) 

-- A top-level parser function that complains if there are leftovers.
parse :: String -> ArithExp
parse str = case parseSExp (lexer str) of
    (e, []) -> e -- if empty just return the parsed e
    (_, c:_) -> error ("leftover tokens: " ++ show c) -- if not empty

-- from johnannes...
-- parse ts =
--   case parseSExp ts of
--     (e,[]) -> e
--     (_,c:_) -> error ("Unexpected token: " ++ show c)


{- This unparser will produce more parentheses than strictly necessary.
   It's a fun exercise to make one that produces minimal parentheses.
 -}
unparse :: ArithExp -> String
unparse(NumE i) = show i
unparse(TimesE e e') = "(" ++ unparse e ++ "*" ++ unparse e' ++ ")"
unparse(PlusE e e') = "(" ++ unparse e ++ "+" ++ unparse e' ++ ")"


--------------------------------HOAS--------------------------------
{- "just raw strings" representation -}
data ArithExpL =
  NumL Int
  | TimesL ArithExpL ArithExpL
  | PlusL ArithExpL ArithExpL
  | VarL String
  | LetL String ArithExpL ArithExpL
  deriving (Eq,Show)

{- de Bruijn represenation -}
data ArithExpDB =
  NumDB Int
  | TimesDB ArithExpDB ArithExpDB
  | PlusDB ArithExpDB ArithExpDB
  | VarDB Int
  | LetDB ArithExpDB ArithExpDB -- No need for var names on binding occurences
  deriving (Eq,Show)

{- HOAS representation -}
data ArithExpHOAS =
  NumHOAS Int
  | TimesHOAS ArithExpHOAS ArithExpHOAS
  | PlusHOAS ArithExpHOAS ArithExpHOAS
  | LetHOAS ArithExpHOAS (ArithExpHOAS -> ArithExpHOAS) -- a function that takes in Exp and outputs Exp
  | FreeHOAS String -- only used for convenience when converting from HOAS.
{-  deriving (Eq,Show)  doesn't work
    - Functions aren't considered comparable using == in Haskell.
    - Functions aren't considered pretty-printable either
 -}


example = LetHOAS (NumHOAS 3) (\x -> PlusHOAS x (NumHOAS 2))

{- Generic substitution problem:
    Whenever we change our syntax, we need to redefine substitution

   In the HOAS setting. Substitution is just function application.
 -}

-- The variable name at the i:th position
-- is the name of the i:th binder
deBruijnify :: [String] -> ArithExpL -> ArithExpDB
deBruijnify env (NumL i) = NumDB i
deBruijnify env (VarL s) =
  case elemIndex s env of
    Nothing -> error ("Out of scope variable: " ++ s)
    Just n -> VarDB n
deBruijnify env (TimesL e e') =
  TimesDB (deBruijnify env e) (deBruijnify env e')
deBruijnify env (PlusL e e') =
  PlusDB (deBruijnify env e) (deBruijnify env e')
deBruijnify env (LetL x e e') =
  LetDB (deBruijnify env e) (deBruijnify (x:env) e')

-- When we deconvert from de Bruijn notation,
-- we need to somehow invent variable names.
-- Here we say that the variable name at the
-- (n-i-1):th position is xi.
dedeBruijnify :: Int -> ArithExpDB -> ArithExpL
dedeBruijnify n (NumDB i) = NumL i
dedeBruijnify n (VarDB i) =
  if i <= n then
    VarL("x" ++ show(n-i-1))
  else
    error $ "Index out of scope: x" ++ show i
dedeBruijnify n (TimesDB e e') =
  TimesL (dedeBruijnify n e) (dedeBruijnify n e')
dedeBruijnify n (PlusDB e e') =
  PlusL (dedeBruijnify n e) (dedeBruijnify n e')
dedeBruijnify n (LetDB e e') =
  LetL ("x" ++ show n) (dedeBruijnify n e) (dedeBruijnify (n+1) e')

{-
   The following exp corresponds to
     let x = 5 in
       5 +
       (let y = 4 in
          x
        end)
     end
 -}
-- deBruijnify [] (LetL "x" (NumL 5) (PlusL (VarL "x") (LetL "y" (NumL 4) (VarL "x"))))

toHOAS :: [(String,ArithExpHOAS)] -> ArithExpL -> ArithExpHOAS
toHOAS env (NumL i) = NumHOAS i
toHOAS env (VarL s) =
  case lookup s env of
    Nothing -> error ("Out of scope variable: " ++ s)
    Just e -> e
toHOAS env (TimesL e e') =
  TimesHOAS (toHOAS env e) (toHOAS env e')
toHOAS env (PlusL e e') =
  PlusHOAS (toHOAS env e) (toHOAS env e')
toHOAS env (LetL x e e') =
  LetHOAS (toHOAS env e) (\e'' -> toHOAS ((x,e''):env) e')
  {-
     The (\x -> ...) notation defines functions without giving them
     names.  These are called lambda expression.  For example,
       (\x -> x + 1)
     is a function that adds 1 to an element, and can be called thus:
       (\x -> x + 1) 2

     Here, the body of the Let expression needs to be a function,
     which given a HOAS expression inserts that expression
     at all occurrences of x in the current scope.
     We achieve that by binding it to x in the current env,
     shadowing any pre-existing binders.
 -}

{-
   Just like the deBruijn case, we need to invent variable names.
   This time around, we'll just use a number n to keep track of how
   many variable names have already been used.
 -}
fromHOAS :: Int -> ArithExpHOAS -> ArithExpL
fromHOAS n (NumHOAS i) = NumL i
fromHOAS n (FreeHOAS s) = VarL s
fromHOAS n (TimesHOAS e e') =
  TimesL (fromHOAS n e) (fromHOAS n e')
fromHOAS n (PlusHOAS e e') =
  PlusL (fromHOAS n e) (fromHOAS n e')
fromHOAS n (LetHOAS e e') =
  let x = "x" ++ show n in
    LetL ("x" ++ show n) (fromHOAS n e) (fromHOAS (n+1) (e' (FreeHOAS x)))



--------- semantics ------------

