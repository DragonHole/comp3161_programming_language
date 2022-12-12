module MinHS.Evaluator where
  import qualified MinHS.Env as E
  import MinHS.Syntax
  import MinHS.Pretty
  import qualified Text.PrettyPrint.ANSI.Leijen as PP

  type VEnv = E.Env Value

  data Value = I Integer
             | B Bool
             | Nil
             | Cons Integer Value -- constructor for list of integers
             -- Others as needed
             -- CAN modify here
             | FuncV VEnv Id [Id] Exp -- function value (closure, function name, variable names, function expression)
             deriving (Show)

  instance PP.Pretty Value where
    pretty (I i) = numeric $ i
    pretty (B b) = datacon $ show b
    pretty (Nil) = datacon "Nil"
    pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
    pretty _ = undefined -- should not ever be used

  -- create distinct identifier by simply counting up
  counter :: String -> String
  counter s  = show (i+1) where
    i = read s :: Integer

  evaluate :: Program -> Value
  evaluate [Bind _ _ _ e] = evalE E.empty e
  evaluate bs = evalE E.empty (Let bs (Var "main"))
  -- E.empty: start with an empty environment, and the second paren is an expression


  evalE :: VEnv -> Exp -> Value

  -- do things like `evalE undefined (Num 5)`
  evalE env (Num n) = I n
  evalE env (Con "True") = B True
  evalE env (Con "False") = B False

  -- primitive operations :: int
  -- evalE env (App (App (Prim Add) e1) e2) = case (evalE env e1, evalE env e2) of
  --   (Nil, _) -> Nil
  --   (_, Nil) -> Nil
  evalE env (App (App (Prim Add) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> I (v1+v2)
    _ -> error "Add only supports int"
  evalE env (App (App (Prim Sub) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> I (v1-v2)
    _ -> error "Sub only supports int"
  evalE env (App (App (Prim Mul) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> I (v1*v2)
    _ -> error "Mul only supports int"
  evalE env (App (App (Prim Quot) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> if v2==0 then
      error "can't divide by zero"
      else I (quot v1 v2)
    _ -> error "Quot only supports int"
  evalE env (App (Prim Neg) e1) = case (evalE env e1) of
    (I v1) -> I (-v1)
    _ -> error "Neg only supports int"
  evalE env (App (App (Prim Gt) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> B (v1 > v2)
    _ -> error "Gt only supports int"
  evalE env (App (App (Prim Ge) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> B (v1 >= v2)
    _ -> error "Ge only supports int"
  evalE env (App (App (Prim Lt) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> B (v1 < v2)
    _ -> error "Lt only supports int"
  evalE env (App (App (Prim Le) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> B (v1 <= v2)
    _ -> error "Le only supports int"
  evalE env (App (App (Prim Eq) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> B (v1 == v2)
    _ -> error "Eq only supports int"
  evalE env (App (App (Prim Ne) e1) e2) = case (evalE env e1, evalE env e2) of
    (I v1, I v2) -> B (v1 /= v2)
    _ -> error "Ne only supports int"

  -- primitive operations :: list
  evalE env (Con "Nil") = Nil
  -- for constructing list

  evalE env (App (App (Con "Cons") e1) e2) = case evalE env e1 of
      I n -> (Cons n (evalE env e2))
      _   -> error "list only supports ints"
  evalE env (App (Prim Null) e1) = case (evalE env e1) of
    Nil -> (B True)
    _ -> (B False)
  evalE env (App (Prim Head) e1) = case (evalE env e1) of
    (Cons x xs) -> (I x)
    _ -> error "head cannot be applied to a NULL or int value"
  evalE env (App (Prim Tail) e1) = case (evalE env e1) of
    (Cons x xs) -> xs
    _ -> error "tail cannot be applied to a NULL or int value"

  -- partial function application, just return another function
  -- switch variable x to a fixed string
  evalE env (App (Prim f) e1) = case (evalE env e1) of
    (I x) -> (FuncV env "_" ["_"] e') where -- one bind pending
      e' = (App (App (Prim f) (Var "_")) (Num x))

  -- Letrec [Bind] Exp
  evalE env (Letrec bs e1) = case (bs) of
    [] -> (evalE env e1)
    bss -> case (bss) of
      (Bind varId t _ vexp):xbs -> case evalE env vexp of
      --Bind Id Type [Id] Exp
        n -> (evalE env' (Letrec (xbs) e1)) where env' = E.add env (varId, n)


  -- If
  evalE env (If ec et ee) = case (evalE env ec) of
    (B True) -> evalE env et
    (B False) -> evalE env ee
    _ -> error "If condition needs to evaluate to a boolean"


  -- Var
  evalE env (Var varID) = case (E.lookup env varID) of
    (Just Nil) -> Nil
    (Just (I v)) -> (I v)
    (Just (B v)) -> (B v)
    (Just (Cons x y)) -> (Cons x y)
    (Just (FuncV env' funcID [] e)) -> (evalE env' e) -- needs to be before the below line for the pattern matching mechanism
    (Just (FuncV env' funcID vars e)) -> (FuncV env' funcID vars e)
    Nothing -> Nil

  -- Let
  -- bs: bindings
  evalE env (Let [] e1) = evalE env e1 -- no binding anyways
  evalE env (Let bs e1) = case (head bs) of
    (Bind varID t [] es) -> evalE env' (Let (tail bs) e1) where -- recursive call another `Let` in case there's more bindings
      env' = E.add env (varID, (evalE env es))
    -- handle multi-bind, add one by one, can also use env.addAll()
    (Bind funcID t v es) -> evalE env' (Let (tail bs) e1) where -- if there's more bindings then recursive call another `Let`, todo: but it has same mark as `evalE env' e1`
      env' = E.add env (funcID, (FuncV env funcID v es))
    _ -> error "unhandled case in Let"



  -- Apply
  -- App (function value, argument)
  -- App (1)
  evalE env (App (Var s) e1) = case (E.lookup env s) of
    Just (FuncV oldEnv funcID v fe) -> runFunc env (FuncV oldEnv funcID v fe) e1
    Nothing -> error "Var not found"

  -- App (2)
  -- when applied directly to a Recfun expression
  evalE env (App fun e1) = case evalE env fun of
    (FuncV oldEnv funcID v fe)   -> runFunc env (FuncV oldEnv funcID v fe) e1

    -- `Recfun Bind` introduces a new, named function value, add it to environment.
  --  `Bind` contains the binding information of the function
  evalE env (Recfun (Bind funcID t args e1)) =
    FuncV env' funcID args e1 where
      env' = E.add env (funcID, (evalE env e1))

  -- Evaluate functions
  runFunc :: VEnv -> Value -> Exp -> Value
  runFunc env (FuncV oldEnv n v fe) e1 = case v of
    [] ->   let oldEnv' = E.add oldEnv (n, (FuncV oldEnv n [] fe)) -- for App (2) where the function value is yet to be added to environment
              in case (evalE oldEnv' fe) of
                -- if the function returns another function value, i.e
                FuncV oldEnv'' n' v' fe' -> runFunc env (FuncV oldEnv'' n' v' fe') e1
                x -> x -- if the function exp doesn't take an argument
    _ -> let
              oldEnv' = E.add oldEnv (n, FuncV oldEnv n v fe) -- step1 bind the function value to environment
              oldEnv'' = E.add oldEnv' (head v, evalE env e1) -- step2 bind the function argument to environment
            in if tail v /= [] -- if no more pending variables to bind to enviornment
              then (FuncV oldEnv'' n (tail v) fe)
              else (evalE oldEnv'' fe) -- there are more vars, keep binding




  -- evalE env e1 = error ("Implement me!")


-- module MinHS.Evaluator where
-- import qualified MinHS.Env as E
-- import MinHS.Syntax
-- import MinHS.Pretty
-- import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- type VEnv = E.Env Value

-- data Value = I Integer
--            | B Bool
--            | Nil
--            | Cons Integer Value -- constructor for list of integers
--            -- Others as needed
--            -- CAN modify here
--            | FuncV VEnv Id [Id] Exp -- function value (closure, function name, variable names, function expression)
--            deriving (Show)

-- instance PP.Pretty Value where
--   pretty (I i) = numeric $ i
--   pretty (B b) = datacon $ show b
--   pretty (Nil) = datacon "Nil"
--   pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
--   pretty _ = undefined -- should not ever be used

-- -- create distinct identifier by simply counting up
-- counter :: String -> String
-- counter s  = show (i+1) where
--   i = read s :: Integer

-- evaluate :: Program -> Value
-- evaluate [Bind _ _ _ e] = evalE E.empty e
-- evaluate bs = evalE E.empty (Let bs (Var "main")) 
-- -- E.empty: start with an empty environment, and the second paren is an expression

-- -- CAN add more staff here


-- evalE :: VEnv -> Exp -> Value

-- -- do things like `evalE undefined (Num 5)`
-- evalE env (Num n) = I n
-- evalE env (Con "True") = B True
-- evalE env (Con "False") = B False

-- -- primitive operations :: int
-- -- 反而分还少了？
-- -- evalE env (App (App (Prim Add) e1) e2) = case (evalE env e1, evalE env e2) of 
-- --   (Nil, _) -> Nil
-- --   (_, Nil) -> Nil
-- evalE env (App (App (Prim Add) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> I (v1+v2)
--   _ -> error "Add only supports int"
-- evalE env (App (App (Prim Sub) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> I (v1-v2)
--   _ -> error "Sub only supports int"
-- evalE env (App (App (Prim Mul) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> I (v1*v2)
--   _ -> error "Mul only supports int"
-- evalE env (App (App (Prim Quot) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> if v2==0 then
--     error "can't divide by zero"
--     else I (quot v1 v2)
--   _ -> error "Quot only supports int"
-- evalE env (App (Prim Neg) e1) = case (evalE env e1) of 
--   (I v1) -> I (-v1)
--   _ -> error "Neg only supports int"
-- evalE env (App (App (Prim Gt) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> B (v1 > v2)
--   _ -> error "Gt only supports int"
-- evalE env (App (App (Prim Ge) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> B (v1 >= v2)
--   _ -> error "Ge only supports int"
-- evalE env (App (App (Prim Lt) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> B (v1 < v2)
--   _ -> error "Lt only supports int"
-- evalE env (App (App (Prim Le) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> B (v1 <= v2)
--   _ -> error "Le only supports int"
-- evalE env (App (App (Prim Eq) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> B (v1 == v2)
--   _ -> error "Eq only supports int"
-- evalE env (App (App (Prim Ne) e1) e2) = case (evalE env e1, evalE env e2) of 
--   (I v1, I v2) -> B (v1 /= v2)
--   _ -> error "Ne only supports int"

-- -- primitive operations :: list
-- evalE env (Con "Nil") = Nil
-- -- for constructing list

-- -- todo:not needed?
-- -- evalE env (App (Con "Cons") e1) = case (evalE env e1) of
-- --   (Nil)-> error "can't append Nil to integer list"
-- evalE env (App (App (Con "Cons") e1) e2) = case evalE env e1 of 
--     I n -> (Cons n (evalE env e2))
--     _   -> error "list only supports ints"
-- evalE env (App (Prim Null) e1) = case (evalE env e1) of
--   Nil -> (B True)
--   _ -> (B False)
-- evalE env (App (Prim Head) e1) = case (evalE env e1) of
--   (Cons x xs) -> (I x)
--   _ -> error "head cannot be applied to a NULL or int value"
-- evalE env (App (Prim Tail) e1) = case (evalE env e1) of
--   (Cons x xs) -> xs
--   _ -> error "tail cannot be applied to a NULL or int value"

-- -- partial function application, just return another function
-- -- switch variable x to a fixed string
-- evalE env (App (Prim f) e1) = case (evalE env e1) of
--   (I x) -> (FuncV env "_" ["_"] e') where -- one bind pending
--     e' = (App (App (Prim f) (Var "_")) (Num x))


--   -- Nothing -> Error "Function not found"
--   -- _ -> error "App cannot be applied to a NULL value"


-- -- If
-- evalE env (If ec et ee) = case (evalE env ec) of
--   (B True) -> evalE env et 
--   (B False) -> evalE env ee
--   _ -> error "If condition needs to evaluate to a boolean"


-- -- Var
-- -- todo?:  only lookup the leftmost?
-- evalE env (Var varID) = case (E.lookup env varID) of
--   (Just Nil) -> Nil
--   (Just (I v)) -> (I v)
--   (Just (B v)) -> (B v)
--   (Just (Cons x y)) -> (Cons x y)
--   (Just (FuncV env' funcID [] e)) -> (evalE env' e) -- needs to be before the below line for the pattern matching mechanism
--   (Just (FuncV env' funcID vars e)) -> (FuncV env' funcID vars e)
--   Nothing -> Nil 

-- -- Letrec [Bind] Exp
-- evalE env (Letrec bs e1) = case (bs) of 
--   [] -> (evalE env e1)
--   bss -> case (bss) of
--     (Bind varId t _ vexp):xbs -> case evalE env vexp of
--     --Bind Id Type [Id] Exp
--       n -> (evalE env' (Letrec (xbs) e1)) where env' = E.add env (varId, n)


-- -- Let 
-- -- bs: bindings
-- evalE env (Let [] e1) = evalE env e1 -- no binding anyways
-- evalE env (Let bs e1) = case (head bs) of
--   (Bind varID t [] es) -> evalE env' (Let (tail bs) e1) where -- recursive call another `Let` in case there's more bindings
--     env' = E.add env (varID, (evalE env es))
--   -- handle multi-bind, add one by one, can also use env.addAll() 
--   (Bind funcID t v es) -> evalE env' (Let (tail bs) e1) where -- if there's more bindings then recursive call another `Let`, todo: but it has same mark as `evalE env' e1`
--     env' = E.add env (funcID, (FuncV env funcID v es))
--   _ -> error "unhandled case in Let"
--   --[] -> (evalE env e1) -- this case is already handled

-- -- `Recfun Bind` introduces a new, named function value, add it to environment. 
-- --  `Bind` contains the binding information of the function
-- evalE env (Recfun (Bind funcID t args e1)) = 
--   FuncV env' funcID args e1 where 
--     env' = E.add env (funcID, (evalE env e1))
-- -- evalE env (Recfun (Bind funcID t a e1)) = 
-- --   evalE env' e1 where
-- --     env' = E.add env (funcID, (FuncV env funcID a e1))

-- -- it turns out Recfun shouldn't return a value?
-- -- evalE env (Recfun (Bind funcID t a e1)) = 
-- --   (FuncV env funcID a e1)

-- -- data Bind = Bind Id Type [Id] Exp
-- -- <Turn the above into the below>
-- -- FuncV VEnv Id [Id] Exp


-- -- Apply
-- -- App (function value, argument)
-- -- evalE env (App (Recfun (Bind f t ids e1)) arg) = let

-- -- todo: rewrite LMAO
-- -- App (1)
-- evalE env (App (Var s) e1) = case (E.lookup env s) of 
--   Just (FuncV oldEnv funcID v fe) -> runFunc env (FuncV oldEnv funcID v fe) e1
--   Nothing -> error "Var not found"

-- -- App (2)
-- -- when applied directly to a Recfun expression 
-- evalE env (App fun e1) = case evalE env fun of 
--   (FuncV oldEnv funcID v fe)   -> runFunc env (FuncV oldEnv funcID v fe) e1

-- -- Evaluate functions
-- runFunc :: VEnv -> Value -> Exp -> Value
-- -- todo: can remove this first definition?
-- -- runFunc g (FuncV fg n [] fe) e1 = 
-- --   let fg' = E.add fg (n, (FuncV fg n [] fe)) -- for App (2) where the function value is yet to be added to environment
-- --   in case (evalE fg' fe) of 
-- --     -- if the function returns another function value, i.e 
-- --     FuncV fg'' n' v' fe' -> runFunc g (FuncV fg'' n' v' fe') e1
-- --     x -> x -- if the function exp doesn't take an argument 

-- runFunc env (FuncV oldEnv n v fe) e1 = case v of
--   [] ->   let oldEnv' = E.add oldEnv (n, (FuncV oldEnv n [] fe)) -- for App (2) where the function value is yet to be added to environment
--             in case (evalE oldEnv' fe) of 
--               -- if the function returns another function value, i.e 
--               FuncV oldEnv'' n' v' fe' -> runFunc env (FuncV oldEnv'' n' v' fe') e1
--               x -> x -- if the function exp doesn't take an argument 
--   _ -> let 
--             oldEnv' = E.add oldEnv (n, FuncV oldEnv n v fe) -- step1 bind the function value to environment
--             oldEnv'' = E.add oldEnv' (head v, evalE env e1) -- step2 bind the function argument to environment
--           in if tail v /= [] -- if no more pending variables to bind to enviornment
--             then (FuncV oldEnv'' n (tail v) fe)
--             else (evalE oldEnv'' fe) -- there are more vars, keep binding


-- -- evalE env e1 = error ("Implement me!")

