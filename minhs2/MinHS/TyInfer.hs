module MinHS.TyInfer where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Subst
import MinHS.TCMonad

import Data.Monoid (Monoid (..), (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union, (\\))

-- infer type for primary operations, just hardcode
primOpType :: Op -> QType -- note $ makes it right-associative
primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Neg  = Ty $ Base Int `Arrow` Base Int
primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int)

-- 
constType :: Id -> Maybe QType
constType "True"  = Just $ Ty $ Base Bool -- Maybe+QType wraps on base type
constType "False" = Just $ Ty $ Base Bool
constType "()"    = Just $ Ty $ Base Unit
constType "Pair"  = Just --    :: a -> b -> a x b
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
constType "Inl"   = Just  -- `In Left` :: a -> (a + b)
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType "Inr"   = Just -- `In Right` for product type :: b -> (a + b)
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType _       = Nothing

-- an environment of QTypes
type Gamma = E.Env QType

initialGamma :: Gamma
initialGamma = E.empty

tv :: Type -> [Id]
tv = tv'
 where
   tv' (TypeVar x) = [x]
   tv' (Prod  a b) = tv a `union` tv b
   tv' (Sum   a b) = tv a `union` tv b
   tv' (Arrow a b) = tv a `union` tv b
   tv' (Base c   ) = []

tvQ :: QType -> [Id]
tvQ (Forall x t) = filter (/= x) $ tvQ t
tvQ (Ty t) = tv t

tvGamma :: Gamma -> [Id]
tvGamma = nub . foldMap tvQ

infer :: Program -> Either TypeError Program
infer program = do (p',t, s) <- runTC $ inferProgram initialGamma program
                   return p'

unquantify :: QType -> TC Type
{-
Normally this implementation would be possible:

unquantify (Ty t) = return t
unquantify (Forall x t) = do x' <- fresh
                             unquantify (substQType (x =:x') t)

However as our "fresh" names are not checked for collisions with names bound in the type
we avoid capture entirely by first replacing each bound
variable with a guaranteed non-colliding variable with a numeric name,
and then substituting those numeric names for our normal fresh variables
-}

unquantify = unquantify' 0 emptySubst
unquantify' :: Int -> Subst -> QType -> TC Type
unquantify' i s (Ty t) = return $ substitute s t
unquantify' i s (Forall x t) = do x' <- fresh
                                  unquantify' (i + 1)
                                              ((show i =: x') <> s)
                                              (substQType (x =:TypeVar (show i)) t)
unify :: Type -> Type -> TC Subst
unify (Base t1) (Base t2) = 
  case t1==t2 of 
    True -> return emptySubst 
    False -> typeError $ TypeMismatch (Base t1) (Base t2)

unify (TypeVar t1) (TypeVar t2) = 
  case t1==t2 of
    True -> return emptySubst
    False -> return (t2 =: (TypeVar t1))

unify (Prod t1a t1b) (Prod t2a t2b) = do
  sub <- unify t1a t2a
  sub2 <- unify (substitute sub t1b) (substitute sub t2b)
  return $ mappend sub sub2

unify (Sum t1a t1b) (Sum t2a t2b) = do
  sub <- unify t1a t2a
  sub2 <- unify (substitute sub t1b) (substitute sub t2b)
  return $ mappend sub sub2

unify (Arrow t1a t1b) (Arrow t2a t2b) = do
  sub <- unify t1a t2a 
  sub2 <- unify (substitute sub t1b) (substitute sub t2b)
  return $ mappend sub sub2

unify (TypeVar v) t = 
  if elem v (tv t)
    then 
      typeError $ OccursCheckFailed v t
    else 
      return $ v =: t

unify t (TypeVar v) = unify (TypeVar v) t

unify t1 t2 = typeError $ TypeMismatch t1 t2


bvarN :: Gamma -> [Bind] -> TC ([Bind], Gamma, Subst)
bvarN env bindings = bvarN' env bindings [] emptySubst

bvarN' :: Gamma -> [Bind] -> [Bind] -> Subst -> TC ([Bind], Gamma, Subst)
bvarN' env [] bindings sub = return $ (reverse bindings, env, sub)
bvarN' env ((Bind x t0 [] e):xs) bindings ss = do
  (e', ty, sub)  <- inferExp env e 
  case t0 of 
    Nothing-> do 
      let env' =  substGamma sub $ env `E.add` (x, generalise (substGamma sub env) ty)
      (bvarN' env' xs ((Bind x (Just (generalise env' ty)) [] e'):bindings) (mappend sub ss))
    Just (Ty ty') -> do 
      u <- unify ty' ty
      let env' =  substGamma sub $ env `E.add` (x, generalise (substGamma sub env) ty)
      (bvarN' env' xs ((Bind x (Just (generalise env' ty)) [] e'):bindings) (mappend sub ss))

convert :: [Id] -> Type -> QType
convert [] ty = Ty ty
convert (x:xs) ty = Forall x (convert xs ty)
generalise :: Gamma -> Type -> QType

generalise env ty = convert ((tvQ (Ty ty)) \\ (tvGamma env)) ty             


inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
inferProgram env [Bind name _ [] exp] = do 
  (e, ty, sub) <- inferExp env exp 
  case generalise env (substitute sub ty) of 
    (ty')-> return ([Bind "main" (Just $ ty') [] e], substitute sub ty, sub)
    (Ty ty) -> return ([Bind "main" (Just $ Ty (substitute sub ty)) [] e], substitute sub ty, sub)
    --_ -> typeError $ MalformedAlternatives 
--inferProgram env bs = error "implement me! don't forget to run the result substitution on the"
--                            "entire expression using allTypes from Syntax.hs"

inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
-- inferExp g _ = error "Implement me!"

inferExp _ (Num i) = do return (Num i, Base Int, emptySubst)

inferExp env (Var id) = do
  case E.lookup env id of 
    Just ty -> do
      ty2 <- unquantify ty
      return (Var id, ty2, emptySubst)
    Nothing -> typeError $ NoSuchVariable $ "variable not found"

inferExp _ (Con e) = do
  case constType e of
    Just ty -> do
      ty2 <- unquantify ty
      return (Con e, ty2, emptySubst)
    Nothing -> typeError $ NoSuchConstructor $ "Constructor not found"

inferExp _ (Prim e) = do
  case primOpType e of
    x -> do
      ty <- unquantify x
      return (Prim e, ty, emptySubst)

inferExp env (App e1 e2) = do
  x <- fresh
  (e12, t1, sub) <- inferExp env e1 -- infer the function body type first
  (e22, t2, sub2) <- inferExp (substGamma sub env) e2 -- infer the argument type
  y <- unify (substitute sub2 t1) (Arrow t2 x)
  return (App (allTypes (substQType (mappend y sub2)) e12) e22, (substitute y x), (y<>sub<>sub2))

inferExp env (If ec et ee) = do 
  (ec', ty, sub) <- inferExp env ec
  u <- unify ty (Base Bool)
  case substitute (mappend u sub) ty of 
    Base Bool   -> do
      let env1 = substGamma (mappend u sub) env
      (et', t1, sub1) <- inferExp env1 et
      let env2 = substGamma (sub1 <> u <> sub) env
      (ee', t2, sub2) <- inferExp env2 ee
      u'<- unify (substitute sub2 t1) t2 
      return ((If ec' et' ee'), substitute u' t2, u' <> sub2 <> sub1 <> u <> sub)
    t -> typeError $ TypeMismatch (Base Bool) ty

inferExp env (Recfun (Bind f _ [x] e)) = do 
    alpha1 <- fresh
    alpha2 <- fresh
    let env1 = E.add (E.add env (x, Ty alpha1)) (f, Ty alpha2)
    (e', ty, sub) <- inferExp env1 e
    let ft = Arrow (substitute sub alpha1) ty 
    u <- unify (substitute sub alpha2) ft
    let finalType = substitute u ft
    return (Recfun (Bind f (Just (Ty finalType)) [x] e'), finalType, mappend u sub)  


inferExp env (Case expr [Alt "Inl" [x] expr1, Alt "Inr" [y] expr2]) = 
  do al <- fresh
     ar <- fresh
     (expr', t, s) <- inferExp env expr
     let env1 = substGamma s (E.add env (x, Ty al)) 
     (expr1', tl, s1) <- inferExp env1 expr1
     let env2 = substGamma s1 (substGamma s (E.add env (y, Ty ar)))
     (expr2', tr, s2) <- inferExp env2 expr2
     u <- unify (substitute s2 (substitute s1 (substitute s (Sum al ar)))) (substitute s2 (substitute s1 t))
     u' <- unify (substitute u (substitute s2 tl)) (substitute u tr)
     let finType = substitute u' (substitute u tr)
     return (Case expr' [Alt "Inl" [x] expr1', Alt "Inr" [y] expr2'], finType, u' <> u <> s2 <> s1 <> s)
inferExp env (Case e _) = typeError MalformedAlternatives


inferExp env (Let bindings e) = do
  (bindings', env', sub) <- bvarN env bindings 
  (e', t', sub') <- inferExp env' e 
  return (allTypes (substQType (mappend sub sub')) (Let bindings' e'), t', mappend sub sub')


inferExp _ e = error (show e)