{-# LANGUAGE ScopedTypeVariables #-}

module UnTyLambda.Interpreter where

import Prelude hiding (catch)
import Control.Exception

type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

-- свободные переменные 
free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

--why?
newname fv = head . filter (not . flip elem fv) . iterate ('_':)

betaReduct :: Variable -> Term -> Term -> Term
betaReduct var what term = subst  (renameBindings (free what) term) var what
  where renameBindings vars subterm = case subterm of 
          Var _ -> subterm
          App t t' -> App (renameBindings vars t) (renameBindings vars t')
          Lam n t -> Lam nn newt
            where nameUsed = elem n vars
                  nn = if nameUsed then newname (vars ++ free t) n else n
                  newt = if nameUsed then subst t n (Var nn)else t

hasRedexes (Var _) = False
hasRedexes (Lam v t) = hasRedexes t
hasRedexes (App (Lam _ t) t') = True
hasRedexes (App t t') = hasRedexes t || hasRedexes t'

normalReduce term = case term of
  Var _ -> term
  Lam var subterm -> Lam var $ normalReduce subterm
  App (Lam var subterm) term' -> betaReduct var term' subterm
  App term term' -> if hasRedexes term
                    then App (normalReduce term) term'
                    else App term $ normalReduce term'

applicativeReduce term = case term of
  Var _ -> term
  Lam var subterm -> Lam var $ applicativeReduce subterm
  App term term' -> if hasRedexes term' 
                    then App term $ applicativeReduce term' 
                    else case term of
                      Lam v subt -> betaReduct v term' subt
                      _ -> App (applicativeReduce term) term'

inWeakHeadForm (Var _) = True
inWeakHeadForm (Lam _ _) = True
inWeakHeadForm (App (Lam _ t) t') = False
inWeakHeadForm (App t t') = inWeakHeadForm t

weakHeadReduce term = case term of
  App (Lam v t) t2 -> subst t v t2
  App t1 t2 -> App (weakHeadReduce t1) t2
  _ -> term

wh, no, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
sa 0 term = error $ "Too long sequence at [" ++ show term ++ "]"
sa n term = if (hasRedexes term) 
                then sa (n - 1) $ applicativeReduce term
                else term

-- Нормализация нормальным порядком
no 0 term = error $ "Too long sequence at [" ++ show term ++ "]"
no n term = if (hasRedexes term) 
                then no (n - 1) $ normalReduce term
                else term

-- Редукция в слабую головную нормальную форму
wh 0 term = error $ "Too long sequence at [" ++ show term ++ "]"
wh n term = if (inWeakHeadForm term) 
                then term
                else wh (n - 1) $ weakHeadReduce term

-- Редукция в слабую головную нормальную форму
--wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
--wh n (App (Lam v t) t') = wh (n - 1) $ subst t v t'
--wh n t@(Lam v b) = Lam v $ wh (n - 1) b
--wh n t = t
  
orders =
    [ 
    ("wh", wh)
    , ("no", no)
    ,("sa", sa)
     ]

pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

my1 = Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
my2 = (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
my3 = App lamxx (Var "y")

test = testfuncs orders
    [ Var "a"
    , my1
    , my2
    , my3
    , omega
    ]
