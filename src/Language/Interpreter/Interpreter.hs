module Language.Interpreter.Interpreter (evalP) where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Prelude hiding (lookup)

type Context k v = [(k, v)]

type RContext = (VContext, FContext)

type VContext = Context Ident Integer

type FContext = Context Ident Function

evalP :: Program -> Integer -> Integer
evalP (Prog fs) input = eval context (Call (Ident "main") [EVar (Ident "n")])
  where
    initialFContext = updatecF ([(Ident "n", input)], []) fs
    context = initialFContext

eval :: RContext -> Exp -> Integer
eval context@(vcontext, fcontext) x = case x of
  EAdd exp0 exp1 -> eval context exp0 + eval context exp1
  ESub exp0 exp1 -> eval context exp0 - eval context exp1
  EMul exp0 exp1 -> eval context exp0 * eval context exp1
  EDiv exp0 exp1 -> eval context exp0 `div` eval context exp1
  EInt n -> n
  EVar vId -> fromJust $ lookup vcontext vId
  EIf cond expi expe -> case eval context cond of
    0 -> eval context expe
    _ -> eval context expi
  Call fId pExps -> eval (paramBindings, fcontext) fExp
    where
      (Fun _ decls fExp) = fromJust $ lookup fcontext fId
      paramBindings = zip decls (map (eval context) pExps)

lookup :: Eq k => Context k v -> k -> Maybe v
lookup [] _ = Nothing
lookup ((i, v) : cs) s
  | i == s = Just v
  | otherwise = lookup cs s

update :: Eq k => Context k v -> k -> v -> Context k v
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF (vcontext, fcontext) (f@(Fun fId _ _) : fs) = updatecF (vcontext, newFContext) fs
  where
    newFContext = update fcontext fId f
