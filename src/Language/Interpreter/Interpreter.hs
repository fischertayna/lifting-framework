module Language.Interpreter.Interpreter where

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

type VContext = Context Ident Valor

type FContext = Context Ident Function

data Valor
    = ValorInt
        { i :: Integer
        }
    | ValorBool
        { b :: Bool
        }
    | ValorStr
        { s :: String
        }
    | ValorList
        { l :: [Valor]
        }
    | ValorPair
        { p :: (Valor, Valor)
        }
    deriving (Show, Eq)

evalP :: Program -> Valor -> Valor
evalP (Prog fs) input = eval context (Call (Ident "main") [EVar (Ident "n")])
  where
    initialFContext = updatecF ([(Ident "n", input)], []) fs
    context = initialFContext

eval :: RContext -> Exp -> Valor
eval context@(vcontext, fcontext) x = case x of
  ECon exp0 exp1  -> ValorStr (s (eval context exp0) ++ s (eval context exp1))
  EAdd exp0 exp1  -> ValorInt (i (eval context exp0) + i (eval context exp1))
  ESub exp0 exp1  -> ValorInt (i (eval context exp0) - i (eval context exp1))
  EMul exp0 exp1  -> ValorInt (i (eval context exp0) * i (eval context exp1))
  EDiv exp0 exp1  -> ValorInt (i (eval context exp0) `div` i (eval context exp1))
  EOr exp0 exp1   -> ValorBool (b (eval context exp0) || b (eval context exp1))
  EAnd exp0 exp1  -> ValorBool (b (eval context exp0)  && b (eval context exp1))
  ENot exp1        -> ValorBool ( not (b (eval context exp1)))
  EStr s          -> ValorStr s
  ETrue           -> ValorBool True
  EFalse          -> ValorBool False
  EInt n          -> ValorInt n
  EVar vId -> fromJust $ lookup vcontext vId
  EIf cond expi expe -> case eval context cond of
    ValorInt 0 -> eval context expe
    _ -> eval context expi
  EList list -> ValorList (map (eval context) list)
  EPair p1 p2 -> ValorPair (eval context p1, eval context p2)
  Call id pExps -> case id of
    Ident "head" -> head list
    Ident "tail" ->  ValorList (tail list)
    Ident "isNil" -> ValorInt (boolToInt (null list))
    Ident "fst" -> f
    Ident "snd" -> s
    Ident "isPair" -> case arg of
      ValorPair _ -> ValorInt 1
      _ -> ValorInt 0
    Ident "isEqual" -> let
        val1 = eval context (pExps !! 0)
        val2 = eval context (pExps !! 1)
      in ValorInt (boolToInt (val1 == val2))
    Ident func -> eval (paramBindings, fcontext) fExp
    where
      arg = eval context (head pExps)
      list = l arg
      (f,s) = p arg
      (Fun _ _ decls fExp) = fromJust $ lookup fcontext id
      paramBindings = zip decls (map (eval context) pExps)

boolToInt :: Bool -> Integer
boolToInt b
  | not b = 0
  | otherwise = 1

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
updatecF (vcontext, fcontext) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext) fs
  where
    newFContext = update fcontext fId f
