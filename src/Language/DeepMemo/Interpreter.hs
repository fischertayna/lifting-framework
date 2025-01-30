{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Language.DeepMemo.Interpreter where

import Data.Maybe (fromJust)
import Language.Frontend.AbsLanguage
  ( Exp (..),
    Function (..),
    Ident (..),
    Program (..),
  )
import Memoization.Core.Memory (KeyMemory, KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State, (<.>))
import Variability.VarTypes
  ( PresenceCondition,
    VarValor(..),
    Var (..),
    Context,
    substitute,
    negPC,
    ffPC,
    sat,
    unsat,
    doTraceOrResult,
    disj,
    mkBDDVar,
    notBDD,
    tt, ff,
    ttPC,
    valList,
    (+++),
    (++++), (||||),
    (|||), (/\), (\/),
    union
  )
import Prelude hiding (lookup)
import Debug.Trace
import Data.List (sortBy)
import Language.MInterpreter.Interpreter (Valor(ValorList, ValorInt))
import Language.VInterpreter.Functions
  (
    applyBinaryOperator,
    applyUnaryOperator,
    applyEqualOperator,
    applyLtOperator,
    applySortList,
    applyUnion,
    applyDifference,
    partition,
    lookup,
    update,
    boolToInt
  )
import Data.Hashable (hash, Hashable)

type RContext m = (VContext m, FContext, [Ident])

type VContext m = Context Ident (State m VarValor)

type FContext = Context Ident Function

data FuncKey = FuncKey
  { funcName :: String   -- Name of the function
  , funcArgsHash :: Int  -- Hash of the function arguments
  } deriving (Eq, Show)

createFuncKey :: String -> [VarValor] -> FuncKey
createFuncKey name args = FuncKey
  { funcName = name
  , funcArgsHash = hash args
  }

type Mem =  (KeyValueArray FuncKey VarValor)

evalP :: Program -> [String] -> State Mem (VarValor -> State Mem VarValor)
evalP (Prog fs) memoizedFunctionNames =
  return
    ( \input ->
        -- trace ("\n\n evalP: Called with memoizedFunctionNames: " ++ show memoizedFunctionNames ++ "\n") $
        let memoizedIdents = map Ident memoizedFunctionNames
            initialFContext = updatecF ([(Ident "n", return input)], [], memoizedIdents) fs
         in let context = initialFContext
             in eval context <.> return (Call (Ident "main") [EVar (Ident "n")])
    )

eval :: RContext Mem -> State Mem (Exp -> State Mem VarValor)
eval context@(vcontext, fcontext, memoizedFunctionNames) = do
  return
    ( \x -> case x of
        EInt n -> return (VarInteger (Var [(n, ttPC)]))
        (ECon exp0 exp1) -> do
          val0 <- eval context <.> return exp0
          val1 <- eval context <.> return exp1
          case (val0, val1) of
            (VarString s0, VarString s1) -> applyBinaryOperatorWithState VarString str context exp0 exp1 (++)
            (VarList l0, VarList l1) -> return $ VarList (l0 ++ l1)
            _ -> error "Type error in concatenation"
        (EAdd exp0 exp1) -> applyBinaryOperatorWithState VarInteger int context exp0 exp1 (+)
        (ESub exp0 exp1) -> applyBinaryOperatorWithState VarInteger int context exp0 exp1 (-)
        (EMul exp0 exp1) -> applyBinaryOperatorWithState VarInteger int context exp0 exp1 (*)
        (EDiv exp0 exp1) -> applyBinaryOperatorWithState VarInteger int context exp0 exp1 div
        (EOr exp0 exp1)  -> applyBinaryOperatorWithState VarBool bool context exp0 exp1 (||)
        (EAnd exp0 exp1) -> applyBinaryOperatorWithState VarBool bool context exp0 exp1 (&&)
        (ENot exp1)        -> applyUnaryOperatorWithState VarBool bool context exp1 not
        EVar vId -> fromJust (lookup vcontext vId)
        EStr s          -> return (VarString (Var [(s, ttPC)]))
        ETrue           -> return (VarBool (Var [(True, ttPC)]))
        EFalse          -> return (VarBool (Var [(False, ttPC)]))
        EList exps -> do
          vals <- mapM (\e -> eval context <.> return e) exps
          return $ VarList vals
        EPair p1 p2 -> do
          v1 <- eval context <.> return p1
          v2 <- eval context <.> return p2
          return $ VarPair (v1, v2)
        EIf cond eT eE -> do
          e <- eval context <.> return cond
          let (pct, pcf) = partition e
          if pct == ttPC
            then eval context <.> return eT
            else
              if pct == ffPC
                then eval context <.> return eE
                else applyRestrict context pct pcf eT eE
        Call fId pExps -> case fId of
          Ident "head" -> do
            arg <- (eval context <.> return (pExps !! 0))
            let ls = list arg
            return $ head ls
          Ident "tail" -> do
            arg <- (eval context <.> return (pExps !! 0))
            let ls = list arg
            return $ VarList (tail ls)
          Ident "isNil" -> do
            arg <- (eval context <.> return (pExps !! 0))
            let ls = list arg
            return $ VarInteger (Var [(boolToInt (null ls), ttPC)])
          Ident "fst" -> do
            arg <- (eval context <.> return (pExps !! 0))
            case arg of
              VarPair (f, _) -> return f
              _ -> error $ "fst: Expected VarPair but got: " ++ substitute (show arg)
          Ident "snd" -> do
            arg <- (eval context <.> return (pExps !! 0))
            case arg of
              VarPair (_, s) -> return s
              _ -> error $ "snd: Expected VarPair but got: " ++ substitute (show arg)
          Ident "isPair" -> do
            arg <- (eval context <.> return (pExps !! 0))
            case arg of
              VarPair _ -> return $ VarInteger (Var [(1, ttPC)])
              _ -> return $ VarInteger (Var [(0, ttPC)])
          Ident "isEqual" -> applyEqualOperatorWithState context (pExps !! 0) (pExps !! 1)
          Ident "sortList" -> applySortListWithState context (pExps !! 0)
          Ident "lt" -> applyLtOperatorWithState context (pExps !! 0) (pExps !! 1)
          Ident "union" -> applyUnionWithState context (pExps !! 0) (pExps !! 1)
          Ident "difference" -> applyDifferenceWithState context (pExps !! 0) (pExps !! 1)
          _ ->
            if fId `elem` memoizedFunctionNames
              then 
                  -- trace ("\n eval: Memoized function call to " ++ show (getIdentString fId)) $
                  memoizedCall context fId pExps
              else 
                -- trace ("\n eval: Regular function call to " ++ show (getIdentString fId)) $ 
                regularCall context fId pExps
                -- let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
                -- -- trace ("\n eval: Function declaration: " ++ show decls ++ ", body: " ++ show fExp) $
                --     paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
                --     in eval (paramBindings, fcontext, memoizedFunctionNames) <.> return fExp
                --     -- result = eval (paramBindings, fcontext, memoizedFunctionNames) <.> return fExp
                --     -- in trace ("\n eval: Function (" ++ show (getIdentString fId) ++ ") result: " ++ show result) $ result
    )

applyBinaryOperatorWithState :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext Mem -> Exp -> Exp -> (a -> a -> a) -> State Mem VarValor
applyBinaryOperatorWithState cons f context exp0 exp1 op = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = applyBinaryOperator cons f v0 v1 op
  return $ result

applyUnaryOperatorWithState :: (Var a -> VarValor) -> (VarValor -> Var a) -> RContext Mem -> Exp -> (a -> a) -> State Mem VarValor
applyUnaryOperatorWithState cons f context exp op = do
  v <- (eval context <.> return exp)
  let result = applyUnaryOperator cons f v op
  return $ result

applyEqualOperatorWithState :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyEqualOperatorWithState context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = applyEqualOperator v0 v1
  return $ result

applyLtOperatorWithState :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyLtOperatorWithState context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = applyLtOperator v0 v1
  return $ result

applySortListWithState :: RContext Mem -> Exp -> State Mem VarValor
applySortListWithState context exp = do
  v <- (eval context <.> return exp)
  let result = applySortList v
  return $ result

applyUnionWithState :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyUnionWithState context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = applyUnion v0 v1
  return $ result

applyDifferenceWithState :: RContext Mem -> Exp -> Exp -> State Mem VarValor
applyDifferenceWithState context exp0 exp1 = do
  v0 <- (eval context <.> return exp0)
  v1 <- (eval context <.> return exp1)
  let result = applyDifference v0 v1
  return $ result

regularCall :: RContext Mem -> Ident -> [Exp] -> State Mem VarValor
regularCall context@(vcontext, fcontext, memoizedFunctionNames) fId pExps =
  let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
   in let paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
          name = (getIdentString fId)
          in  eval (paramBindings, fcontext, memoizedFunctionNames) <.> return fExp
        -- in let paramListM = mapM snd paramBindings
        --     in do
        --       -- trace ("\n regularCall "++ show name ++ ": decls = " ++ show decls) $ return ()
        --       -- trace ("\n regularCall "++ show name ++ ": paramBindings = " ++ show (map fst paramBindings)) $ return ()
        --       -- paramList <- paramListM
        --       -- trace ("\n regularCall "++ show name ++ ": paramList = " ++ show paramList) $ return ()
        --       result <- eval (paramBindings, fcontext, memoizedFunctionNames) <.> return fExp
        --       -- trace ("\n regularCall "++ show name ++ ": result = " ++ show result) $ return ()
        --       return result

memoizedCall :: RContext Mem -> Ident -> [Exp] -> State Mem VarValor
memoizedCall context@(vcontext, fcontext, memoizedFunctionNames) fId pExps =
  let (Fun _ _ decls fExp) = fromJust $ lookup fcontext fId
   in let paramBindings = zip decls (map (\e -> eval context <.> return e) pExps)
          name = (getIdentString fId)
       in let paramListM = mapM snd paramBindings
           in do
                -- trace ("\n memoizedCall "++ show name ++ ": decls = " ++ show decls) $ return ()
                -- trace ("\n memoizedCall "++ show name ++ ": paramBindings = " ++ show (map fst paramBindings)) $ return ()
                paramList <- paramListM
                -- trace ("\n memoizedCall "++ show name ++ ": paramList = " ++ show paramList) $ return ()
                retrieveOrRun name (createFuncKey name paramList) (\_ -> eval (paramBindings, fcontext, memoizedFunctionNames) <.> return fExp)

updatecF :: RContext Mem -> [Function] -> RContext Mem
updatecF c [] = c
updatecF (vcontext, fcontext, memoizedFunctionNames) (f@(Fun _ fId _ _) : fs) = updatecF (vcontext, newFContext, memoizedFunctionNames) fs
  where
    newFContext = update fcontext fId f

restrictVarValue :: State Mem VarValor -> PresenceCondition -> State Mem VarValor
restrictVarValue varState pc = do
  varValue <- varState
  let restrictedVarValue = varValue |||| pc
  return restrictedVarValue

restrictContext :: RContext Mem -> PresenceCondition -> RContext Mem
restrictContext context@(vcontext, fcontext, memoizedFunctionNames) pc = (restrictedVContext, fcontext, memoizedFunctionNames)
  where
    restrictedVContext = [(vId, restrictVarValue v pc) | (vId, v) <- vcontext]


applyRestrict :: RContext Mem -> PresenceCondition -> PresenceCondition -> Exp -> Exp -> State Mem VarValor
applyRestrict context pct pcf eT eE = do
  restrictedEvalET <-  eval (restrictContext context pct) <.> return eT
  restrictedEvalEE <-  eval (restrictContext context pcf) <.> return eE
  return $ (restrictedEvalET |||| pct) ++++ (restrictedEvalEE |||| pcf)

