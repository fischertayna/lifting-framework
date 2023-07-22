module Language.Typechecker.Typechecker where

import Language.Frontend.AbsLanguage
import Language.Frontend.PrintLanguage
import Prelude hiding (lookup)

{-
(types, functions)
typechecker: lookup deep, update shallow,  lookup shallow, update shallow

(values,functions)
interpreter: lookup deep, update deep, lookup shallow, update shallow

-}

type Environment = ([RContext], RContext)

type RContext = [(Ident, Type)]

data R a = OK a | Erro String
    deriving (Eq, Ord, Show, Read)

data Valor
    = ValorInt
        { i :: Integer
        }
    | ValorFun
        { f :: Function
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
    deriving (Show)

typeCheckP :: Program -> Valor -> [R Environment]
typeCheckP (Prog fs) _ =
    let environment = updatecF ([], []) fs
     in case environment of
            OK env -> map (typeCheckF env) fs
            Erro msg -> [Erro msg]

typeCheckF :: Environment -> Function -> R Environment
typeCheckF environment (Fun (Dec idF tps) id decl exp) =
    let r = tke newEnvironment exp tR
        in case r of
            OK env -> tke env exp tR
            Erro msg -> Erro msg
    where
        tR = last tps
        typeBindings = (idF, last tps) : zip decl (init tps)
        newEnvironment = pushB typeBindings environment

combChecks :: Environment -> Exp -> Exp -> Type -> R Type
combChecks environment exp1 exp2 tp =
    let r = tke environment exp1 tp
     in case r of
            OK _ ->
                let r2 = tke environment exp2 tp
                 in case r2 of
                        OK _ -> OK tp
                        Erro msg -> Erro msg
            Erro msg -> Erro msg

tke :: Environment -> Exp -> Type -> R Environment
tke environment exp tp =
    let r = tinf environment exp
     in case r of
            OK tipo ->
                if tipo == tp
                    then OK environment
                    else
                        Erro
                            ( " A expressao "
                                ++ printTree exp
                                ++ " tem tipo "
                                ++ printTree tipo
                                ++ " mas o tipo esperado eh "
                                ++ printTree tp
                            )
            Erro msg -> Erro msg

tinf :: Environment -> Exp -> R Type
tinf environment x = case x of
    ECon exp0 exp -> combChecks environment exp0 exp TStr
    EAdd exp0 exp -> combChecks environment exp0 exp Tint
    ESub exp0 exp -> combChecks environment exp0 exp Tint
    EMul exp0 exp -> combChecks environment exp0 exp Tint
    EDiv exp0 exp -> combChecks environment exp0 exp Tint
    EOr exp0 exp -> combChecks environment exp0 exp Tbool
    EAnd exp0 exp -> combChecks environment exp0 exp Tbool
    ENot exp ->
        let r = tke environment exp Tbool
         in case r of
                OK _ -> OK Tbool
                Erro msg -> Erro msg
    EStr str -> OK TStr
    ETrue -> OK Tbool
    EFalse -> OK Tbool
    EInt n -> OK Tint
    EVar id -> lookupDeepType environment id
    EIf exp0 exp exp2 ->
        let r = tke environment exp0 Tint
         in case r of
                OK _ ->
                    let r2 = tinf environment exp
                     in case r2 of
                            OK _ -> 
                                let r3 = tinf environment exp2
                                 in case r3 of
                                        OK t -> OK t
                                        Erro msg -> Erro msg
                            Erro msg -> Erro msg
                Erro msg -> Erro msg
    EPair exp0 exp ->
        let p = (tinf environment exp0, tinf environment exp)
            in case p of
                (OK t1, OK t2) -> OK (TPair t1 t2)
                (Erro msg, _) -> Erro msg
                (_, Erro msg) -> Erro msg
    EList exps -> case exps of
        [] -> OK (TList Tint)
        (exp : exps) ->
            let r = tinf environment exp
             in case r of
                    OK t ->
                        let r2 = tinf environment (EList exps)
                         in case r2 of
                                OK (TList t2) ->
                                    if t == t2
                                        then OK (TList t)
                                        else Erro "lista com tipos diferentes"
                                Erro msg -> Erro msg
                    Erro msg -> Erro msg
    Call id lexp -> case id of
        Ident "fst" -> f
        Ident "snd" -> s
        Ident "isNil" -> OK Tint
        Ident "head" -> h
        Ident "tail" -> t
        _ -> let r = lookupShallowFunction environment id
            in case r of
                OK (TFun (Fun (Dec idF tps) _ decls stms)) ->
                    if length decls == length lexp
                        then
                            if isThereError tksArgs /= []
                                then Erro "chamada de funcao invalida"
                                else OK tR
                        else Erro " tamanhos diferentes de lista de argumentos e parametros"
                  where
                    tR = last tps
                    parameterTypes = init tps
                    tksArgs = zipWith (tke environment) lexp parameterTypes
                    isThereError l =
                        filter
                            not
                            ( map
                                ( \e ->
                                    ( let r2 = e
                                       in case r2 of
                                            OK _ -> True
                                            Erro _ -> False
                                    )
                                )
                                l
                            )
                Erro msg -> Erro msg
        where
            tP = tinf environment (head lexp)
            h = case tP of
                OK (TList t) -> OK t
                OK _ -> Erro "head espera uma lista"
                Erro msg -> Erro msg
            t = case tP of
                OK (TList t) -> OK (TList t)
                OK _ -> Erro "tail espera uma lista"
                Erro msg -> Erro msg
            f = case tP of
                OK (TPair t1 _) -> OK t1
                OK _ -> Erro "fst espera um par"
                Erro msg -> Erro msg
            s = case tP of
                OK (TPair _ t2) -> OK t2
                OK _ -> Erro "fst espera um par"
                Erro msg -> Erro msg

updatecF :: Environment -> [Function] -> R Environment
updatecF e [] = OK e
updatecF (sc, c) (f@(Fun tR id params stms) : fs) =
    let r = updateShallow c id (TFun f)
     in case r of
            OK ctx -> updatecF (sc, ctx) fs
            Erro msg -> Erro msg

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc, fnCtx) = (typeBindings : sc, fnCtx)

push :: Environment -> Environment
push (sc, fnCtx) = ([] : sc, fnCtx)

pop :: Environment -> Environment
pop (s : scs, fnCtx) = (scs, fnCtx)

updateShallow :: RContext -> Ident -> Type -> R RContext
updateShallow context id tp =
    if id `elem` map fst context
        then Erro "tipo ja definido no contexto de tipos"
        else OK ((id, tp) : context)

lookupDeepType :: Environment -> Ident -> R Type
lookupDeepType ([], fnCtx) id = Erro (printTree id ++ " nao esta no contexto. ")
lookupDeepType (s : scs, fnCtx) id =
    let r = lookupShallow s id
     in case r of
            OK tp -> OK tp
            Erro _ -> lookupDeepType (scs, fnCtx) id

lookupShallowFunction :: Environment -> Ident -> R Type
lookupShallowFunction (sc, fnCtx) = lookupShallow fnCtx

lookupShallow :: RContext -> Ident -> R Type
lookupShallow [] s = Erro (printTree s ++ " nao esta no contexto. ")
lookupShallow ((i, v) : cs) s
    | i == s = OK v
    | otherwise = lookupShallow cs s