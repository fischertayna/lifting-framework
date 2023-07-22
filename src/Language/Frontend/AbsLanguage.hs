-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Language.

module Language.Frontend.AbsLanguage where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Prog [Function]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl = Dec Ident [Type]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Function = Fun Decl Ident [Ident] Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EIf Exp Exp Exp
    | EOr Exp Exp
    | EAnd Exp Exp
    | ENot Exp
    | ECon Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | Call Ident [Exp]
    | EInt Integer
    | EVar Ident
    | EStr String
    | EPair Exp Exp
    | EList [Exp]
    | ETrue
    | EFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
    = Tbool
    | Tint
    | TStr
    | TFun Function
    | TPair Type Type
    | TList Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

