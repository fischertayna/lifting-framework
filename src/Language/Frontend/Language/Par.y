-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Frontend.Language.Par
  ( happyError
  , myLexer
  , pProgram
  , pFunction
  , pListFunction
  , pListIdent
  , pListExp
  , pExp1
  , pExp2
  , pExp3
  , pExp4
  , pExp
  ) where

import Prelude

import qualified Language.Frontend.Language.Abs
import Language.Frontend.Language.Lex

}

%name pProgram Program
%name pFunction Function
%name pListFunction ListFunction
%name pListIdent ListIdent
%name pListExp ListExp
%name pExp1 Exp1
%name pExp2 Exp2
%name pExp3 Exp3
%name pExp4 Exp4
%name pExp Exp
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('      { PT _ (TS _ 1)  }
  ')'      { PT _ (TS _ 2)  }
  '*'      { PT _ (TS _ 3)  }
  '+'      { PT _ (TS _ 4)  }
  ','      { PT _ (TS _ 5)  }
  '-'      { PT _ (TS _ 6)  }
  '/'      { PT _ (TS _ 7)  }
  'else'   { PT _ (TS _ 8)  }
  'if'     { PT _ (TS _ 9)  }
  'then'   { PT _ (TS _ 10) }
  '{'      { PT _ (TS _ 11) }
  '}'      { PT _ (TS _ 12) }
  L_Ident  { PT _ (TV $$)   }
  L_integ  { PT _ (TI $$)   }

%%

Ident :: { Language.Frontend.Language.Abs.Ident }
Ident  : L_Ident { Language.Frontend.Language.Abs.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

Program :: { Language.Frontend.Language.Abs.Program }
Program : ListFunction { Language.Frontend.Language.Abs.Prog $1 }

Function :: { Language.Frontend.Language.Abs.Function }
Function
  : Ident '(' ListIdent ')' '{' Exp '}' { Language.Frontend.Language.Abs.Fun $1 $3 $6 }

ListFunction :: { [Language.Frontend.Language.Abs.Function] }
ListFunction
  : {- empty -} { [] } | Function ListFunction { (:) $1 $2 }

ListIdent :: { [Language.Frontend.Language.Abs.Ident] }
ListIdent
  : {- empty -} { [] }
  | Ident { (:[]) $1 }
  | Ident ',' ListIdent { (:) $1 $3 }

ListExp :: { [Language.Frontend.Language.Abs.Exp] }
ListExp
  : {- empty -} { [] }
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }

Exp1 :: { Language.Frontend.Language.Abs.Exp }
Exp1
  : 'if' '(' Exp1 ')' 'then' Exp1 'else' Exp1 { Language.Frontend.Language.Abs.EIf $3 $6 $8 }
  | Exp1 '+' Exp2 { Language.Frontend.Language.Abs.EAdd $1 $3 }
  | Exp1 '-' Exp2 { Language.Frontend.Language.Abs.ESub $1 $3 }
  | Exp2 { $1 }

Exp2 :: { Language.Frontend.Language.Abs.Exp }
Exp2
  : Exp2 '*' Exp3 { Language.Frontend.Language.Abs.EMul $1 $3 }
  | Exp2 '/' Exp3 { Language.Frontend.Language.Abs.EDiv $1 $3 }
  | Exp3 { $1 }

Exp3 :: { Language.Frontend.Language.Abs.Exp }
Exp3
  : Ident '(' ListExp ')' { Language.Frontend.Language.Abs.Call $1 $3 }
  | Exp4 { $1 }

Exp4 :: { Language.Frontend.Language.Abs.Exp }
Exp4
  : Integer { Language.Frontend.Language.Abs.EInt $1 }
  | Ident { Language.Frontend.Language.Abs.EVar $1 }
  | '(' Exp ')' { $2 }

Exp :: { Language.Frontend.Language.Abs.Exp }
Exp : Exp1 { $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

