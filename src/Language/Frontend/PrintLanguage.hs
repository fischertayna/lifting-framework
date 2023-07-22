-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Language.

module Language.Frontend.PrintLanguage where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Language.Frontend.AbsLanguage

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Language.Frontend.AbsLanguage.Ident where
  prt _ (Language.Frontend.AbsLanguage.Ident i) = doc $ showString i
instance Print Language.Frontend.AbsLanguage.Program where
  prt i = \case
    Language.Frontend.AbsLanguage.Prog functions -> prPrec i 0 (concatD [prt 0 functions])

instance Print Language.Frontend.AbsLanguage.Decl where
  prt i = \case
    Language.Frontend.AbsLanguage.Dec id_ types -> prPrec i 0 (concatD [prt 0 id_, doc (showString "::"), prt 0 types])

instance Print Language.Frontend.AbsLanguage.Function where
  prt i = \case
    Language.Frontend.AbsLanguage.Fun decl id_ ids exp -> prPrec i 0 (concatD [prt 0 decl, prt 0 id_, doc (showString "("), prt 0 ids, doc (showString ")"), doc (showString "{"), prt 0 exp, doc (showString "}")])

instance Print [Language.Frontend.AbsLanguage.Function] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Language.Frontend.AbsLanguage.Type] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "->"), prt 0 xs]

instance Print [Language.Frontend.AbsLanguage.Ident] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Language.Frontend.AbsLanguage.Exp] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Frontend.AbsLanguage.Exp where
  prt i = \case
    Language.Frontend.AbsLanguage.EIf exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp1, doc (showString ")"), doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    Language.Frontend.AbsLanguage.EOr exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])
    Language.Frontend.AbsLanguage.EAnd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "&&"), prt 3 exp2])
    Language.Frontend.AbsLanguage.ENot exp -> prPrec i 3 (concatD [doc (showString "!"), prt 3 exp])
    Language.Frontend.AbsLanguage.ECon exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "++"), prt 5 exp2])
    Language.Frontend.AbsLanguage.EAdd exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "+"), prt 5 exp2])
    Language.Frontend.AbsLanguage.ESub exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "-"), prt 5 exp2])
    Language.Frontend.AbsLanguage.EMul exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "*"), prt 6 exp2])
    Language.Frontend.AbsLanguage.EDiv exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "/"), prt 6 exp2])
    Language.Frontend.AbsLanguage.Call id_ exps -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exps, doc (showString ")")])
    Language.Frontend.AbsLanguage.EInt n -> prPrec i 7 (concatD [prt 0 n])
    Language.Frontend.AbsLanguage.EVar id_ -> prPrec i 7 (concatD [prt 0 id_])
    Language.Frontend.AbsLanguage.EStr str -> prPrec i 7 (concatD [printString str])
    Language.Frontend.AbsLanguage.EPair exp1 exp2 -> prPrec i 7 (concatD [doc (showString "("), prt 0 exp1, doc (showString ","), prt 0 exp2, doc (showString ")")])
    Language.Frontend.AbsLanguage.EList exps -> prPrec i 7 (concatD [doc (showString "["), prt 0 exps, doc (showString "]")])
    Language.Frontend.AbsLanguage.ETrue -> prPrec i 7 (concatD [doc (showString "True")])
    Language.Frontend.AbsLanguage.EFalse -> prPrec i 7 (concatD [doc (showString "False")])

instance Print Language.Frontend.AbsLanguage.Type where
  prt i = \case
    Language.Frontend.AbsLanguage.Tbool -> prPrec i 0 (concatD [doc (showString "bool")])
    Language.Frontend.AbsLanguage.Tint -> prPrec i 0 (concatD [doc (showString "int")])
    Language.Frontend.AbsLanguage.TStr -> prPrec i 0 (concatD [doc (showString "String")])
    Language.Frontend.AbsLanguage.TFun function -> prPrec i 0 (concatD [prt 0 function])
    Language.Frontend.AbsLanguage.TPair type_1 type_2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 type_1, doc (showString ","), prt 0 type_2, doc (showString ")")])
    Language.Frontend.AbsLanguage.TList type_ -> prPrec i 0 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])
