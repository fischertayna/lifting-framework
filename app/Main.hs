module Main where

import qualified Language.MInterpreter.Driver as Memo
import qualified Language.VInterpreter.Driver as Var
import qualified Language.Interpreter.Driver as Base
import qualified Language.Typechecker.Driver as Typecheck

main :: IO ()
main = Memo.main
