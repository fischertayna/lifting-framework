module Main where

import qualified Language.MInterpreter.SLDriver as SL
import qualified Language.MInterpreter.Driver as Memo
import qualified Language.VInterpreter.Driver as Var
import qualified Language.Interpreter.Driver as Base

main :: IO ()
main = SL.main
