import Test.HUnit

import BaseTest
import VarTest
import MemoTest
import SLMemoTest
import DeepMemoTest

main :: IO ()
main = do
    putStrLn "Running tests..."
    runTestTT baseTestSuite
    runTestTT varTestSuite
    runTestTT memoTestSuite
    runTestTT slMemoTestSuite
    runTestTT deepMemoTestSuite
    putStrLn "Done."