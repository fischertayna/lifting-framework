import Test.HUnit

import BaseTest
import VarTest
import MemoTest
import SLMemoTest
import DeepMemoTest
import BaseDFATest
import DeepMemoDFATest
import VarDFATest
import VMemoDFATest
import EncodingTest

main :: IO ()
main = do
    putStrLn "Running tests..."
    runTestTT baseTestSuite
    runTestTT varTestSuite
    runTestTT memoTestSuite
    runTestTT slMemoTestSuite
    runTestTT baseDFATestSuite
    runTestTT varDFATestSuite
    runTestTT vMemoDFATestSuite
    runTestTT deepMemoDFATestSuite
    runTestTT encodingTestSuite
    putStrLn "Done."