import Test.HUnit

import BaseTest
import VarTest
import MemoTest
import BaseDFATest
import VarDFATest
import VMemoDFATest
import EncodingTest

main :: IO ()
main = do
    putStrLn "Running tests..."
    runTestTT baseTestSuite
    runTestTT varTestSuite
    runTestTT memoTestSuite
    runTestTT baseDFATestSuite
    runTestTT varDFATestSuite
    runTestTT vMemoDFATestSuite
    runTestTT encodingTestSuite
    putStrLn "Done."