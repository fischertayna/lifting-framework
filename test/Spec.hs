import Test.HUnit

import BaseTest
import VarTest
import MemoTest
import BaseDFATest
import VarDFATest
import VMemoDFATest
import EncodingTest
import ReadTest

main :: IO ()
main = do
    putStrLn "Running tests..."
    runTestTT test_serialization
    runTestTT baseTestSuite
    runTestTT varTestSuite
    runTestTT memoTestSuite
    runTestTT baseDFATestSuite
    runTestTT varDFATestSuite
    runTestTT vMemoDFATestSuite
    runTestTT encodingTestSuite
    putStrLn "Done."