import Test.HUnit

import BaseTest
import VarTest
import MemoTest
import SLMemoTest
import DeepMemoTest
import ReachingDefinitionsTest
import DeepMemoReachingDefinitionsTest
import VarReachingDefinitionsTest
import VMemoReachingDefinitionsTest

main :: IO ()
main = do
    putStrLn "Running tests..."
    -- runTestTT baseTestSuite
    -- runTestTT varTestSuite
    -- runTestTT memoTestSuite
    -- runTestTT slMemoTestSuite
    runTestTT rdTestSuite
    runTestTT varRdTestSuite
    runTestTT vMemoRdTestSuite
    runTestTT deepMemoRdTestSuite
    putStrLn "Done."