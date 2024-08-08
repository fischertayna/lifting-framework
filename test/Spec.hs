import Test.HUnit

import BaseTest
import VarTest
import MemoTest
import SLMemoTest
import DeepMemoTest
import ReachingDefinitionsTest
import ReachingDefinitionsTestVInicial

main :: IO ()
main = do
    putStrLn "Running tests..."
    -- runTestTT baseTestSuite
    -- runTestTT varTestSuite
    -- runTestTT memoTestSuite
    -- runTestTT slMemoTestSuite
    runTestTT rdTestSuite
    runTestTT rdTestSuiteVInical
    putStrLn "Done."