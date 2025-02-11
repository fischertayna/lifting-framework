module EncodingTest where

import Variability.VarTypes (VarValor(..), substitute)
import WhileExamples 
  ( lvExample, 
    aeExample, 
    vbExample, 
    factorial, 
    power, 
    ex2While
  )
import VarExamples 
  ( expectedLvExample, 
    expectedAeExample, 
    expectedVbExample, 
    expectedFactorial, 
    expectedPower, 
    ex2
  )
import WhileLang.WhileEncoder (encodeStmt)
import Test.HUnit

lvExampleEncoded :: VarValor
lvExampleEncoded = encodeStmt lvExample

aeExampleEncoded :: VarValor
aeExampleEncoded = encodeStmt aeExample

vbExampleEncoded :: VarValor
vbExampleEncoded = encodeStmt vbExample

factorialEncoded :: VarValor
factorialEncoded = encodeStmt factorial

powerEncoded :: VarValor
powerEncoded = encodeStmt power

ex2Encoded :: VarValor
ex2Encoded = encodeStmt ex2While

testLvExample :: Test
testLvExample = TestCase $ do
    -- putStrLn ("\n expected lvExample: " ++ (substitute (show expectedLvExample)))
    -- putStrLn ("\n\n out lvExample: " ++ (substitute (show lvExampleEncoded)))
    assertEqual "Encoding lvExample" expectedLvExample lvExampleEncoded

testAeExample :: Test
testAeExample = TestCase $ do
    -- putStrLn ("\n expected aeExample: " ++ (substitute (show expectedAeExample)))
    -- putStrLn ("\n\n out aeExample: " ++ (substitute (show aeExampleEncoded)))
    assertEqual "Encoding aeExample" expectedAeExample aeExampleEncoded

testVbExample :: Test
testVbExample = TestCase $ do
    -- putStrLn ("\n expected vbExample: " ++ (substitute (show expectedVbExample)))
    -- putStrLn ("\n\n out vbExample: " ++ (substitute (show vbExampleEncoded)))
    assertEqual "Encoding vbExample" expectedVbExample vbExampleEncoded

testFactorial :: Test
testFactorial = TestCase $ do
    -- putStrLn ("\n expected factorial: " ++ (substitute (show expectedFactorial)))
    -- putStrLn ("\n\n out factorial: " ++ (substitute (show factorialEncoded)))
    assertEqual "Encoding factorial" expectedFactorial factorialEncoded

testPower :: Test
testPower = TestCase $ do
    -- putStrLn ("\n expected power: " ++ (substitute (show expectedPower)))
    -- putStrLn ("\n\n out power: " ++ (substitute (show powerEncoded)))
    assertEqual "Encoding power" expectedPower powerEncoded

testEx2 :: Test
testEx2 = TestCase $ do
    -- putStrLn ("\n expected Ex2: " ++ (substitute (show ex2)))
    -- putStrLn ("\n\n out Ex2: " ++ (substitute (show ex2Encoded)))
    assertEqual "Encoding Ex2" ex2 ex2Encoded

encodingTestSuite :: Test
encodingTestSuite = TestList [   
                            TestLabel "lvExample" testLvExample
                        ,   TestLabel "aeExample" testAeExample
                        ,   TestLabel "vbExample" testVbExample
                        ,   TestLabel "factorial" testFactorial
                        ,   TestLabel "power" testPower
                        ,   TestLabel "ex2" testEx2
                        ]