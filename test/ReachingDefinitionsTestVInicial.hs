module ReachingDefinitionsTestVInicial where

import Language.Interpreter.Interpreter
import Language.Interpreter.Driver

import Language.Frontend.ErrM
import Helper

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)

ex1, ex2, ex3, ex4, factorialProg :: Valor

-- x = 1;
-- y = 2;
-- z = x + y;
ex1 = ValorPair ( 
        ValorPair (
            ValorStr "ASGN",
            ValorPair (
                ValorStr "x", 
                ValorPair (
                    ValorStr "INT",
                    ValorStr "1"
                )
            )
        ), 
        ValorPair ( 
            ValorPair (
                ValorStr "ASGN", 
                ValorPair (
                    ValorStr "y", 
                    ValorPair (
                        ValorStr "INT", 
                        ValorStr "2"
                    )
                )
            ), 
            ValorPair (
                ValorStr "ASGN", 
                ValorPair (
                    ValorStr "z", 
                    ValorPair (
                        ValorStr "ADD", 
                        ValorPair (
                            ValorPair (
                                ValorStr "ID", 
                                ValorStr "x"
                            ),
                            ValorPair (
                                ValorStr "ID", 
                                ValorStr "y"
                            )
                        )
                    )
                )
            )
        )
    )

-- x = 1;
ex2 = ValorPair ( 
        ValorPair (
            ValorStr "ASGN", 
            ValorPair (
                ValorStr "x", 
                ValorPair (
                    ValorStr "INT", 
                    ValorStr "1"
                )
            )
        ), 
        ValorPair (
            ValorStr "NIL", 
            ValorStr "NIL"
        )
    )

-- x = 1;
-- soma = 0;
-- c = 10;
-- while (c) {
--     soma = soma + c;
--     c = c - 1; 
-- }
ex3 = ValorPair ( 
        ValorPair (
            ValorStr "ASGN", 
            ValorPair (
                ValorStr "x", 
                ValorPair (
                    ValorStr "INT", 
                    ValorStr "1"
                )
            )
        ), 
        ValorPair (
            ValorPair (
                ValorStr "ASGN", 
                ValorPair (
                    ValorStr "SOMA", 
                    ValorPair (
                        ValorStr "INT", 
                        ValorStr "0"
                    )
                )
            ),
            ValorPair (
                ValorPair (
                    ValorStr "ASGN", 
                    ValorPair (
                        ValorStr "c", 
                        ValorPair (
                            ValorStr "INT", 
                            ValorStr "10"
                        )
                    )
                ),
                ValorPair (
                    ValorStr "WHILE", 
                    ValorPair ( 
                        ValorPair (
                            ValorStr "ID", 
                            ValorStr "c"
                        ),
                        ValorPair (
                            ValorPair (
                                ValorStr "ASGN", 
                                ValorPair (
                                    ValorStr "SOMA", 
                                    ValorPair (
                                        ValorStr "ADD", 
                                        ValorPair (
                                            ValorPair (
                                                ValorStr "ID", 
                                                ValorStr "SOMA"
                                            ),
                                            ValorPair (
                                                ValorStr "ID",
                                                ValorStr "c"
                                            )
                                        )
                                    )
                                )
                            ),
                            ValorPair (
                                ValorStr "ASGN", 
                                ValorPair (
                                    ValorStr "c", 
                                    ValorPair (
                                        ValorStr "SUB", 
                                        ValorPair (
                                            ValorPair (
                                                ValorStr "ID",
                                                ValorStr "c"
                                            ),
                                            ValorPair (
                                                ValorStr "INT",
                                                ValorStr "1"
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

-- x = 1;            1
-- soma = 0;         2
-- c = 10;           3
-- if (!soma)        4
--   then
--     soma = c + x; 5 
--   else
--     soma = 0;     6
ex4 = ValorPair ( 
        ValorPair (
            ValorStr "ASGN", 
            ValorPair (
                ValorStr "x", 
                ValorPair (
                    ValorStr "INT", 
                    ValorStr "1"
                )
            )
        ), 
        ValorPair (
            ValorPair (
                ValorStr "ASGN", 
                ValorPair (
                    ValorStr "SOMA", 
                    ValorPair (
                        ValorStr "INT", 
                        ValorStr "0"
                    )
                )
            ),
            ValorPair (
                ValorPair (
                    ValorStr "ASGN", 
                    ValorPair (
                        ValorStr "C", 
                        ValorPair (
                            ValorStr "INT", 
                            ValorStr "10"
                        )
                    )
                ),
                ValorPair (
                    ValorStr "IF", 
                    ValorPair ( 
                        ValorPair (
                            ValorStr "NOT",
                            ValorPair (
                                ValorStr "ID", 
                                ValorStr "SOMA"
                            )
                        ),
                        ValorPair (
                            ValorPair (
                                ValorStr "ASGN", 
                                ValorPair (
                                    ValorStr "SOMA", 
                                    ValorPair (
                                        ValorStr "ADD", 
                                        ValorPair (
                                            ValorPair (
                                                ValorStr "ID", 
                                                ValorStr "C"
                                            ),
                                            ValorPair (
                                                ValorStr "ID", 
                                                ValorStr "X"
                                            )
                                        )
                                    )
                                )
                            ),
                            ValorPair (
                                ValorStr "ASGN", 
                                ValorPair (
                                    ValorStr "SOMA", 
                                    ValorPair (
                                        ValorStr "INT", 
                                        ValorStr "0"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

-- s01 = Assignment "y" (Var "x") 1
-- s02 = Assignment "z" (Const 1) 2

-- whileTeste = (GTExp (Var "y") (Const 1), 3)
-- whileS1 = Assignment "z" (Mult (Var "z") (Var "y")) 4
-- whileS2 = Assignment "y" (Sub (Var "y") (Const 1)) 5
-- s03 = While whileTeste (Seq whileS1 whileS2)

-- s04 = Assignment "y" (Const 0) 6
factorialProg =  ValorPair (
                    ValorPair (
                        ValorStr "ASGN", 
                        ValorPair (
                            ValorStr "y", 
                            ValorPair (
                                ValorStr "ID", 
                                ValorStr "x"
                            )
                        )
                    ),
                    ValorPair (
                        ValorPair (
                            ValorStr "ASGN", 
                            ValorPair (
                                ValorStr "z", 
                                ValorPair (
                                    ValorStr "INT", 
                                    ValorStr "1"
                                )
                            )
                        ), 
                        ValorPair(
                            ValorPair (
                                ValorStr "WHILE", 
                                ValorPair ( 
                                    ValorPair (
                                        ValorStr "ID", 
                                        ValorStr "y"
                                    ),
                                    ValorPair (
                                        ValorPair (
                                            ValorStr "ASGN", 
                                            ValorPair (
                                                ValorStr "z", 
                                                ValorPair (
                                                    ValorStr "MULT", 
                                                    ValorPair (
                                                        ValorPair (
                                                            ValorStr "ID", 
                                                            ValorStr "z"
                                                        ),
                                                        ValorPair (
                                                            ValorStr "ID",
                                                            ValorStr "y"
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        ValorPair (
                                            ValorStr "ASGN", 
                                            ValorPair (
                                                ValorStr "y", 
                                                ValorPair (
                                                    ValorStr "SUB", 
                                                    ValorPair (
                                                        ValorPair (
                                                            ValorStr "ID",
                                                            ValorStr "y"
                                                        ),
                                                        ValorPair (
                                                            ValorStr "INT",
                                                            ValorStr "1"
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            ValorPair (
                                ValorStr "ASGN", 
                                ValorPair (
                                    ValorStr "y", 
                                    ValorPair (
                                        ValorStr "INT",
                                        ValorStr "0"
                                    )
                                )
                            )
                        )
                    )
                )


testIsPair :: Test
testIsPair = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isPair.lng" ex1
    let expectedOutput = (ValorBool True)
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual :: Test
testIsEqual = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isEqual.lng" (ValorPair (
                                                        ValorInt 1,
                                                        ValorInt 1
                                                    ))
    let expectedOutput = (ValorBool True)
    assertEqual "is ex1 pair" expectedOutput output


testCountEx1 :: Test
testCountEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex1
    let expectedOutput = (ValorInt 3)
    assertEqual "Count Asgns ex1" expectedOutput output

testCountEx2 :: Test
testCountEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex2
    let expectedOutput = (ValorInt 1)
    assertEqual "Count Asgns ex2" expectedOutput output


testCountEx3 :: Test
testCountEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex3
    let expectedOutput = (ValorInt 5)
    assertEqual "Count Asgns ex3" expectedOutput output

testCountEx4 :: Test
testCountEx4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex4
    let expectedOutput = (ValorInt 5)
    assertEqual "Count Asgns ex4" expectedOutput output

testCountFactorial :: Test
testCountFactorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" factorialProg
    let expectedOutput = (ValorInt 5)
    assertEqual "Count Asgns Factorial" expectedOutput output

testChaoticIteration :: Test
testChaoticIteration = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/chaoticIteration.lng" (ValorInt 4)
    let expectedOutput = (ValorInt 4)
    assertEqual "chaotic iteration test" expectedOutput output

rdTestSuiteVInical :: Test
rdTestSuiteVInical = TestList [    TestLabel "is pair" testIsPair
                        ,   TestLabel "is Equal" testIsEqual
                        ,   TestLabel "Count Asgns ex1" testCountEx1
                        ,   TestLabel "Count Asgns ex2" testCountEx2
                        ,   TestLabel "Count Asgns ex3" testCountEx3
                        ,   TestLabel "Count Asgns ex4" testCountEx4
                        ,   TestLabel "Count Asgns factorial" testCountFactorial
                        -- ,   TestLabel "Chaotic Iteration" testChaoticIteration
                        ]