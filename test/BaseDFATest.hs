module BaseDFATest where

import Language.Interpreter.Interpreter
import Language.Interpreter.Driver

import Language.Frontend.ErrM
import Helper

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)
import Valor (Valor(..))

ex1, ex2, ex3, ex4, factorialProg :: Valor

-- x = 1;      1
-- y = 2;      2
-- z = x + y;  3
ex1 = ValorPair(
        ValorStr "SEQ",
        ValorPair ( 
            ValorPair (
                ValorStr "ASGN",
                ValorPair (
                    ValorStr "1",
                    ValorPair (
                        ValorStr "x",
                        ValorPair(
                            ValorStr "CONST", 
                            ValorStr "1"
                        )
                    )
                )
            ),
            ValorPair(
                ValorStr "SEQ",
                ValorPair ( 
                    ValorPair (
                        ValorStr "ASGN",
                        ValorPair (
                            ValorStr "2", 
                            ValorPair (
                                ValorStr "y",
                                ValorPair(
                                    ValorStr "CONST", 
                                    ValorStr "2"
                                )
                            )
                        )
                    ), 
                    ValorPair(
                        ValorStr "ASGN",
                        ValorPair (
                            ValorStr "3", 
                            ValorPair (
                                ValorStr "z", 
                                ValorPair (
                                    ValorStr "ADD", 
                                    ValorPair (
                                        ValorPair (
                                            ValorStr "VAR", 
                                            ValorStr "x"
                                        ),
                                        ValorPair (
                                            ValorStr "VAR", 
                                            ValorStr "y"
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
    

-- x = 1;  1
ex2 = ValorPair(
        ValorStr "ASGN",
        ValorPair (
            ValorStr "1", 
            ValorPair (
                ValorStr "x",
                ValorPair(
                    ValorStr "CONST", 
                    ValorStr "1"
                )
            )
        ) 
    )

-- x = 1;                   1
-- soma = 0;                2
-- c = 10;                  3
-- while (c) {              4
--     soma = soma + c;     5
--     c = c - 1;           6
-- }
ex3 = ValorPair(
        ValorStr "SEQ",
        ValorPair ( 
            ValorPair(
                ValorStr "ASGN",
                ValorPair (
                    ValorStr "1", 
                    ValorPair (
                        ValorStr "x", 
                        ValorPair(
                            ValorStr "CONST", 
                            ValorStr "1"
                        )
                    )
                )
            ), 
            ValorPair (
                ValorStr "SEQ",
                ValorPair (
                    ValorPair(
                        ValorStr "ASGN",
                        ValorPair (
                            ValorStr "2", 
                            ValorPair (
                                ValorStr "soma", 
                                ValorPair(
                                    ValorStr "CONST", 
                                    ValorInt 0
                                )
                            )
                        )
                    ),
                    ValorPair (
                        ValorStr "SEQ",
                        ValorPair (
                            ValorPair(
                                ValorStr "ASGN",
                                ValorPair (
                                    ValorStr "3", 
                                    ValorPair (
                                        ValorStr "c", 
                                        ValorPair(
                                            ValorStr "CONST", 
                                            ValorStr "10"
                                        )
                                    )
                                )
                            ),
                            ValorPair(
                                ValorStr "WHILE",
                                ValorPair (
                                    ValorStr "4",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "VAR", 
                                            ValorStr "c"
                                        ),
                                        ValorPair(
                                            ValorStr "SEQ",
                                            ValorPair (
                                                ValorPair(
                                                    ValorStr "ASGN",
                                                    ValorPair (
                                                        ValorStr "5", 
                                                        ValorPair (
                                                            ValorStr "soma", 
                                                            ValorPair (
                                                                ValorStr "ADD", 
                                                                ValorPair (
                                                                    ValorPair (
                                                                        ValorStr "VAR", 
                                                                        ValorStr "soma"
                                                                    ),
                                                                    ValorPair (
                                                                        ValorStr "VAR",
                                                                        ValorStr "c"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                ),
                                                ValorPair(
                                                    ValorStr "ASGN",
                                                    ValorPair (
                                                        ValorStr "6", 
                                                        ValorPair (
                                                            ValorStr "c", 
                                                            ValorPair (
                                                                ValorStr "SUB", 
                                                                ValorPair (
                                                                    ValorPair (
                                                                        ValorStr "VAR",
                                                                        ValorStr "c"
                                                                    ),
                                                                    ValorPair(
                                                                        ValorStr "CONST", 
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
        ValorStr "SEQ",
        ValorPair(
            ValorPair(
                ValorStr "ASGN",
                ValorPair (
                    ValorStr "1", 
                    ValorPair (
                        ValorStr "x", 
                        ValorPair(
                            ValorStr "CONST", 
                            ValorStr "1"
                        )
                    )
                )
            ),
            ValorPair(
                ValorStr "SEQ",
                ValorPair (
                    ValorPair(
                        ValorStr "ASGN",
                        ValorPair (
                            ValorStr "2", 
                            ValorPair (
                                ValorStr "soma", 
                                ValorPair(
                                    ValorStr "CONST", 
                                    ValorInt 0
                                )
                            )
                        )
                    ),
                    ValorPair(
                        ValorStr "SEQ",
                        ValorPair (
                            ValorPair(
                                ValorStr "ASGN",
                                ValorPair (
                                    ValorStr "3", 
                                    ValorPair (
                                        ValorStr "c", 
                                        ValorPair(
                                            ValorStr "CONST", 
                                            ValorStr "10"
                                        )
                                    )
                                )
                            ),
                            ValorPair(
                                ValorStr "IF",
                                ValorPair ( 
                                    ValorStr "4",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "NOT",
                                            ValorPair (
                                                ValorStr "VAR", 
                                                ValorStr "soma"
                                            )
                                        ),
                                        ValorPair (
                                            ValorPair(
                                                ValorStr "ASGN",
                                                ValorPair (
                                                    ValorStr "5", 
                                                    ValorPair (
                                                        ValorStr "soma", 
                                                        ValorPair (
                                                            ValorStr "ADD", 
                                                            ValorPair (
                                                                ValorPair (
                                                                    ValorStr "VAR", 
                                                                    ValorStr "c"
                                                                ),
                                                                ValorPair (
                                                                    ValorStr "VAR", 
                                                                    ValorStr "x"
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            ),
                                            ValorPair(
                                                ValorStr "ASGN",
                                                ValorPair (
                                                    ValorStr "6", 
                                                    ValorPair (
                                                        ValorStr "soma", 
                                                        ValorPair(
                                                            ValorStr "CONST", 
                                                            ValorInt 0
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
                    ValorStr "SEQ",
                    ValorPair(
                        ValorPair(
                            ValorStr "ASGN",
                            ValorPair (
                                ValorStr "1", 
                                ValorPair (
                                    ValorStr "y", 
                                    ValorPair (
                                        ValorStr "VAR", 
                                        ValorStr "x"
                                    )
                                )
                            )
                        ),
                        ValorPair(
                            ValorStr "SEQ",
                            ValorPair (
                                ValorPair(
                                    ValorStr "ASGN",
                                    ValorPair (
                                        ValorStr "2", 
                                        ValorPair (
                                            ValorStr "z", 
                                            ValorPair(
                                                ValorStr "CONST", 
                                                ValorStr "1"
                                            )
                                        )
                                    )
                                ),
                                ValorPair(
                                    ValorStr "SEQ",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "WHILE", 
                                            ValorPair ( 
                                                ValorStr "3",
                                                ValorPair(
                                                    ValorPair (
                                                        ValorStr "VAR", 
                                                        ValorStr "y"
                                                    ),
                                                    ValorPair (
                                                        ValorStr "SEQ",
                                                        ValorPair(
                                                            ValorPair(
                                                                ValorStr "ASGN",
                                                                ValorPair (
                                                                    ValorStr "4", 
                                                                    ValorPair (
                                                                        ValorStr "z", 
                                                                        ValorPair (
                                                                            ValorStr "MULT", 
                                                                            ValorPair (
                                                                                ValorPair (
                                                                                    ValorStr "VAR", 
                                                                                    ValorStr "z"
                                                                                ),
                                                                                ValorPair (
                                                                                    ValorStr "VAR",
                                                                                    ValorStr "y"
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            ),
                                                            ValorPair(
                                                                ValorStr "ASGN",
                                                                ValorPair (
                                                                    ValorStr "5", 
                                                                    ValorPair (
                                                                        ValorStr "y", 
                                                                        ValorPair (
                                                                            ValorStr "SUB", 
                                                                            ValorPair (
                                                                                ValorPair (
                                                                                    ValorStr "VAR",
                                                                                    ValorStr "y"
                                                                                ),
                                                                                ValorPair(
                                                                                    ValorStr "CONST", 
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
                                        ),
                                        ValorPair(
                                            ValorStr "ASGN",
                                            ValorPair (
                                                ValorStr "6", 
                                                ValorPair (
                                                    ValorStr "y", 
                                                    ValorPair(
                                                        ValorStr "CONST", 
                                                        ValorInt 0
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

-- s01 = Assignment "x" (Const 5) 1
-- s02 = Assignment "y" (Const 1) 2

-- whileTeste = (GTExp (Var "x") (Const 1), 3)
-- whileS1 = Assignment "y" (Mult (Var "x") (Var "y")) 4
-- whileS2 = Assignment "x" (Sub (Var "x") (Const 1)) 5
-- s03 = While whileTeste (Seq whileS1 whileS2)

s01 = ValorPair(
        ValorStr "ASGN",
        ValorPair (
            ValorStr "1", 
            ValorPair (
                ValorStr "x", 
                ValorPair (
                    ValorStr "CONST", 
                    ValorStr "5"
                )
            )
        )
    )

s02 = ValorPair(
        ValorStr "ASGN",
        ValorPair (
            ValorStr "2", 
            ValorPair (
                ValorStr "y", 
                ValorPair(
                    ValorStr "CONST", 
                    ValorStr "1"
                )
            )
        )
    )

whileTeste = ValorPair (
                ValorStr "VAR", 
                ValorStr "x"
            )

whileS1 = ValorPair(
            ValorStr "ASGN",
            ValorPair (
                ValorStr "4", 
                ValorPair (
                    ValorStr "y", 
                    ValorPair (
                        ValorStr "MULT", 
                        ValorPair (
                            ValorPair (
                                ValorStr "VAR", 
                                ValorStr "x"
                            ),
                            ValorPair (
                                ValorStr "VAR",
                                ValorStr "y"
                            )
                        )
                    )
                )
            )
        )

whileS2 = ValorPair(
        ValorStr "ASGN",
        ValorPair (
            ValorStr "5", 
            ValorPair (
                ValorStr "x", 
                ValorPair (
                    ValorStr "SUB", 
                    ValorPair (
                        ValorPair (
                            ValorStr "VAR",
                            ValorStr "x"
                        ),
                        ValorPair(
                            ValorStr "CONST", 
                            ValorStr "1"
                        )
                    )
                )
            )
        )
    )

exPPA =  ValorPair (
                    ValorStr "SEQ",
                    ValorPair(
                        s01,
                        ValorPair(
                            ValorStr "SEQ",
                            ValorPair (
                                s02,
                                ValorPair (
                                    ValorStr "WHILE", 
                                    ValorPair ( 
                                        ValorStr "3",
                                        ValorPair(
                                            whileTeste,
                                            ValorPair (
                                                ValorStr "SEQ",
                                                ValorPair(
                                                    whileS1,
                                                    whileS2
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )


testIsPair :: Test
testIsPair = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isPair.lng" ex1
    let expectedOutput = (ValorBool True)
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual :: Test
testIsEqual = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isEqual.lng" (ValorPair (
                                                        ValorInt 1,
                                                        ValorInt 1
                                                    ))
    let expectedOutput = (ValorBool True)
    assertEqual "is ex1 pair" expectedOutput output


testCountEx1 :: Test
testCountEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/Count-Asgns.lng" ex1
    let expectedOutput = (ValorInt 3)
    assertEqual "Count Asgns ex1" expectedOutput output

testCountEx2 :: Test
testCountEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/Count-Asgns.lng" ex2
    let expectedOutput = (ValorInt 1)
    assertEqual "Count Asgns ex2" expectedOutput output


testCountEx3 :: Test
testCountEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/Count-Asgns.lng" ex3
    let expectedOutput = (ValorInt 5)
    assertEqual "Count Asgns ex3" expectedOutput output

testCountEx4 :: Test
testCountEx4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/Count-Asgns.lng" ex4
    let expectedOutput = (ValorInt 5)
    assertEqual "Count Asgns ex4" expectedOutput output

testCountFactorial :: Test
testCountFactorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/Count-Asgns.lng" factorialProg
    let expectedOutput = (ValorInt 5)
    assertEqual "Count Asgns Factorial" expectedOutput output

testInitEx1 :: Test
testInitEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/init.lng" ex1
    let expectedOutput = (ValorStr "1")
    assertEqual "init ex1" expectedOutput output

testInitExWhile :: Test
testInitExWhile = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/init.lng" (ValorPair (
                                ValorStr "WHILE", 
                                ValorPair ( 
                                    ValorStr "3",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "VAR", 
                                            ValorStr "y"
                                        ),
                                        ValorPair(
                                            ValorStr "4",
                                            ValorPair (
                                                ValorStr "ASGN", 
                                                ValorPair (
                                                    ValorStr "c", 
                                                    ValorPair(
                                                        ValorStr "CONST",
                                                        ValorStr "1"
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ))
    let expectedOutput = (ValorStr "3")
    assertEqual "init while" expectedOutput output

testInitExIF :: Test
testInitExIF = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/init.lng" (ValorPair(
                                ValorStr "IF",
                                ValorPair ( 
                                    ValorStr "4",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "NOT",
                                            ValorPair (
                                                ValorStr "VAR", 
                                                ValorStr "SOMA"
                                            )
                                        ),
                                        ValorPair (
                                            ValorPair(
                                                ValorStr "ASGN",
                                                ValorPair (
                                                    ValorStr "5", 
                                                    ValorPair (
                                                        ValorStr "x", 
                                                        ValorPair (
                                                            ValorStr "CONST",
                                                            ValorStr "3"
                                                        )
                                                    )
                                                )
                                            ),
                                            ValorPair(
                                                ValorStr "ASGN",
                                                ValorPair (
                                                    ValorStr "6", 
                                                    ValorPair (
                                                        ValorStr "SOMA", 
                                                        ValorPair(
                                                            ValorStr "CONST", 
                                                            ValorInt 0
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ))
    let expectedOutput = (ValorStr "4")
    assertEqual "init if" expectedOutput output

testFinalEx1 :: Test
testFinalEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/final.lng" ex1
    let expectedOutput = (ValorList [ValorStr "3"])
    assertEqual "final ex1" expectedOutput output

testFinalExWhile :: Test
testFinalExWhile = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/final.lng" (ValorPair (
                                ValorStr "WHILE", 
                                ValorPair ( 
                                    ValorStr "3",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "VAR", 
                                            ValorStr "y"
                                        ),
                                        ValorPair(
                                            ValorStr "ASGN",
                                            ValorPair (
                                                ValorStr "4", 
                                                ValorPair (
                                                    ValorStr "c", 
                                                    ValorPair(
                                                        ValorStr "CONST",
                                                        ValorStr "1"
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ))
    let expectedOutput = (ValorList [ValorStr "3"])
    assertEqual "final while" expectedOutput output

testFinalExIF :: Test
testFinalExIF = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/final.lng" (ValorPair(
                                ValorStr "IF",
                                ValorPair ( 
                                    ValorStr "4",
                                    ValorPair(
                                        ValorPair (
                                            ValorStr "NOT",
                                            ValorPair (
                                                ValorStr "VAR", 
                                                ValorStr "SOMA"
                                            )
                                        ),
                                        ValorPair (
                                            ValorPair(
                                                ValorStr "ASGN",
                                                ValorPair (
                                                    ValorStr "5", 
                                                    ValorPair (
                                                        ValorStr "x", 
                                                        ValorPair (
                                                            ValorStr "CONST",
                                                            ValorStr "3"
                                                        )
                                                    )
                                                )
                                            ),
                                            ValorPair(
                                                ValorStr "ASGN",
                                                ValorPair (
                                                    ValorStr "6", 
                                                    ValorPair (
                                                        ValorStr "SOMA", 
                                                        ValorPair(
                                                            ValorStr "CONST", 
                                                            ValorInt 0
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ))
    let expectedOutput = (ValorList [ValorStr "5", ValorStr "6"])
    assertEqual "final if" expectedOutput output

testElem :: Test
testElem = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/elem.lng" (ValorPair(
            ValorStr "b",
            ValorList [ValorBool True, ValorStr "b", ValorInt 4]
        ))
    let expectedOutput = (ValorInt 1)
    assertEqual "elem" expectedOutput output

testNotElem :: Test
testNotElem = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/elem.lng" (ValorPair(
            ValorStr "a",
            ValorList [ValorBool True, ValorStr "b", ValorInt 4]
        ))
    let expectedOutput = (ValorInt 0)
    assertEqual "not elem" expectedOutput output

testAddUnique :: Test
testAddUnique = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/addUnique.lng" (ValorPair(
            ValorStr "a",
            ValorList [ValorBool True, ValorStr "b", ValorInt 4]
        ))
    let expectedOutput = (ValorList [ValorBool True, ValorStr "b", ValorInt 4, ValorStr "a"])
    assertEqual "addUnique" expectedOutput output

testAddUniqueSame :: Test
testAddUniqueSame = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/addUnique.lng" (ValorPair(
            ValorStr "b",
            ValorList [ValorBool True, ValorStr "b", ValorInt 4]
        ))
    let expectedOutput = (ValorList [ValorBool True, ValorStr "b", ValorInt 4])
    assertEqual "addUnique same" expectedOutput output

testUnion :: Test
testUnion = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/union.lng" (ValorPair(
            ValorList [ValorStr "a", ValorStr "b", ValorInt 1],
            ValorList [ValorBool True, ValorStr "b", ValorInt 4]
        ))
    let expectedOutput = (ValorList [ValorStr "a", ValorStr "b", ValorStr "1", ValorBool True, ValorStr "4"])
    assertEqual "union" expectedOutput output

testFlowEx1 :: Test
testFlowEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flow.lng" ex1
    let expectedOutput = (ValorList[ValorPair(ValorStr "1", ValorStr "2"), ValorPair(ValorStr "2", ValorStr "3")])
    assertEqual "Flow ex1" expectedOutput output

testFlowEx2 :: Test
testFlowEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flow.lng" ex2
    let expectedOutput = (ValorList[])
    assertEqual "Flow ex2" expectedOutput output


testFlowEx3 :: Test
testFlowEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flow.lng" ex3
    let expectedOutput = (ValorList[
            ValorPair(ValorStr "1", ValorStr "2"), 
            ValorPair(ValorStr "2", ValorStr "3"),
            ValorPair(ValorStr "3", ValorStr "4"),
            ValorPair(ValorStr "6", ValorStr "4"),
            ValorPair(ValorStr "4", ValorStr "5"),
            ValorPair(ValorStr "5", ValorStr "6")])
    assertEqual "Flow ex3" expectedOutput output

testFlowEx4 :: Test
testFlowEx4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flow.lng" ex4
    let expectedOutput = (ValorList[
            ValorPair(ValorStr "1", ValorStr "2"), 
            ValorPair(ValorStr "2", ValorStr "3"),
            ValorPair(ValorStr "3", ValorStr "4"),
            ValorPair(ValorStr "4", ValorStr "6"),
            ValorPair(ValorStr "4", ValorStr "5")])
    assertEqual "Flow ex4" expectedOutput output

testFlowFactorial :: Test
testFlowFactorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flow.lng" factorialProg
    let expectedOutput = (ValorList[
            ValorPair(ValorStr "1", ValorStr "2"), 
            ValorPair(ValorStr "2", ValorStr "3"),
            ValorPair(ValorStr "3", ValorStr "6"),
            ValorPair(ValorStr "5", ValorStr "3"),
            ValorPair(ValorStr "3", ValorStr "4"),
            ValorPair(ValorStr "4", ValorStr "5")])
    assertEqual "Flow Factorial" expectedOutput output

testChaoticIteration1 :: Test
testChaoticIteration1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/chaoticIteration.lng" (ValorPair(ValorInt 1, ValorInt 2))
    let expectedOutput = (ValorInt 3)
    assertEqual "chaotic iteration 1 test" expectedOutput output

testChaoticIteration2 :: Test
testChaoticIteration2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/chaoticIteration.lng" (ValorPair(ValorInt 2, ValorInt 2))
    let expectedOutput = (ValorInt 4)
    assertEqual "chaotic iteration 2 test" expectedOutput output

testAssignmentsEx1 :: Test
testAssignmentsEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/assignments.lng" ex1
    let expectedOutput = ValorList[ValorPair(ValorStr "z", ValorStr "3"),
                                   ValorPair(ValorStr "y", ValorStr "2"),
                                   ValorPair(ValorStr "x", ValorStr "1")]
    assertEqual " Asgns ex1" expectedOutput output

testAssignmentsEx2 :: Test
testAssignmentsEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/assignments.lng" ex2
    let expectedOutput = ValorList[ValorPair(ValorStr "x", ValorStr "1")]
    assertEqual " Asgns ex2" expectedOutput output


testAssignmentsEx3 :: Test
testAssignmentsEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/assignments.lng" ex3
    let expectedOutput = ValorList[ValorPair(ValorStr "c", ValorStr "6"),
                                   ValorPair(ValorStr "soma", ValorStr "5"),
                                   ValorPair(ValorStr "c", ValorStr "3"),
                                   ValorPair(ValorStr "soma", ValorStr "2"),
                                   ValorPair(ValorStr "x", ValorStr "1")]
    assertEqual " Asgns ex3" expectedOutput output

testAssignmentsEx4 :: Test
testAssignmentsEx4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/assignments.lng" ex4
    let expectedOutput = ValorList[ValorPair(ValorStr "soma", ValorStr "6"),
                                   ValorPair(ValorStr "soma", ValorStr "5"),
                                   ValorPair(ValorStr "c", ValorStr "3"),
                                   ValorPair(ValorStr "soma", ValorStr "2"),
                                   ValorPair(ValorStr "x", ValorStr "1")]
    assertEqual " Asgns ex4" expectedOutput output

testAssignmentsFactorial :: Test
testAssignmentsFactorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/assignments.lng" factorialProg
    let expectedOutput = ValorList[ValorPair(ValorStr "y", ValorStr "6"),
                                   ValorPair(ValorStr "y", ValorStr "5"),
                                   ValorPair(ValorStr "z", ValorStr "4"),
                                   ValorPair(ValorStr "z", ValorStr "2"),
                                   ValorPair(ValorStr "y", ValorStr "1")]
    assertEqual " Asgns Factorial" expectedOutput output

testfvEx1 :: Test
testfvEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/fv.lng" ex1
    let expectedOutput = ValorList[ValorStr "x", ValorStr "y", ValorStr "z"]
    assertEqual "fv ex1" expectedOutput output

testfvEx2 :: Test
testfvEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/fv.lng" ex2
    let expectedOutput = ValorList[ValorStr "x"]
    assertEqual "fv ex2" expectedOutput output


testfvEx3 :: Test
testfvEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/fv.lng" ex3
    let expectedOutput = ValorList[ValorStr "c", ValorStr "soma", ValorStr "x"]
    assertEqual "fv ex3" expectedOutput output

testfvEx4 :: Test
testfvEx4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/fv.lng" ex4
    let expectedOutput = ValorList[ValorStr "soma", ValorStr "c", ValorStr "x"]
    assertEqual "fv ex4" expectedOutput output

testfvFactorial :: Test
testfvFactorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/fv.lng" factorialProg
    let expectedOutput = ValorList[ValorStr "y", ValorStr "z", ValorStr "x"]
    assertEqual "fv Factorial" expectedOutput output

testmakeSetOfFVEx1 :: Test
testmakeSetOfFVEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/makeSetOfFV.lng" ex1
    let expectedOutput = ValorList[ValorPair(ValorStr "x", ValorStr "?"), 
                                   ValorPair(ValorStr "y", ValorStr "?"), 
                                   ValorPair(ValorStr "z", ValorStr "?") ]
    assertEqual "makeSetOfFV ex1" expectedOutput output

testFilterFlow :: Test
testFilterFlow = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/filterFlow.lng" (ValorPair(ValorStr "3", ValorList[
            ValorPair(ValorStr "1", ValorStr "2"), 
            ValorPair(ValorStr "2", ValorStr "3"),
            ValorPair(ValorStr "3", ValorStr "4"),
            ValorPair(ValorStr "4", ValorStr "5"),
            ValorPair(ValorStr "5", ValorStr "3"),
            ValorPair(ValorStr "3", ValorStr "6")]))
    let expectedOutput = (ValorList[ 
            ValorPair(ValorStr "2", ValorStr "3"),
            ValorPair(ValorStr "5", ValorStr "3")])
    assertEqual "Flow Factorial" expectedOutput output

exPPAEntry = ValorList[
    ValorPair(ValorStr "1", ValorList[
                            ValorPair(ValorStr "x", ValorStr "?"), 
                            ValorPair(ValorStr "y", ValorStr "?") ]),
    ValorPair(ValorStr "2", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "y", ValorStr "?") ]),
    ValorPair(ValorStr "3", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "2"),
                            ValorPair(ValorStr "y", ValorStr "4") ]),
    ValorPair(ValorStr "4", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"),
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "2"), 
                            ValorPair(ValorStr "y", ValorStr "4") ]),
    ValorPair(ValorStr "5", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"),
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "4") ])]

exPPAExit = ValorList[
    ValorPair(ValorStr "1", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "y", ValorStr "?") ]),
    ValorPair(ValorStr "2", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "y", ValorStr "2") ]),
    ValorPair(ValorStr "3", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "2"),
                            ValorPair(ValorStr "y", ValorStr "4") ]),
    ValorPair(ValorStr "4", ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "4") ]),
    ValorPair(ValorStr "5", ValorList[
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "4") ])]

testFindOrDefault :: Test
testFindOrDefault = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findOrDefault.lng" (
                                ValorPair(
                                    ValorStr "2", 
                                    exPPAExit))

    let expectedOutput = ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "y", ValorStr "2") ]
    assertEqual "testFindOrDefault PPA" expectedOutput output

testFindOrDefault2 :: Test
testFindOrDefault2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findOrDefault.lng" (
                                ValorPair(
                                    ValorStr "6", 
                                    exPPAExit))

    let expectedOutput = ValorList[]
    assertEqual "testFindOrDefault2 PPA" expectedOutput output

testRDEntry1 :: Test
testRDEntry1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdEntry.lng" (
                                ValorPair(ValorStr "1", 
                                          ValorPair(
                                            exPPA, 
                                            exPPAExit)))
    let expectedOutput = ValorList[ValorPair(ValorStr "x", ValorStr "?"), 
                                   ValorPair(ValorStr "y", ValorStr "?") ]
    assertEqual "rdEntry 1" expectedOutput output

testRDEntry2 :: Test
testRDEntry2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdEntry.lng" (
                                ValorPair(
                                    ValorStr "2", 
                                    ValorPair(
                                        exPPA, 
                                        exPPAExit)))

    let expectedOutput = ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"), 
                            ValorPair(ValorStr "y", ValorStr "?") ]
    assertEqual "rdEntry 2" expectedOutput output

testRDEntry4 :: Test
testRDEntry4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdEntry.lng" (
                                ValorPair(
                                    ValorStr "4", 
                                    ValorPair(
                                        exPPA, 
                                        exPPAExit)))

    let expectedOutput = ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"),
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "y", ValorStr "2"), 
                            ValorPair(ValorStr "y", ValorStr "4") ]
    assertEqual "rdEntry 4" expectedOutput output

testfindBlock1 :: Test
testfindBlock1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findBlock.lng" (ValorPair(ValorStr "1", exPPA))
    let expectedOutput = s01
    assertEqual "testfindBlock 1" expectedOutput output

testfindBlock2 :: Test
testfindBlock2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findBlock.lng" (ValorPair(ValorStr "2", exPPA))
    let expectedOutput = s02
    assertEqual "testfindBlock 2" expectedOutput output

testfindBlock3 :: Test
testfindBlock3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findBlock.lng" (ValorPair(ValorStr "3", exPPA))
    let expectedOutput = whileTeste
    assertEqual "testfindBlock 3" expectedOutput output

testfindBlock4 :: Test
testfindBlock4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findBlock.lng" (ValorPair(ValorStr "4", exPPA))
    let expectedOutput = whileS1
    assertEqual "testfindBlock 4" expectedOutput output

testfindBlock5 :: Test
testfindBlock5 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findBlock.lng" (ValorPair(ValorStr "5", exPPA))
    let expectedOutput = whileS2
    assertEqual "testfindBlock 5" expectedOutput output

testGenRD1 :: Test
testGenRD1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/genRD.lng" (s01)
    let expectedOutput = ValorList[
                            ValorPair(ValorStr "x", ValorStr "1") ]
    assertEqual "test genRD 1" expectedOutput output

testGenRD2 :: Test
testGenRD2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/genRD.lng" (exPPA)
    let expectedOutput = ValorList[]
    assertEqual "test genRD factorial" expectedOutput output

testKillRD :: Test
testKillRD = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/killRD.lng" (ValorPair(s01, exPPA))
    let expectedOutput = ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"),
                            ValorPair(ValorStr "x", ValorStr "5"),
                            ValorPair(ValorStr "x", ValorStr "?") ]
    assertEqual "test killRD" expectedOutput output

testRDExit1 :: Test
testRDExit1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdExit.lng" (
                                ValorPair(ValorStr "1", 
                                          ValorPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = ValorList[
                            ValorPair(ValorStr "x", ValorStr "1"),
                            ValorPair(ValorStr "y", ValorStr "?") ]
    assertEqual "rdExit 1" expectedOutput output

testRDExit2 :: Test
testRDExit2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdExit.lng" (
                                ValorPair(ValorStr "2", 
                                          ValorPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = ValorList[
                            ValorPair(ValorStr "y", ValorStr "2"), 
                            ValorPair(ValorStr "x", ValorStr "1") ]
    assertEqual "rdExit 2" expectedOutput output

testRDExit4 :: Test
testRDExit4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdExit.lng" (
                                ValorPair(ValorStr "4", 
                                          ValorPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = ValorList[
                        ValorPair(ValorStr "y", ValorStr "4"),
                        ValorPair(ValorStr "x", ValorStr "1"),
                        ValorPair(ValorStr "x", ValorStr "5") ]
    assertEqual "rdExit 4" expectedOutput output

testLabels :: Test
testLabels = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/labels.lng" (exPPA)
    let expectedOutput = ValorList[ValorStr "1", ValorStr "2", ValorStr "3", ValorStr "4", ValorStr "5"]
    assertEqual "labels" expectedOutput output

testInsertInto1 :: Test
testInsertInto1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/insertIntoMap.lng" (ValorPair(
                        ValorStr "2",
                        ValorPair(
                            ValorList [
                                ValorPair(ValorStr "teste", ValorStr "teste"), 
                                ValorPair(ValorStr "teste2", ValorStr "teste2")],
                            exPPAEntry
                        )))
    let expectedOutput = ValorList[
                            ValorPair(ValorStr "1", ValorList[
                                                    ValorPair(ValorStr "x", ValorStr "?"), 
                                                    ValorPair(ValorStr "y", ValorStr "?") ]),
                            ValorPair(ValorStr "2", ValorList[
                                                    ValorPair(ValorStr "teste", ValorStr "teste"), 
                                                    ValorPair(ValorStr "teste2", ValorStr "teste2"),
                                                    ValorPair(ValorStr "x", ValorStr "1"), 
                                                    ValorPair(ValorStr "y", ValorStr "?") ]),
                            ValorPair(ValorStr "3", ValorList[
                                                    ValorPair(ValorStr "x", ValorStr "1"), 
                                                    ValorPair(ValorStr "x", ValorStr "5"),
                                                    ValorPair(ValorStr "y", ValorStr "2"),
                                                    ValorPair(ValorStr "y", ValorStr "4") ]),
                            ValorPair(ValorStr "4", ValorList[
                                                    ValorPair(ValorStr "x", ValorStr "1"),
                                                    ValorPair(ValorStr "x", ValorStr "5"),
                                                    ValorPair(ValorStr "y", ValorStr "2"), 
                                                    ValorPair(ValorStr "y", ValorStr "4") ]),
                            ValorPair(ValorStr "5", ValorList[
                                                    ValorPair(ValorStr "x", ValorStr "1"),
                                                    ValorPair(ValorStr "x", ValorStr "5"),
                                                    ValorPair(ValorStr "y", ValorStr "4") ])]
    assertEqual "testInsertInto 1" expectedOutput output

testInsertInto2 :: Test
testInsertInto2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/insertIntoMap.lng" (ValorPair(
                        ValorStr "2",
                        ValorPair(
                            ValorList [
                                ValorPair(ValorStr "y", ValorStr "?")],
                            exPPAEntry
                        )))
    let expectedOutput = exPPAEntry
    assertEqual "testInsertInto 2" expectedOutput output

testUpdateMappings :: Test
testUpdateMappings = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/updateMappings.lng" (exPPA)
    let expectedOutput = ValorPair(exPPAEntry, exPPAExit)
    assertEqual "testUpdateMappings" expectedOutput output

testReachingDefinitionsPPA :: Test
testReachingDefinitionsPPA = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/reachingDefinitions.lng" (ValorPair(ValorInt 10, exPPA))
    let expectedOutput = ValorPair(exPPAEntry, exPPAExit)
    assertEqual "testReachingDefinitionsPPA" expectedOutput output

baseDFATestSuite :: Test
baseDFATestSuite = TestList [    TestLabel "is pair" testIsPair
                        ,   TestLabel "is Equal" testIsEqual
                        ,   TestLabel "Count Asgns ex1" testCountEx1
                        ,   TestLabel "Count Asgns ex2" testCountEx2
                        ,   TestLabel "Count Asgns ex3" testCountEx3
                        ,   TestLabel "Count Asgns ex4" testCountEx4
                        ,   TestLabel "Count Asgns factorial" testCountFactorial
                        ,   TestLabel "Init ex1" testInitEx1
                        ,   TestLabel "Init while" testInitExWhile
                        ,   TestLabel "Init if" testInitExIF
                        ,   TestLabel "Final ex1" testFinalEx1
                        ,   TestLabel "Final while" testFinalExWhile
                        ,   TestLabel "Final if" testFinalExIF
                        ,   TestLabel "Elem" testElem
                        ,   TestLabel "Not Elem" testNotElem
                        ,   TestLabel "AddUnique" testAddUnique
                        ,   TestLabel "AddUnique same" testAddUniqueSame
                        ,   TestLabel "Flow ex1" testFlowEx1
                        ,   TestLabel "Flow ex2" testFlowEx2
                        ,   TestLabel "Flow ex3" testFlowEx3
                        ,   TestLabel "Flow ex4" testFlowEx4
                        ,   TestLabel "Flow factorial" testFlowFactorial
                        ,   TestLabel "Chaotic Iteration 1" testChaoticIteration1
                        ,   TestLabel "Chaotic Iteration 2" testChaoticIteration2
                        ,   TestLabel "Asgns ex1" testAssignmentsEx1
                        ,   TestLabel "Asgns ex2" testAssignmentsEx2
                        ,   TestLabel "Asgns ex3" testAssignmentsEx3
                        ,   TestLabel "Asgns ex4" testAssignmentsEx4
                        ,   TestLabel "Asgns factorial" testAssignmentsFactorial
                        ,   TestLabel "fv ex1" testfvEx1
                        ,   TestLabel "fv ex2" testfvEx2
                        ,   TestLabel "fv ex3" testfvEx3
                        ,   TestLabel "fv ex4" testfvEx4
                        ,   TestLabel "fv factorial" testfvFactorial
                        ,   TestLabel "testmakeSetOfFVEx1" testmakeSetOfFVEx1
                        ,   TestLabel "testFilterFlow" testFilterFlow
                        ,   TestLabel "testFindOrDefault" testFindOrDefault
                        ,   TestLabel "testFindOrDefault2" testFindOrDefault2
                        ,   TestLabel "rdEntry 1" testRDEntry1
                        ,   TestLabel "rdEntry 2" testRDEntry2
                        ,   TestLabel "rdEntry 4" testRDEntry4
                        ,   TestLabel "testfindBlock 1" testfindBlock1
                        ,   TestLabel "testfindBlock 2" testfindBlock2
                        ,   TestLabel "testfindBlock 3" testfindBlock3
                        ,   TestLabel "testfindBlock 4" testfindBlock4
                        ,   TestLabel "testfindBlock 5" testfindBlock5
                        ,   TestLabel "testGenRD 1" testGenRD1
                        ,   TestLabel "testGenRD 2" testGenRD2
                        ,   TestLabel "testKillRD" testKillRD
                        ,   TestLabel "rdExit 1" testRDExit1
                        ,   TestLabel "rdExit 2" testRDExit2
                        ,   TestLabel "rdExit 4" testRDExit4
                        ,   TestLabel "labels" testLabels
                        ,   TestLabel "testInsertInto 1" testInsertInto1
                        ,   TestLabel "testInsertInto 2" testInsertInto2
                        ,   TestLabel "testReachingDefinitionsPPA" testReachingDefinitionsPPA
                        ]