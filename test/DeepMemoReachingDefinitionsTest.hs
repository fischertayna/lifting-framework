module DeepMemoReachingDefinitionsTest where

import Language.DeepMemo.Interpreter
import Language.DeepMemo.Driver
import Variability.VarTypes (Prop, VarValor(..), Var (Var), apply, mkBDDVar, notBDD, ttPC, ffPC, tt, ff, (/\), (\/), (|||))

import Language.Frontend.ErrM
import Helper

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)

propA :: Prop
propA = mkBDDVar "A"

propB :: Prop
propB = mkBDDVar "B"

atbt :: Prop
atbt = propA /\ propB

atbf :: Prop
atbf = propA /\ notBDD propB

afbt :: Prop
afbt = notBDD propA /\ propB

afbf :: Prop
afbf = notBDD propA /\ notBDD propB

substitutions :: [(String, String)]
substitutions =
    [ (show atbt, " atbt")
    , (show afbt, " afbt")
    , (show atbf, " atbf")
    , (show afbf, " afbf")
    , (show tt, " tt")
    , (show ff, " ff")
    , (show ttPC, " ttPC")
    , (show ffPC, " ffPC")
    ]

-- ex1, ex2, ex3, ex4, factorialProg :: VarValor
ex1 :: VarValor

-- x = 1;      1
-- #IFDEF A
-- y = 2;      2
-- #ELSE
-- y = 4;      3
-- #ENDIF
-- z = x + y;  4

-- The same as:
-- x = 1;  tt    1
-- y = 2;  A (complete with SKIP for ~A)
-- y = 4;  ~A    3 (complete with SKIP for A)
-- z = x + y;  tt 4
ex1 = VarPair(
        VarString (Var [("SEQ", ttPC)]),
        VarPair ( 
            VarPair (
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]),
                    VarPair (
                        VarString (Var [("x", ttPC)]),
                        VarPair(
                            VarString (Var [("INT", ttPC)]),
                            VarString (Var [("1", ttPC)])
                        )
                    )
                )
            ), -- x = 1;  tt    1
            VarPair(
                VarString (Var [("SEQ", propA)]), -- Should it be A??
                VarPair ( 
                    VarPair (
                        VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
                        VarPair (
                            VarString (Var [("2", propA), ("2", notBDD propA)]),
                            VarPair (
                                VarString (Var [("y", propA), ("DUMMY", notBDD propA)]),
                                VarPair(
                                    VarString (Var [("INT", propA), ("DUMMY", notBDD propA)]),
                                    VarString (Var [("1", propA), ("DUMMY", notBDD propA)])
                                )
                            )
                        )
                    ), -- y = 2;  A    2
                    VarPair(
                        VarString (Var [("SEQ", ttPC)]), -- Should it be ~A??
                        VarPair(
                            VarPair (
                                VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
                                VarPair (
                                    VarString (Var [("3", notBDD propA), ("3", propA)]),
                                    VarPair (
                                        VarString (Var [("y", notBDD propA), ("DUMMY", propA)]),
                                        VarPair(
                                            VarString (Var [("INT", notBDD propA), ("DUMMY", propA)]),
                                            VarString (Var [("4",notBDD  propA), ("DUMMY", propA)])
                                        )
                                    )
                                )
                            ), -- y = 4;  ~A    3
                            VarPair(
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("4", ttPC)]),
                                    VarPair (
                                        VarString (Var [("z", ttPC)]),
                                        VarPair (
                                            VarString (Var [("ADD", ttPC)]),
                                            VarPair (
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("x", ttPC)])
                                                ),
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("y", ttPC)])
                                                )
                                            )   
                                        )
                                    )
                                )
                            ) -- z = x + y;  tt 4
                        )
                    )
                )
            )
        )
    )
    

-- x = 1;  1
ex2 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("1", ttPC)]), 
            VarPair (
                VarString (Var [("x", ttPC)]),
                VarPair(
                    VarString (Var [("CONST", ttPC)]), 
                    VarInteger (Var [(1, ttPC)])
                )
            )
        ) 
    )

-- x = 1;                   1
-- soma = 0;                2
-- # IFDEF A
-- c = 10;                  3
-- # IFDEF B
-- d = 5;                   4                 
-- # ELSE
-- e = 2;                   5
-- # ENDIF
-- # ELSE
-- c = 9;                   6
-- # ENDIF
-- while (c) {              7
--     soma = soma + c;     8
--     c = c - 1;           9
-- # IFDEF B
--     d = d + c;           10
-- # ELSE
--     e = e * c;           11
-- # ENDIF
-- }

-- A && B
-- x = 1;                   1
-- soma = 0;                2
-- c = 10;                  3
-- d = 5;                   4
-- while (c) {              7
--     soma = soma + c;     8
--     c = c - 1;           9
--     d = d + c;          10
-- }

-- A && ~B
-- x = 1;                   1
-- soma = 0;                2
-- c = 10;                  3
-- e = 2;                   5
-- while (c) {              7
--     soma = soma + c;     8
--     c = c - 1;           9
--     e = e * c;          11
-- }

-- ~A && B
-- x = 1;                   1
-- soma = 0;                2
-- d = 5;                   4
-- c = 9;                   6
-- while (c) {              7
--     soma = soma + c;     8
--     c = c - 1;           9
--     d = d + c;           10
-- }

-- ~A && ~B
-- x = 1;                   1
-- soma = 0;                2
-- e = 2;                   5
-- c = 9;                   6
-- while (c) {              7
--     soma = soma + c;     8
--     c = c - 1;           9
--     e = e * c;           11
-- }
ex3 = VarPair(
        VarString (Var [("SEQ", ttPC)]),
        VarPair ( 
            VarPair(
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]), 
                    VarPair (
                        VarString (Var [("x", ttPC)]), 
                        VarPair(
                            VarString (Var [("CONST", ttPC)]), 
                            VarInteger (Var [(1, ttPC)])
                        )
                    )
                )
            ), -- x = 1;  1
            VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair (
                    VarPair(
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", ttPC)]), 
                            VarPair (
                                VarString (Var [("SOMA", ttPC)]), 
                                VarPair(
                                    VarString (Var [("CONST", ttPC)]), 
                                    VarInteger (Var [(0, ttPC)])
                                )
                            )
                        )
                    ), -- soma = 0;  2
                    VarPair (
                        VarString (Var [("SEQ", ttPC)]),
                        VarPair (
                            VarPair(
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("3", atbt), ("3", atbf), ("4", afbt), ("4", afbf)]), 
                                    VarPair (
                                        VarString (Var [("c", atbt), ("c", atbf), ("d", afbt), ("e", afbf)]), 
                                        VarPair(
                                            VarString (Var [("CONST", ttPC)]), 
                                            VarInteger (Var [(10, atbt), (10, atbf), (5, afbt), (2, afbf)])
                                        )
                                    )
                                )
                            ), -- c = 10; 3 A && B A && ~B / d = 5; 4 ~A && B / e = 2; 5 ~A && ~B
                            VarPair(
                                VarString (Var [("SEQ", ttPC)]),
                                VarPair(
                                    VarPair(
                                        VarString (Var [("ASGN", ttPC)]),
                                        VarPair (
                                            VarString (Var [("4", atbt), ("5", atbf), ("6", afbt), ("6", afbf)]), 
                                            VarPair (
                                                VarString (Var [("d", atbt), ("e", atbt), ("c", afbt), ("c", afbf)]), 
                                                VarPair(
                                                    VarString (Var [("CONST", ttPC)]), 
                                                    VarInteger (Var [(10, atbt), (2, atbt), (9, afbt), (9, afbf)])
                                                )
                                            )
                                        )
                                    ), -- d = 5; 4 A && B / e = 2; 5 A && ~B / c = 9; 6 ~A && B  ~A && ~B
                                    VarPair(
                                        VarString (Var [("WHILE", ttPC)]),
                                        VarPair (
                                            VarPair(
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]), 
                                                    VarString (Var [("c", ttPC)])
                                                ), 
                                                VarString (Var [("7", ttPC)])
                                            ), -- while (c) 7
                                            VarPair(
                                                VarString (Var [("SEQ", ttPC)]),
                                                VarPair (
                                                    VarPair(
                                                        VarString (Var [("ASGN", ttPC)]),
                                                        VarPair (
                                                            VarString (Var [("8", ttPC)]), 
                                                            VarPair (
                                                                VarString (Var [("SOMA", ttPC)]), 
                                                                VarPair (
                                                                    VarString (Var [("ADD", ttPC)]), 
                                                                    VarPair (
                                                                        VarPair (
                                                                            VarString (Var [("VAR", ttPC)]), 
                                                                            VarString (Var [("SOMA", ttPC)])
                                                                        ),
                                                                        VarPair (
                                                                            VarString (Var [("VAR", ttPC)]),
                                                                            VarString (Var [("c", ttPC)])
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ), -- soma = soma + c; 8
                                                    VarPair(
                                                        VarString (Var [("SEQ", ttPC)]),
                                                        VarPair(
                                                            VarPair(
                                                                VarString (Var [("ASGN", ttPC)]),
                                                                VarPair (
                                                                    VarString (Var [("9", ttPC)]), 
                                                                    VarPair (
                                                                        VarString (Var [("c", ttPC)]), 
                                                                        VarPair (
                                                                            VarString (Var [("SUB", ttPC)]), 
                                                                            VarPair (
                                                                                VarPair (
                                                                                    VarString (Var [("VAR", ttPC)]),
                                                                                    VarString (Var [("c", ttPC)])
                                                                                ),
                                                                                VarPair(
                                                                                    VarString (Var [("CONST", ttPC)]), 
                                                                                    VarInteger (Var [(1, ttPC)])
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            ), --     c = c - 1;           9
                                                            VarPair(
                                                                VarString (Var [("ASGN", ttPC)]),
                                                                VarPair (
                                                                    VarString (Var [("10", propB), ("11", notBDD propB)]), 
                                                                    VarPair (
                                                                        VarString (Var [("d", propB), ("e", notBDD propB)]), 
                                                                        VarPair (
                                                                            VarString (Var [("ADD", propB), ("MULT", notBDD propB)]), 
                                                                            VarPair (
                                                                                VarPair (
                                                                                    VarString (Var [("VAR", ttPC)]),
                                                                                    VarString (Var [("d", propB), ("e", notBDD propB)])
                                                                                ),
                                                                                VarPair(
                                                                                    VarString (Var [("VAR", ttPC)]), 
                                                                                    VarString (Var [("c", ttPC)])
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            ) --  d = d + c; 10 A && B ~A && B | e = e * c; 11 A && ~B ~A && ~B
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

-- c = 5                  // (2)
-- # IFDEF A             
-- d = password;          // (3) TAINT PROPAGATION
-- # ELSE
-- d = "not-password";    // (4) 
-- # ENDIF

ex = VarPair(
        VarString (Var [("SEQ", ttPC)]),
        VarPair ( 
            VarPair(
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("2", ttPC)]), 
                    VarPair (
                        VarString (Var [("c", ttPC)]), 
                        VarPair(
                            VarString (Var [("CONST", ttPC)]), 
                            VarInteger (Var [(5, ttPC)])
                        )
                    )
                )
            ), -- c = 5;  (1)
            VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair (
                    VarPair(
                        VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
                        VarPair (
                            VarString (Var [("3", propA), ("DUMMY", notBDD propA)]), 
                            VarPair (
                                VarString (Var [("d", propA), ("DUMMY", notBDD propA)]), 
                                VarPair(
                                    VarString (Var [("VAR", propA), ("DUMMY", notBDD propA)]), 
                                    VarString (Var [("password", propA), ("DUMMY", notBDD propA)])
                                )
                            )
                        )
                    ), -- d = password;          // (3)
                    VarPair(
                        VarString (Var [("SKIP", propA), ("ASGN", notBDD propA)]),
                        VarPair (
                            VarString (Var [("DUMMY", propA), ("4", notBDD propA)]), 
                            VarPair (
                                VarString (Var [("DUMMY", propA), ("d", notBDD propA)]), 
                                VarPair(
                                    VarString (Var [("DUMMY", propA), ("CONST", notBDD propA)]), 
                                    VarString (Var [("DUMMY", propA), ("not-password", notBDD propA)])
                                )
                            )
                        )
                    ) -- d = "not-password";    // (4) 
                )
            )
        )
    )

-- x = 1;            1
-- # IFDEF A
-- soma = 0;         2
-- c = 10;           3
-- if (!soma)        4
--   then
--     soma = c + x; 5 
--   else
--     soma = 0;     6
-- # ELSE
-- y = 2;            7
-- soma = x + y;     8
-- # ENDIF

-- A
-- x = 1;            1
-- soma = 0;         2
-- c = 10;           3
-- if (!soma)        4
--   then
--     soma = c + x; 5 
--   else
--     soma = 0;     6

-- ~A
-- x = 1;            1
-- y = 2;            7
-- soma = x + y;     8
ex4 = VarPair ( 
        VarString (Var [("SEQ", ttPC)]),
        VarPair(
            VarPair(
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]), 
                    VarPair (
                        VarString (Var [("x", ttPC)]), 
                        VarPair(
                            VarString (Var [("CONST", ttPC)]), 
                            VarInteger (Var [(1, ttPC)])
                        )
                    )
                )
            ), -- x = 1; 1
            VarPair(
                VarString (Var [("SEQ", ttPC)]),
                VarPair (
                    VarPair(
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", propA), ("7", notBDD propA)]), 
                            VarPair (
                                VarString (Var [("SOMA", propA), ("y", notBDD propA)]), 
                                VarPair(
                                    VarString (Var [("CONST", ttPC)]), 
                                    VarInteger (Var [(0, propA), (2, notBDD propA)])
                                )
                            )
                        )
                    ), -- soma = 0; 2 A | y = 2; 7 ~A
                    VarPair(
                        VarString (Var [("SEQ", propA), ("ASGN", notBDD propA)]),
                        VarPair (
                            VarPair(
                                VarString (Var [("ASGN", propA), ("8", notBDD propA)]),
                                VarPair (
                                    VarString (Var [("3", propA), ("SOMA", notBDD propA)]), 
                                    VarPair (
                                        VarString (Var [("c", propA), ("ADD", notBDD propA)]), 
                                        VarPair(
                                            VarString (Var [("CONST", propA)]), -- ?????????
                                            -- VarPair (
                                            --                     VarString (Var [("VAR", propA)]), 
                                            --                     VarString (Var [("x", propA)])
                                            --                 ),
                                            VarInteger (Var [(10, propA)]) --- ????????
                                             -- VarPair (
                                            --                     VarString (Var [("VAR", propA)]), 
                                            --                     VarString (Var [("y", propA)])
                                            --                 ),
                                        )
                                    )
                                )
                            ),
                            VarPair(
                                VarString (Var [("IF", propA)]),
                                VarPair ( 
                                    VarPair(
                                        VarPair (
                                            VarString (Var [("NOT", propA)]), 
                                            VarPair (
                                                VarString (Var [("VAR", propA)]), 
                                                VarString (Var [("SOMA", propA)])
                                            )
                                        ),
                                        VarString (Var [("4", propA)])
                                    ),
                                    VarPair (
                                        VarPair(
                                            VarString (Var [("ASGN", propA)]),
                                            VarPair (
                                                VarString (Var [("5", propA)]), 
                                                VarPair (
                                                    VarString (Var [("SOMA", propA)]), 
                                                    VarPair (
                                                        VarString (Var [("ADD", propA)]), 
                                                        VarPair (
                                                            VarPair (
                                                                VarString (Var [("VAR", propA)]), 
                                                                VarString (Var [("c", propA)])
                                                            ),
                                                            VarPair (
                                                                VarString (Var [("VAR", propA)]), 
                                                                VarString (Var [("x", propA)])
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        ),
                                        VarPair(
                                            VarString (Var [("ASGN", propA)]),
                                            VarPair (
                                                VarString (Var [("6", propA)]), 
                                                VarPair (
                                                    VarString (Var [("SOMA", propA)]), 
                                                    VarPair(
                                                        VarString (Var [("CONST", propA)]), 
                                                        VarInteger (Var [(0, propA)])
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ) -- SEQ A | soma = x + y; 8 ~A
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
factorialProg =  VarPair (
                    VarString (Var [("SEQ", ttPC)]),
                    VarPair(
                        VarPair(
                            VarString (Var [("ASGN", ttPC)]),
                            VarPair (
                                VarString (Var [("1", ttPC)]), 
                                VarPair (
                                    VarString (Var [("y", ttPC)]), 
                                    VarPair (
                                        VarString (Var [("VAR", ttPC)]), 
                                        VarString (Var [("x", ttPC)])
                                    )
                                )
                            )
                        ),
                        VarPair(
                            VarString (Var [("SEQ", ttPC)]),
                            VarPair (
                                VarPair(
                                    VarString (Var [("ASGN", ttPC)]),
                                    VarPair (
                                        VarString (Var [("2", ttPC)]), 
                                        VarPair (
                                            VarString (Var [("z", ttPC)]), 
                                            VarPair(
                                                VarString (Var [("CONST", ttPC)]), 
                                                VarInteger (Var [(1, ttPC)])
                                            )
                                        )
                                    )
                                ),
                                VarPair(
                                    VarString (Var [("SEQ", ttPC)]),
                                    VarPair(
                                        VarPair (
                                            VarString (Var [("WHILE", ttPC)]), 
                                            VarPair ( 
                                                VarPair(
                                                    VarPair (
                                                        VarString (Var [("VAR", ttPC)]), 
                                                        VarString (Var [("y", ttPC)])
                                                    ),
                                                    VarString (Var [("3", ttPC)])
                                                ),
                                                VarPair (
                                                    VarString (Var [("SEQ", ttPC)]),
                                                    VarPair(
                                                        VarPair(
                                                            VarString (Var [("ASGN", ttPC)]),
                                                            VarPair (
                                                                VarString (Var [("4", ttPC)]), 
                                                                VarPair (
                                                                    VarString (Var [("z", ttPC)]), 
                                                                    VarPair (
                                                                        VarString (Var [("MULT", ttPC)]), 
                                                                        VarPair (
                                                                            VarPair (
                                                                                VarString (Var [("VAR", ttPC)]), 
                                                                                VarString (Var [("z", ttPC)])
                                                                            ),
                                                                            VarPair (
                                                                                VarString (Var [("VAR", ttPC)]),
                                                                                VarString (Var [("y", ttPC)])
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        ),
                                                        VarPair(
                                                            VarString (Var [("ASGN", ttPC)]),
                                                            VarPair (
                                                                VarString (Var [("5", ttPC)]), 
                                                                VarPair (
                                                                    VarString (Var [("y", ttPC)]), 
                                                                    VarPair (
                                                                        VarString (Var [("SUB", ttPC)]), 
                                                                        VarPair (
                                                                            VarPair (
                                                                                VarString (Var [("VAR", ttPC)]),
                                                                                VarString (Var [("y", ttPC)])
                                                                            ),
                                                                            VarPair(
                                                                                VarString (Var [("CONST", ttPC)]), 
                                                                                VarInteger (Var [(1, ttPC)])
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
                                        VarPair(
                                            VarString (Var [("ASGN", ttPC)]),
                                            VarPair (
                                                VarString (Var [("6", ttPC)]), 
                                                VarPair (
                                                    VarString (Var [("y", ttPC)]), 
                                                    VarPair(
                                                        VarString (Var [("CONST", ttPC)]), 
                                                        VarInteger (Var [(0, ttPC)])
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

-- -- s01 = Assignment "x" (Const 5) 1
-- -- s02 = Assignment "y" (Const 1) 2

-- -- whileTeste = (GTExp (Var "x") (Const 1), 3)
-- -- whileS1 = Assignment "y" (Mult (Var "x") (Var "y")) 4
-- -- whileS2 = Assignment "x" (Sub (Var "x") (Const 1)) 5
-- -- example = While whileTeste (Seq whileS1 whileS2)

s01 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("1", ttPC)]),
            VarPair (
                VarString (Var [("x", ttPC)]),
                VarPair (
                    VarString (Var [("CONST", ttPC)]),
                    VarString (Var [("5", ttPC)])
                ))))

s02 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("2", ttPC)]),
            VarPair (
                VarString (Var [("y", ttPC)]), 
                VarPair(
                    VarString (Var [("CONST", ttPC)]),
                    VarString (Var [("1", ttPC)])
                ))))

whileTeste = VarPair (
                VarString (Var [("VAR", ttPC)]),
                VarString (Var [("x", ttPC)])
            )

whileS1 = VarPair(
            VarString (Var [("ASGN", ttPC)]),
            VarPair (
                VarString (Var [("4", ttPC)]),
                VarPair (
                    VarString (Var [("y", ttPC)]),
                    VarPair (
                        VarString (Var [("MULT", ttPC)]), 
                        VarPair (
                            VarPair (
                                VarString (Var [("VAR", ttPC)]),
                                VarString (Var [("x", ttPC)])
                            ),
                            VarPair (
                                VarString (Var [("VAR", ttPC)]),
                                VarString (Var [("y", ttPC)])
                            ))))))

whileS2 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("5", ttPC)]),
            VarPair (
                VarString (Var [("x", ttPC)]),
                VarPair (
                    VarString (Var [("SUB", ttPC)]),
                    VarPair (
                        VarPair (
                            VarString (Var [("VAR", ttPC)]),
                            VarString (Var [("x", ttPC)])
                        ),
                        VarPair(
                            VarString (Var [("CONST", ttPC)]),
                            VarString (Var [("1", ttPC)])
                        ))))))

example =  VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair(
                    s01,
                    VarPair(
                        VarString (Var [("SEQ", ttPC)]),
                        VarPair (
                            s02,
                            VarPair (
                                VarString (Var [("WHILE", ttPC)]),
                                VarPair ( 
                                    VarPair(
                                        whileTeste,
                                        VarString (Var [("3", ttPC)])
                                    ),
                                    VarPair (
                                        VarString (Var [("SEQ", ttPC)]),
                                        VarPair(
                                            whileS1,
                                            whileS2
                                        ))))))))

-- #IFDEF A
-- vs01 = Assignment "x" (Const 5) 1
-- #ELSE
-- vas011 = Assignment "z" (Const 2) 11
-- vas022 = Assignment "x" (Mult (Const 5) (Var "y")) 12
-- #ENDIF
-- vs02 = Assignment "y" (Const 1) 2

-- vwhileTeste = (GTExp (Var "x") (Const 1), 3)
-- vwhileS1 = Assignment "y" (Mult (Var "x") (Var "y")) 4
-- vwhileS2 = Assignment "x" (Sub (Var "x") (Const 1)) 5
-- vexample = While whileTeste (Seq whileS1 whileS2)

-- equivalent

-- vs01 = Assignment "x" (Const 5) 1                        A  ||  skip ~A  19
-- vas011 = Assignment "z" (Const 2) 11                     ~A ||  skip A  119
-- vas022 = Assignment "x" (Mult (Const 5) (Var "y")) 12    ~A ||  skip A  129

-- vs02 = Assignment "y" (Const 1) 2

-- vwhileTeste = (GTExp (Var "x") (Const 1), 3)
-- vwhileS1 = Assignment "y" (Mult (Var "x") (Var "y")) 4
-- vwhileS2 = Assignment "x" (Sub (Var "x") (Const 1)) 5
-- vexample = While whileTeste (Seq whileS1 whileS2)

vs01 = VarPair(
        VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
        VarPair (
            VarString (Var [("1", propA), ("19", notBDD propA)]),
            VarPair (
                VarString (Var [("x", propA), ("DUMMY", notBDD propA)]),
                VarPair (
                    VarString (Var [("CONST", propA), ("DUMMY", notBDD propA)]),
                    VarString (Var [("5", propA), ("DUMMY", notBDD propA)])
                ))))

vas011 = VarPair(
            VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
            VarPair (
                VarString (Var [("11", notBDD propA), ("119", propA)]),
                VarPair (
                    VarString (Var [("z", notBDD propA), ("DUMMY", propA)]),
                    VarPair (
                        VarString (Var [("CONST", notBDD propA), ("DUMMY", propA)]),
                        VarString (Var [("2", notBDD propA), ("DUMMY", propA)])
                    ))))

vas012 = VarPair(
            VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
            VarPair (
                VarString (Var [("12", notBDD propA), ("129", propA)]),
                VarPair (
                    VarString (Var [("x", notBDD propA), ("DUMMY", propA)]),
                    VarPair (
                        VarString (Var [("MULT", notBDD propA), ("DUMMY", propA)]), 
                        VarPair (
                            VarPair (
                                VarString (Var [("CONST", notBDD propA), ("DUMMY", propA)]),
                                VarString (Var [("5", notBDD propA), ("DUMMY", propA)])
                            ),
                            VarPair (
                                VarString (Var [("VAR", notBDD propA), ("DUMMY", propA)]),
                                VarString (Var [("y",notBDD propA), ("DUMMY", propA)])
                            ))))))

vs02 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("2", ttPC)]),
            VarPair (
                VarString (Var [("y", ttPC)]), 
                VarPair(
                    VarString (Var [("CONST", ttPC)]),
                    VarString (Var [("1", ttPC)])
                ))))

vwhileTeste = VarPair (
                VarString (Var [("VAR", ttPC)]),
                VarString (Var [("x", ttPC)])
            )

vwhileS1 = VarPair(
            VarString (Var [("ASGN", ttPC)]),
            VarPair (
                VarString (Var [("4", ttPC)]),
                VarPair (
                    VarString (Var [("y", ttPC)]),
                    VarPair (
                        VarString (Var [("MULT", ttPC)]), 
                        VarPair (
                            VarPair (
                                VarString (Var [("VAR", ttPC)]),
                                VarString (Var [("x", ttPC)])
                            ),
                            VarPair (
                                VarString (Var [("VAR", ttPC)]),
                                VarString (Var [("y", ttPC)])
                            ))))))

vwhileS2 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("5", ttPC)]),
            VarPair (
                VarString (Var [("x", ttPC)]),
                VarPair (
                    VarString (Var [("SUB", ttPC)]),
                    VarPair (
                        VarPair (
                            VarString (Var [("VAR", ttPC)]),
                            VarString (Var [("x", ttPC)])
                        ),
                        VarPair(
                            VarString (Var [("CONST", ttPC)]),
                            VarString (Var [("1", ttPC)])
                        ))))))

vexample =  VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair(
                    vs01,
                    VarPair(
                        VarString (Var [("SEQ", ttPC)]),
                        VarPair(
                            vas011,
                            VarPair(
                                VarString (Var [("SEQ", ttPC)]),
                                VarPair(
                                    vas012,
                                    VarPair(
                                        VarString (Var [("SEQ", ttPC)]),
                                        VarPair (
                                            vs02,
                                            VarPair (
                                                VarString (Var [("WHILE", ttPC)]),
                                                VarPair ( 
                                                    VarPair(
                                                        vwhileTeste,
                                                        VarString (Var [("3", ttPC)])
                                                    ),
                                                    VarPair (
                                                        VarString (Var [("SEQ", ttPC)]),
                                                        VarPair(
                                                            vwhileS1,
                                                            vwhileS2
                                                        ))))))))))))


testIsPair :: Test
testIsPair = TestCase $ do
    output <- processFile (executeProg "check") "src/Language/Examples/taint/isPair.lng" ex1
    let expectedOutput = (VarBool (Var [(True, ttPC)]))
    assertEqual "is ex1 pair" expectedOutput (fst output)

-- testIsEqual :: Test
-- testIsEqual = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/isEqual.lng" (VarPair (
--                                                         VarInteger (Var [(1, ttPC)]),
--                                                         VarInteger (Var [(1, ttPC)])
--                                                     ))
--     let expectedOutput = (ValorBool True)
--     assertEqual "is ex1 pair" expectedOutput output


testCountEx1 :: Test
testCountEx1 = TestCase $ do
    output <- processFile (executeProg "count") "src/Language/Examples/taint/Count-Asgns.lng" ex1
    let expectedOutput = (VarInteger (Var [(3, ttPC)]))
    assertEqual "Count Asgns ex1" expectedOutput (fst output)

testCountEx2 :: Test
testCountEx2 = TestCase $ do
    output <- processFile (executeProg "count") "src/Language/Examples/taint/Count-Asgns.lng" ex2
    let expectedOutput = (VarInteger (Var [(1, ttPC)]))
    assertEqual "Count Asgns ex2" expectedOutput (fst output)


testCountEx3 :: Test
testCountEx3 = TestCase $ do
    output <- processFile (executeProg "count") "src/Language/Examples/taint/Count-Asgns.lng" ex3
    let expectedOutput = (VarInteger (Var [(7, ttPC)]))
    assertEqual "Count Asgns ex3" expectedOutput (fst output)

-- testCountEx4 :: Test
-- testCountEx4 = TestCase $ do
--     output <- processFile (executeProg "count") "src/Language/Examples/taint/Count-Asgns.lng" ex4
--     let expectedOutput = (ValorInt 5)
--     assertEqual "Count Asgns ex4" expectedOutput output

-- testCountFactorial :: Test
-- testCountFactorial = TestCase $ do
--     output <- processFile (executeProg "count") "src/Language/Examples/taint/Count-Asgns.lng" factorialProg
--     let expectedOutput = (ValorInt 5)
--     assertEqual "Count Asgns Factorial" expectedOutput output

testInitEx1 :: Test
testInitEx1 = TestCase $ do
    output <- processFile (executeProg "init") "src/Language/Examples/taint/init.lng" ex1
    let expectedOutput = (VarString (Var [("1", ttPC)]))
    assertEqual "init ex1" expectedOutput (fst output)

-- testInitExWhile :: Test
-- testInitExWhile = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/init.lng" (VarPair (
--                                 VarString (Var [("WHILE", ttPC)]), 
--                                 VarPair ( 
--                                     VarPair(
--                                         VarPair (
--                                             VarString (Var [("VAR", ttPC)]), 
--                                             VarString (Var [("y", ttPC)])
--                                         ),
--                                         VarString (Var [("3", ttPC)])
--                                     ),
--                                     VarPair(
--                                         VarString (Var [("4", ttPC)]),
--                                         VarPair (
--                                             VarString (Var [("ASGN", ttPC)]), 
--                                             VarPair (
--                                                 VarString (Var [("c", ttPC)]), 
--                                                 VarPair(
--                                                     VarString (Var [("CONST", ttPC)]),
--                                                     VarInteger (Var [(1, ttPC)])
--                                                 )
--                                             )
--                                         )
--                                     )
--                                 )
--                             ))
--     let expectedOutput = (VarString (Var [("3", ttPC)]))
--     assertEqual "init while" expectedOutput output

-- testInitExIF :: Test
-- testInitExIF = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/init.lng" (VarPair(
--                                 VarString (Var [("IF", ttPC)]),
--                                 VarPair ( 
--                                     VarPair(
--                                         VarPair (
--                                             VarString (Var [("NOT", ttPC)]), 
--                                             VarPair (
--                                                 VarString (Var [("VAR", ttPC)]), 
--                                                 VarString (Var [("SOMA", ttPC)])
--                                             )
--                                         ),
--                                         VarString (Var [("4", ttPC)])
--                                     ),
--                                     VarPair (
--                                         VarPair(
--                                             VarString (Var [("ASGN", ttPC)]),
--                                             VarPair (
--                                                 VarString (Var [("5", ttPC)]), 
--                                                 VarPair (
--                                                     VarString (Var [("x", ttPC)]), 
--                                                     VarPair (
--                                                         VarString (Var [("CONST", ttPC)]),
--                                                         ValorInt 3
--                                                     )
--                                                 )
--                                             )
--                                         ),
--                                         VarPair(
--                                             VarString (Var [("ASGN", ttPC)]),
--                                             VarPair (
--                                                 VarString (Var [("6", ttPC)]), 
--                                                 VarPair (
--                                                     VarString (Var [("SOMA", ttPC)]), 
--                                                     VarPair(
--                                                         VarString (Var [("CONST", ttPC)]), 
--                                                         VarInteger (Var [(0, ttPC)])
--                                                     )
--                                                 )
--                                             )
--                                         )
--                                     )
--                                 )
--                             ))
--     let expectedOutput = (VarString (Var [("4", ttPC)]))
--     assertEqual "init if" expectedOutput output

testFinalEx1 :: Test
testFinalEx1 = TestCase $ do
    output <- processFile (executeProg "final") "src/Language/Examples/taint/final.lng" ex1
    let expectedOutput = (VarList [VarString (Var [("4", ttPC)])])
    assertEqual "final ex1" expectedOutput (fst output)

-- testFinalExWhile :: Test
-- testFinalExWhile = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/final.lng" (VarPair (
--                                 VarString (Var [("WHILE", ttPC)]), 
--                                 VarPair ( 
--                                     VarPair(
--                                         VarPair (
--                                             VarString (Var [("VAR", ttPC)]), 
--                                             VarString (Var [("y", ttPC)])
--                                         ),
--                                         VarString (Var [("3", ttPC)])
--                                     ),
--                                     VarPair(
--                                         VarString (Var [("ASGN", ttPC)]),
--                                         VarPair (
--                                             VarString (Var [("4", ttPC)]), 
--                                             VarPair (
--                                                 VarString (Var [("c", ttPC)]), 
--                                                 VarPair(
--                                                     VarString (Var [("CONST", ttPC)]),
--                                                     VarInteger (Var [(1, ttPC)])
--                                                 )
--                                             )
--                                         )
--                                     )
--                                 )
--                             ))
--     let expectedOutput = (ValorList [VarString (Var [("3", ttPC)])])
--     assertEqual "final while" expectedOutput output

-- testFinalExIF :: Test
-- testFinalExIF = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/final.lng" (VarPair(
--                                 VarString (Var [("IF", ttPC)]),
--                                 VarPair ( 
--                                     VarPair(
--                                         VarPair (
--                                             VarString (Var [("NOT", ttPC)]), 
--                                             VarPair (
--                                                 VarString (Var [("VAR", ttPC)]), 
--                                                 VarString (Var [("SOMA", ttPC)])
--                                             )
--                                         ),
--                                         VarString (Var [("4", ttPC)])
--                                     ),
--                                     VarPair (
--                                         VarPair(
--                                             VarString (Var [("ASGN", ttPC)]),
--                                             VarPair (
--                                                 VarString (Var [("5", ttPC)]), 
--                                                 VarPair (
--                                                     VarString (Var [("x", ttPC)]), 
--                                                     VarPair (
--                                                         VarString (Var [("CONST", ttPC)]),
--                                                         ValorInt 3
--                                                     )
--                                                 )
--                                             )
--                                         ),
--                                         VarPair(
--                                             VarString (Var [("ASGN", ttPC)]),
--                                             VarPair (
--                                                 VarString (Var [("6", ttPC)]), 
--                                                 VarPair (
--                                                     VarString (Var [("SOMA", ttPC)]), 
--                                                     VarPair(
--                                                         VarString (Var [("CONST", ttPC)]), 
--                                                         VarInteger (Var [(0, ttPC)])
--                                                     )
--                                                 )
--                                             )
--                                         )
--                                     )
--                                 )
--                             ))
--     let expectedOutput = (ValorList [VarString (Var [("5", ttPC)]), VarString (Var [("6", ttPC)])])
--     assertEqual "final if" expectedOutput output

-- testElem :: Test
-- testElem = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/elem.lng" (VarPair(
--             ValorStr "b",
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (VarInteger (Var [(1, ttPC)]))
--     assertEqual "elem" expectedOutput output

-- testNotElem :: Test
-- testNotElem = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/elem.lng" (VarPair(
--             ValorStr "a",
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (VarInteger (Var [(0, ttPC)]))
--     assertEqual "not elem" expectedOutput output

-- testAddUnique :: Test
-- testAddUnique = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/addUnique.lng" (VarPair(
--             ValorStr "a",
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (ValorList [ValorStr "a", ValorBool True, ValorStr "b", ValorInt 4])
--     assertEqual "addUnique" expectedOutput output

-- testAddUniqueSame :: Test
-- testAddUniqueSame = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/addUnique.lng" (VarPair(
--             ValorStr "b",
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (ValorList [ValorBool True, ValorStr "b", ValorInt 4])
--     assertEqual "addUnique same" expectedOutput output

-- testUnion :: Test
-- testUnion = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/union.lng" (VarPair(
--             ValorList [ValorStr "a", ValorStr "b", VarInteger (Var [(1, ttPC)])],
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (ValorList [ValorStr "a", ValorStr "b", VarInteger (Var [(1, ttPC)]), ValorBool True, ValorInt 4])
--     assertEqual "union" expectedOutput output

testFlowEx1 :: Test
testFlowEx1 = TestCase $ do
    output <- processFile (executeProg "flow") "src/Language/Examples/taint/cfg.lng" ex1
    let expectedOutput = (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", propA),("3", notBDD propA)])),
                                VarPair(VarString (Var [("2", propA),("3", notBDD propA)]),VarString (Var [("4", ttPC)]))])
    -- putStrLn ("\n Flow out: " ++ (substitute (show (fst output)) substitutions))
    assertEqual "Flow ex1" expectedOutput (fst output)

testFlowEx2 :: Test
testFlowEx2 = TestCase $ do
    output <- processFile (executeProg "flow") "src/Language/Examples/taint/cfg.lng" ex2
    let expectedOutput = (VarList[])
    assertEqual "Flow ex2" expectedOutput (fst output)


testFlowEx3 :: Test
testFlowEx3 = TestCase $ do
    output <- processFile (executeProg "flow") "src/Language/Examples/taint/cfg.lng" ex3
    let expectedOutput = (VarList[
            VarPair(VarString (Var [("1", ttPC)]), VarString (Var [("2", ttPC)])), 
            VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
            VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)])),
            VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("6", ttPC)])),
            VarPair(VarString (Var [("6", ttPC)]), VarString (Var [("4", ttPC)]))])
    putStrLn ("\n Flow out: " ++ (substitute (show (fst output)) substitutions))
    assertEqual "Flow ex3" expectedOutput (fst output)

-- testFlowEx4 :: Test
-- testFlowEx4 = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/cfg.lng" ex4
--     let expectedOutput = (ValorList[
--             VarPair(VarString (Var [("1", ttPC)]), VarString (Var [("2", ttPC)])), 
--             VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
--             VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
--             VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)])),
--             VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("6", ttPC)]))])
--     assertEqual "Flow ex4" expectedOutput output

-- testFlowFactorial :: Test
-- testFlowFactorial = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/cfg.lng" factorialProg
--     let expectedOutput = (ValorList[
--             VarPair(VarString (Var [("1", ttPC)]), VarString (Var [("2", ttPC)])), 
--             VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
--             VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
--             VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)])),
--             VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)])),
--             VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("6", ttPC)]))])
--     assertEqual "Flow Factorial" expectedOutput output

-- testChaoticIteration :: Test
-- testChaoticIteration = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/chaoticIteration.lng" (ValorInt 4)
--     let expectedOutput = (ValorInt 4)
--     assertEqual "chaotic iteration test" expectedOutput output

deepMemoRdTestSuite :: Test
deepMemoRdTestSuite = TestList [    TestLabel "is pair" testIsPair
                        -- ,   TestLabel "is Equal" testIsEqual
                        ,   TestLabel "Count Asgns ex1" testCountEx1
                        -- ,   TestLabel "Count Asgns ex2" testCountEx2
                        -- ,   TestLabel "Count Asgns ex3" testCountEx3
                        -- ,   TestLabel "Count Asgns ex4" testCountEx4
                        -- ,   TestLabel "Count Asgns factorial" testCountFactorial
                        -- ,   TestLabel "Init ex1" testInitEx1
                        -- ,   TestLabel "Init while" testInitExWhile
                        -- ,   TestLabel "Init if" testInitExIF
                        -- ,   TestLabel "Final ex1" testFinalEx1
                        -- ,   TestLabel "Final while" testFinalExWhile
                        -- ,   TestLabel "Final if" testFinalExIF
                        -- ,   TestLabel "Elem" testElem
                        -- ,   TestLabel "Not Elem" testNotElem
                        -- ,   TestLabel "AddUnique" testAddUnique
                        -- ,   TestLabel "AddUnique same" testAddUniqueSame
                        -- ,   TestLabel "Union" testUnion
                        -- ,   TestLabel "Flow ex1" testFlowEx1
                        -- ,   TestLabel "Flow ex2" testFlowEx2
                        -- ,   TestLabel "Flow ex3" testFlowEx3
                        -- ,   TestLabel "Flow ex4" testFlowEx4
                        -- ,   TestLabel "Flow factorial" testFlowFactorial
                        -- l "Chaotic Iteration" testChaoticIteration
                        ]