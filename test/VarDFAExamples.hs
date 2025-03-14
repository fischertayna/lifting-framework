module VarDFAExamples where

import Variability.VarTypes (Prop, VarValor(..), Var (Var), propA, propB, atbt, atbf, afbt, afbf, apply, substitute, mkBDDVar, notBDD, ttPC, ffPC, tt, ff, (/\), (\/), (|||))

expectedLvExample :: VarValor
expectedLvExample = VarPair (
        VarString (Var [("SEQ", ttPC)]),
        VarPair (
            VarPair (
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]),
                    VarPair (
                        VarString (Var [("x", ttPC)]),
                        VarPair (
                            VarString (Var [("CONST", ttPC)]),
                            VarString (Var [("2", ttPC)])
                        )
                    )
                )
            ),
            VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", ttPC)]),
                            VarPair (
                                VarString (Var [("y", ttPC)]),
                                VarPair (
                                    VarString (Var [("CONST", ttPC)]),
                                    VarString (Var [("4", ttPC)])
                                )
                            )
                        )
                    ),
                    VarPair (
                        VarString (Var [("SEQ", ttPC)]),
                        VarPair (
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("3", ttPC)]),
                                    VarPair (
                                        VarString (Var [("x", ttPC)]),
                                        VarPair (
                                            VarString (Var [("CONST", ttPC)]),
                                            VarString (Var [("1", ttPC)])
                                        )
                                    )
                                )
                            ),
                            VarPair (
                                VarString (Var [("SEQ", ttPC)]),
                                VarPair(
                                    VarPair (
                                        VarString (Var [("IF", ttPC)]),
                                        VarPair (
                                            VarString (Var [("4", ttPC)]),
                                            VarPair(
                                                VarPair (
                                                    VarString (Var [("GT", ttPC)]),
                                                    VarPair (
                                                        VarPair (
                                                            VarString (Var [("VAR", ttPC)]),
                                                            VarString (Var [("y", ttPC)])
                                                        ),
                                                        VarPair (
                                                            VarString (Var [("VAR", ttPC)]),
                                                            VarString (Var [("x", ttPC)])
                                                        )
                                                    )
                                                ),
                                                VarPair (
                                                    VarPair (
                                                        VarString (Var [("ASGN", ttPC)]),
                                                        VarPair (
                                                            VarString (Var [("5", ttPC)]),
                                                            VarPair (
                                                                VarString (Var [("z", ttPC)]),
                                                                VarPair (
                                                                    VarString (Var [("VAR", ttPC)]),
                                                                    VarString (Var [("y", ttPC)])
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    VarPair (
                                                        VarString (Var [("ASGN", ttPC)]),
                                                        VarPair (
                                                            VarString (Var [("6", ttPC)]),
                                                            VarPair (
                                                                VarString (Var [("z", ttPC)]),
                                                                VarPair (
                                                                    VarString (Var [("MULT", ttPC)]),
                                                                    VarPair (
                                                                        VarPair (
                                                                            VarString (Var [("VAR", ttPC)]),
                                                                            VarString (Var [("y", ttPC)])
                                                                        ),
                                                                        VarPair (
                                                                            VarString (Var [("VAR", ttPC)]),
                                                                            VarString (Var [("y", ttPC)])
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
                                    VarPair (
                                        VarString (Var [("ASGN", ttPC)]),
                                        VarPair (
                                            VarString (Var [("7", ttPC)]),
                                            VarPair (
                                                VarString (Var [("x", ttPC)]),
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("z", ttPC)])
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

lvExampleEntry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("6", ttPC)]), VarList[VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("7", ttPC)]), VarList[VarString (Var [("z", ttPC)])])]

lvExampleExit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[VarString (Var [("y", ttPC)])]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[VarString (Var [("z", ttPC)])]),
    VarPair(VarString (Var [("6", ttPC)]), VarList[VarString (Var [("z", ttPC)])]),
    VarPair(VarString (Var [("7", ttPC)]), VarList[])]

a_plus_b = VarPair (
                VarString (Var [("ADD", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("a", ttPC)])
                    ),
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("b", ttPC)])
                    )
                )
            )

a_mult_b = VarPair (
                VarString (Var [("MULT", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("a", ttPC)])
                    ),
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("b", ttPC)])
                    )
                )
            )

a_plus_1 = VarPair (
                VarString (Var [("ADD", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("a", ttPC)])
                    ),
                    VarPair (
                        VarString (Var [("CONST", ttPC)]),
                        VarString (Var [("1", ttPC)])
                    )
                )
            )

expectedAeExample :: VarValor
expectedAeExample = VarPair (
        VarString (Var [("SEQ", ttPC)]),
        VarPair (
            VarPair (
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]),
                    VarPair (
                        VarString (Var [("x", ttPC)]),
                        a_plus_b
                    )
                )
            ),
            VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", ttPC)]),
                            VarPair (
                                VarString (Var [("y", ttPC)]),
                                a_mult_b
                            )
                        )
                    ),
                    VarPair (
                        VarString (Var [("WHILE", ttPC)]),
                        VarPair (
                            VarString (Var [("3", ttPC)]),
                            VarPair (
                                VarPair (
                                    VarString (Var [("GT", ttPC)]),
                                    VarPair (
                                        VarPair (
                                            VarString (Var [("VAR", ttPC)]),
                                            VarString (Var [("y", ttPC)])
                                        ),
                                        a_plus_b
                                    )
                                ),
                                VarPair (
                                    VarString (Var [("SEQ", ttPC)]),
                                    VarPair (
                                        VarPair (
                                            VarString (Var [("ASGN", ttPC)]),
                                            VarPair (
                                                VarString (Var [("4", ttPC)]),
                                                VarPair (
                                                    VarString (Var [("a", ttPC)]),
                                                    a_plus_1
                                                )
                                            )
                                        ),
                                        VarPair (
                                            VarString (Var [("ASGN", ttPC)]),
                                            VarPair (
                                                VarString (Var [("5", ttPC)]),
                                                VarPair (
                                                    VarString (Var [("x", ttPC)]),
                                                    a_plus_b
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

aeExampleEntry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[a_plus_b]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[a_plus_b]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[a_plus_b]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[])]

aeExampleExit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[a_plus_b]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[a_plus_b, a_mult_b]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[a_plus_b]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[a_plus_b])]

a_minus_b = VarPair (
                VarString (Var [("SUB", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("a", ttPC)])
                    ),
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("b", ttPC)])
                    )
                )
            )

b_minus_a = VarPair (
                VarString (Var [("SUB", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("b", ttPC)])
                    ),
                    VarPair (
                        VarString (Var [("VAR", ttPC)]),
                        VarString (Var [("a", ttPC)])
                    )
                )
            )

expectedVbExample :: VarValor
expectedVbExample = VarPair (
        VarString (Var [("IF", ttPC)]),
        VarPair (
            VarString (Var [("1", ttPC)]),
            VarPair (
                VarPair (
                    VarString (Var [("GT", ttPC)]),
                    VarPair (
                        VarPair (
                            VarString (Var [("VAR", ttPC)]),
                            VarString (Var [("a", ttPC)])
                        ),
                        VarPair (
                            VarString (Var [("VAR", ttPC)]),
                            VarString (Var [("b", ttPC)])
                        )
                    )
                ),
                VarPair (
                    VarPair (
                        VarString (Var [("SEQ", ttPC)]),
                        VarPair (
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("2", ttPC)]),
                                    VarPair (
                                        VarString (Var [("x", ttPC)]),
                                        b_minus_a
                                    )
                                )
                            ),
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("3", ttPC)]),
                                    VarPair (
                                        VarString (Var [("y", ttPC)]),
                                        a_minus_b
                                    )
                                )
                            )
                        )
                    ),
                    VarPair (
                        VarString (Var [("SEQ", ttPC)]),
                        VarPair (
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("4", ttPC)]),
                                    VarPair (
                                        VarString (Var [("y", ttPC)]),
                                        b_minus_a
                                    )
                                )
                            ),
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("5", ttPC)]),
                                    VarPair (
                                        VarString (Var [("x", ttPC)]),
                                        a_minus_b
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

vbExampleEntry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[a_minus_b, b_minus_a]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[a_minus_b, b_minus_a]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[a_minus_b]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[a_minus_b, b_minus_a]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[a_minus_b])]

vbExampleExit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[a_minus_b, b_minus_a]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[a_minus_b]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[a_minus_b]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[])]

expectedFactorial :: VarValor
expectedFactorial = VarPair (
        VarString (Var [("SEQ", ttPC)]),
        VarPair (
            VarPair (
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
            VarPair (
                VarString (Var [("SEQ", ttPC)]),
                VarPair (
                    VarPair (
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", ttPC)]),
                            VarPair (
                                VarString (Var [("z", ttPC)]),
                                VarPair (
                                    VarString (Var [("CONST", ttPC)]),
                                    VarString (Var [("1", ttPC)])
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
                                    VarString (Var [("3", ttPC)]),
                                    VarPair (
                                        VarPair (
                                            VarString (Var [("GT", ttPC)]),
                                            VarPair (
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("y", ttPC)])
                                                ),
                                                VarPair (
                                                    VarString (Var [("CONST", ttPC)]),
                                                    VarString (Var [("1", ttPC)])
                                                )
                                            )
                                        ),
                                        VarPair (
                                            VarString (Var [("SEQ", ttPC)]),
                                            VarPair (
                                                VarPair (
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
                                                VarPair (
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
                                                                    VarPair (
                                                                        VarString (Var [("CONST", ttPC)]),
                                                                        VarString (Var [("1", ttPC)])
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
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("6", ttPC)]),
                                    VarPair (
                                        VarString (Var [("y", ttPC)]),
                                        VarPair (
                                            VarString (Var [("CONST", ttPC)]),
                                            VarString (Var [("0", ttPC)])
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

expectedPower :: VarValor
expectedPower = VarPair (
        VarString (Var [("SEQ", ttPC)]),
        VarPair (
            VarPair (
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]),
                    VarPair (
                        VarString (Var [("z", ttPC)]),
                        VarPair (
                            VarString (Var [("CONST", ttPC)]),
                            VarString (Var [("1", ttPC)])
                        )
                    )
                )
            ),
            VarPair (
                VarString (Var [("WHILE", ttPC)]),
                VarPair (
                    VarString (Var [("2", ttPC)]),
                    VarPair (
                        VarPair (
                            VarString (Var [("GT", ttPC)]),
                            VarPair (
                                VarPair (
                                    VarString (Var [("VAR", ttPC)]),
                                    VarString (Var [("x", ttPC)])
                                ),
                                VarPair (
                                    VarString (Var [("CONST", ttPC)]),
                                    VarString (Var [("0", ttPC)])
                                )
                            )
                        ),
                        VarPair (
                            VarString (Var [("SEQ", ttPC)]),
                            VarPair (
                                VarPair (
                                    VarString (Var [("ASGN", ttPC)]),
                                    VarPair (
                                        VarString (Var [("3", ttPC)]),
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
                                VarPair (
                                    VarString (Var [("ASGN", ttPC)]),
                                    VarPair (
                                        VarString (Var [("4", ttPC)]),
                                        VarPair (
                                            VarString (Var [("x", ttPC)]),
                                            VarPair (
                                                VarString (Var [("SUB", ttPC)]),
                                                VarPair (
                                                    VarPair (
                                                        VarString (Var [("VAR", ttPC)]),
                                                        VarString (Var [("x", ttPC)])
                                                    ),
                                                    VarPair (
                                                        VarString (Var [("CONST", ttPC)]),
                                                        VarString (Var [("1", ttPC)])
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

-- x = 1;      1
-- y = 2;      2
-- y = 4;      3
-- z = x + y;  4

-- x = 1;  tt    1
-- y = 2;  tt    2
-- y = 4;  tt    3
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
                VarString (Var [("SEQ", ttPC)]), -- Should it be A??
                VarPair ( 
                    VarPair (
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", ttPC)]),
                            VarPair (
                                VarString (Var [("y", ttPC)]),
                                VarPair(
                                    VarString (Var [("INT", ttPC)]),
                                    VarString (Var [("1", ttPC)])
                                )
                            )
                        )
                    ), -- y = 2;    2
                    VarPair(
                        VarString (Var [("SEQ", ttPC)]), 
                        VarPair(
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("3", ttPC)]),
                                    VarPair (
                                        VarString (Var [("y", ttPC)]),
                                        VarPair(
                                            VarString (Var [("INT", ttPC)]),
                                            VarString (Var [("4", ttPC)])
                                        )
                                    )
                                )
                            ), -- y = 4;    3
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

ex2_1 = VarPair (
            VarString (Var [("ASGN", ttPC)]),
            VarPair (
                VarString (Var [("1", ttPC)]),
                VarPair (
                    VarString (Var [("x", ttPC)]),
                    VarPair(
                        VarString (Var [("CONST", ttPC)]),
                        VarString (Var [("1", ttPC)])
                    )
                )
            )
        ) -- x = 1;  tt    1

ex2_2 = VarPair (
            VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
            VarPair (
                VarString (Var [("2", propA), ("-2", notBDD propA)]),
                VarPair (
                    VarString (Var [("y", propA)]),
                    VarPair(
                        VarString (Var [("CONST", propA)]),
                        VarString (Var [("1", propA)])
                    )
                )
            )
        ) -- y = 2;  A 2 / skip; ~A 21

ex2_3 = VarPair (
            VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
            VarPair (
                VarString (Var [("3", notBDD propA), ("-3", propA)]),
                VarPair (
                    VarString (Var [("y", notBDD propA)]),
                    VarPair(
                        VarString (Var [("CONST", notBDD propA)]),
                        VarString (Var [("4", notBDD  propA)])
                    )
                )
            )
        ) -- y = 4;  ~A 3 / skip; A 31

ex2_4 = VarPair(
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

ex2_var = VarPair(
            VarString (Var [("SEQ", ttPC)]),
            VarPair(
                ex2_2,
                ex2_3
            )
        )

-- x = 1;      1
-- #IFDEF A
-- y = 2;      2 
-- #ELSE
-- y = 4;      3
-- #ENDIF
-- z = x + y;  4

-- x = 1;  tt    1
-- y = 2;  A 2   / skip ~A   21
-- y = 4;  ~A 3  / skip A   31
-- z = x + y;  tt 4
ex2 = VarPair(
        VarString (Var [("SEQ", ttPC)]),
        VarPair ( 
            ex2_1,
            VarPair(
                VarString (Var [("SEQ", ttPC)]),
                VarPair(
                    ex2_var,
                    ex2_4
                )
            )
        )
    )

ex2Entry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]) ]

ex2Exit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("4", ttPC)])) ]) ]

rdExampleEntry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ])]

rdExampleExit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])) ]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ])]

propUnsafeDiv :: Prop
propUnsafeDiv = mkBDDVar "UNSAFE_DIV"

runningRDEntry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", propUnsafeDiv), ("-3", notBDD propUnsafeDiv)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("4", notBDD propUnsafeDiv), ("-4", propUnsafeDiv)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", notBDD propUnsafeDiv)]), VarString (Var [("2", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", propUnsafeDiv)]), VarString (Var [("3", propUnsafeDiv)])), 
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", propUnsafeDiv)]), VarString (Var [("1", propUnsafeDiv)])),
                            VarPair(VarString (Var [("x", notBDD propUnsafeDiv)]), VarString (Var [("4", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", notBDD propUnsafeDiv)]), VarString (Var [("2", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", propUnsafeDiv)]), VarString (Var [("3", propUnsafeDiv)])), 
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])]

runningRDExit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", propUnsafeDiv), ("-3", notBDD propUnsafeDiv)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", notBDD propUnsafeDiv)]), VarString (Var [("2", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", propUnsafeDiv)]), VarString (Var [("3", propUnsafeDiv)])), 
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("4", notBDD propUnsafeDiv), ("-4", propUnsafeDiv)]), VarList[
                            VarPair(VarString (Var [("x", propUnsafeDiv)]), VarString (Var [("1", propUnsafeDiv)])),
                            VarPair(VarString (Var [("x", notBDD propUnsafeDiv)]), VarString (Var [("4", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", notBDD propUnsafeDiv)]), VarString (Var [("2", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", propUnsafeDiv)]), VarString (Var [("3", propUnsafeDiv)])), 
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", propUnsafeDiv)]), VarString (Var [("1", propUnsafeDiv)])),
                            VarPair(VarString (Var [("x", notBDD propUnsafeDiv)]), VarString (Var [("4", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", notBDD propUnsafeDiv)]), VarString (Var [("2", notBDD propUnsafeDiv)])), 
                            VarPair(VarString (Var [("y", propUnsafeDiv)]), VarString (Var [("3", propUnsafeDiv)])),
                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("5", ttPC)])) ])]

