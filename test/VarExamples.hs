module VarExamples where

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
                            VarPair(
                                VarString (Var [("SEQ", ttPC)]),
                                VarPair(
                                    VarPair (
                                    VarString (Var [("IF", ttPC)]),
                                    VarPair (
                                        VarPair (
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
                                            VarString (Var [("4", ttPC)])
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
                        VarPair (
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
                            )
                        )
                    ),
                    VarPair (
                        VarString (Var [("WHILE", ttPC)]),
                        VarPair (
                            VarPair (
                                VarPair (
                                    VarString (Var [("GT", ttPC)]),
                                    VarPair (
                                        VarPair (
                                            VarString (Var [("VAR", ttPC)]),
                                            VarString (Var [("y", ttPC)])
                                        ),
                                        VarPair (
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
                                    )
                                ),
                                VarString (Var [("3", ttPC)])
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
                                                VarPair (
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
                                            )
                                        )
                                    ),
                                    VarPair (
                                        VarString (Var [("ASGN", ttPC)]),
                                        VarPair (
                                            VarString (Var [("5", ttPC)]),
                                            VarPair (
                                                VarString (Var [("x", ttPC)]),
                                                VarPair (
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

expectedVbExample :: VarValor
expectedVbExample = VarPair (
        VarString (Var [("IF", ttPC)]),
        VarPair (
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
                VarString (Var [("1", ttPC)])
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
                                    VarPair (
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
                                )
                            )
                        ),
                        VarPair (
                            VarString (Var [("ASGN", ttPC)]),
                            VarPair (
                                VarString (Var [("3", ttPC)]),
                                VarPair (
                                    VarString (Var [("y", ttPC)]),
                                    VarPair (
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
                                    VarPair (
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
                                )
                            )
                        ),
                        VarPair (
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
                                                VarString (Var [("a", ttPC)])
                                            ),
                                            VarPair (
                                                VarString (Var [("VAR", ttPC)]),
                                                VarString (Var [("b", ttPC)])
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
                                        VarString (Var [("3", ttPC)])
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
                        VarString (Var [("2", ttPC)])
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

ex2_1 = VarPair (
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
        ) -- x = 1;  tt    1

ex2_2 = VarPair (
            VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
            VarPair (
                VarString (Var [("2", propA), ("21", notBDD propA)]),
                VarPair (
                    VarString (Var [("y", propA), ("DUMMY", notBDD propA)]),
                    VarPair(
                        VarString (Var [("INT", propA), ("DUMMY", notBDD propA)]),
                        VarString (Var [("1", propA), ("DUMMY", notBDD propA)])
                    )
                )
            )
        ) -- y = 2;  A 2 / skip; ~A 21

ex2_3 = VarPair (
            VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
            VarPair (
                VarString (Var [("3", notBDD propA), ("31", propA)]),
                VarPair (
                    VarString (Var [("y", notBDD propA), ("DUMMY", propA)]),
                    VarPair(
                        VarString (Var [("INT", notBDD propA), ("DUMMY", propA)]),
                        VarString (Var [("4", notBDD  propA), ("DUMMY", propA)])
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

ex2_s1 = VarPair(
            VarString (Var [("SEQ", ttPC)]), -- Should it be ~A??
            VarPair(
                ex2_3,
                ex2_4
            )
        )

ex2_s2 = VarPair(
                VarString (Var [("SEQ", ttPC)]), -- Should it be A??
                VarPair ( 
                    ex2_2,
                    ex2_s1
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
            ex2_s2
        )
    )