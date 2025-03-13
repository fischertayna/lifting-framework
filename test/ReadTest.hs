module ReadTest where

import Test.HUnit
import Variability.VarTypes

test_andBDD_basic :: Test
test_andBDD_basic = TestLabel "andBDD identity and name reuse" $ TestList
  [ TestCase $ assertEqual "A && A == A (same BDD, same name)"
      (andBDD propA propA)
      propA
  , TestCase $ assertEqual "A && tt == A"
      (andBDD propA tt)
      propA
  , TestCase $ assertEqual "tt && A == A"
      (andBDD tt propA)
      propA
  , TestCase $ assertEqual "A && ff == ff"
      (andBDD propA ff)
      ff
  , TestCase $ assertEqual "ff && A == ff"
      (andBDD ff propA)
      ff
  ]

test_orBDD_basic :: Test
test_orBDD_basic = TestLabel "orBDD identity and name reuse" $ TestList
  [ TestCase $ assertEqual "A || A == A"
      (orBDD propA propA)
      propA
  , TestCase $ assertEqual "A || ff == A"
      (orBDD propA ff)
      propA
  , TestCase $ assertEqual "ff || A == A"
      (orBDD ff propA)
      propA
  , TestCase $ assertEqual "A || tt == tt"
      (orBDD propA tt)
      tt
  , TestCase $ assertEqual "tt || A == tt"
      (orBDD tt propA)
      tt
  ]

test_notBDD_basic :: Test
test_notBDD_basic = TestLabel "notBDD simplification and name correctness" $ TestList
  [ TestCase $ assertEqual "~~A == A (double negation name simplified)"
      (notBDD (notBDD propA))
      propA
  , TestCase $ assertEqual "~tt == ff"
      (notBDD tt)
      ff
  , TestCase $ assertEqual "~ff == tt"
      (notBDD ff)
      tt
  ]

test_serialization :: Test
test_serialization = TestList
  [ TestCase $ assertEqual "read . show for Prop"
      (reads (show tt) :: [(Prop, String)])
      [(tt, "")]
  , TestCase $ assertEqual "read . show for Var Integer"
      (reads (show (Var [(42, tt), (7, ff)])) :: [(Var Integer, String)])
      [(Var [(42, tt), (7, ff)], "")]
  , TestCase $ assertEqual "read . show for VarValor"
      (reads (show (VarInteger (Var [(3, tt), (4, ff)]))) :: [(VarValor, String)])
      [(VarInteger (Var [(3, tt), (4, ff)]), "")]
  , TestCase $ assertEqual "read . show for complex VarValor list"
      (reads (show (VarList [VarInteger (Var [(1, propA)]), VarInteger (Var [(2, propB)])])) :: [(VarValor, String)])
      [(VarList [VarInteger (Var [(1, propA)]), VarInteger (Var [(2, propB)])], "")]
  ]