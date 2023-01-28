{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Memoization.Examples.GuidedFibonacci (fibMemoized) where

import Memoization.Core.Memory (KeyValueArray, retrieveOrRun)
import Memoization.Core.State (State, (<.>))

-- Primeiro, vamos começar com a definição da função de Fibonacci

cmap :: (a -> b) -> [a] -> [b]
cmap _ [] = []
cmap f (x : xs) = f x : map f xs

-- Esta função foi extraída para não termos que lidar ainda com a regra lambda de reescrita, que
-- é um pouco chatinha...
applyFToRange :: (Num a, Enum a) => (a -> b) -> a -> [b]
applyFToRange f n = cmap f [0 .. n]

fib :: Int -> Int
fib n = 1 + sum (applyFToRange fib (n - 2))

-- Vamos lá!
-- Inicialmente, vemos que o primeiro passo é a aplicação da função (+) nos parâmetros (1) e (sum (applyFToRange fib (n - 2)))
-- Precisamos então ter os tipos liftados de (+), (1) e (sum (applyFToRange fib (n - 2)))
-- para aplicar a regra `apply` com (+) <.> (1) <.> (sum (applyFToRange fib (n - 2))).
-- Conseguimos aplicar a regra `pure` tanto em (+) quanto em (1), pois:
-- As assinaturas de tipos correspondem ao que é aceito na regra `pure`; e
-- Nenhum dos subtermos de (+) e (1) está no domínio de `tau` (que é `fib` e `n`)

plusM ::
  State
    m
    ( Int ->
      State
        m
        (Int -> State m Int)
    )
plusM = return (\a -> return (\b -> return ((+) a b)))

oneM :: State m Int
oneM = return 1

-- No termo (sum (applyFToRange fib (n - 2))), nos deparamos com uma outra aplicação. Vamos fazer analogamente ao caso acima do plus:
sumM ::
  State
    m
    ([Int] -> State m Int)
sumM = return (\a -> return (sum a))

-- Mesma coisa com o applyFToRange fib (n - 2), só que aqui temos que ir mais fundo e liftar o applyFToRange
-- Vou fazer um pequeno atalho para ter o applyFToRange liftado:
-- (applyFToRange f n = cmap f [0 .. n])
-- Inicialmente, começamos com o cmap:
cmapM :: State m ((a -> State m b) -> State m ([a] -> State m [b]))
cmapM =
  return
    ( \f ->
        return
          ( \case
              [] -> return []
              (x : xs) -> return (\x1 -> return (\x2 -> return (x1 : x2))) <.> f x <.> (cmapM <.> return f <.> return xs)
          )
    )

-- Depois, com o rangeFromTo (neste caso, o [..])
rangeFromToM ::
  State
    m
    ( Int ->
      State
        m
        ( Int ->
          State m [Int]
        )
    )
rangeFromToM = return (\a -> return (\b -> return [a .. b]))

zeroM :: State m Int
zeroM = return 0

-- Com o cmapM, rangeFromToM e o zeroM, conseguimos agora fazer o applyFToRangeM:
applyFToRangeM ::
  State
    m
    ( (Int -> State m Int) ->
      State m (Int -> State m [Int])
    )
applyFToRangeM =
  return
    ( \f ->
        return
          ( \n ->
              cmapM <.> return f <.> (rangeFromToM <.> zeroM <.> return n)
          )
    )

-- Agora só precisamos liftar o (n - 2) para conseguirmos usar a regra `apply` no `sum`
minusM ::
  State
    m
    ( Int ->
      State
        m
        (Int -> State m Int)
    )
minusM = return (\t0 -> return (\t1 -> return (t0 - t1)))

twoM :: State m Int
twoM = return 2

-- Praticamente agora temos tudo o que precisamos pra sair reescrevento usando as regras de `apply`
-- Um pequeno problema que eu não entendi é como fazer quando uma subexpressão usa um parâmetro (como o `n`) que foi definido somente
-- anteriormente.
-- Isso vai ser um desafio na hora de fazer a reescrita de forma programática, pois a única forma que vi foi fazer tudo DIRETAÇO.

-- Desta forma, temos o Fib. Porem, este é o fib Monadificado!.
fibM :: State m (Int -> State m Int)
fibM =
  return
    ( \n ->
        plusM <.> oneM <.> (sumM <.> (applyFToRangeM <.> fibM <.> (minusM <.> return n <.> twoM)))
    )

-- O fib Monadificado e Memoizado é obtido usando a marotagem do `retrieveOrRun`:
fibMemoized :: State (KeyValueArray Int Int) (Int -> State (KeyValueArray Int Int) Int)
fibMemoized = return (\n -> retrieveOrRun n (\_ -> plusM <.> oneM <.> (sumM <.> (applyFToRangeM <.> fibMemoized <.> (minusM <.> return n <.> twoM)))))

-- Quais seriam as diretivas de pré-processamento necessárias?
-- A mais comum é verificar que tipo de expressão estamos liftando:
    -- Se estamos liftando uma constante
    -- Se estamos liftando um identificador
    -- Se estamos liftando uma aplicação de função
    -- Se estamos liftando um `case`

-- Em uma constante: precisamos de uma primitiva para encapsulá-la em um return
-- Para um identificador, precisamos verificar se já temos a versão monadificada dele. 
-- Caso não tenha, precisamos da estrutura interna do identificador para reescrevê-lo.

-- Na aplicação de função, precisamos ver o identificador da função e analisar as expressões recursivas

-- Em um case, tenho que pensar como seria. A marotagem do \case pode ajudar caso o case seja aplicado em um dos parâmetros.