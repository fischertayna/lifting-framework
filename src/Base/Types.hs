module Base.Types where

data Valor
    = ValorInt
        { i :: Integer
        }
    | ValorBool
        { b :: Bool
        }
    | ValorStr
        { s :: String
        }
    | ValorList
        { l :: [Valor]
        }
    | ValorPair
        { p :: (Valor, Valor)
        }
    deriving (Show, Eq, Ord)

type Context k v = [(k, v)]