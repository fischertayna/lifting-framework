module Base.Types where

import Data.Hashable (Hashable(hashWithSalt))

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
    deriving (Show, Eq, Ord, Read)

type Context k v = [(k, v)]

instance Hashable Valor where
  hashWithSalt salt (ValorInt i) = hashWithSalt salt i
  hashWithSalt salt (ValorBool b) = hashWithSalt salt b
  hashWithSalt salt (ValorStr s)  = hashWithSalt salt s
  hashWithSalt salt (ValorList vs) = hashWithSalt salt vs
  hashWithSalt salt (ValorPair (v1, v2)) = salt `hashWithSalt` v1 `hashWithSalt` v2