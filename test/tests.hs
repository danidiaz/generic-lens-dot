{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Lens (Lens, Lens', (%~), (&), (.~), (^.))
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.TypeLits
import Data.Generics.Product.Fields.Dot
import Data.Tuple (swap)

-- example taken from https://hackage.haskell.org/package/generic-lens-2.2.1.0/docs/Data-Generics-Product-Fields.html
data Org a = Org {director :: Human a, foo :: Int} deriving (Generic, Show)

data Human a
  = Human
      { name :: String,
        address :: String,
        other :: a
      }
  | HumanNoAddress
      { name :: String,
        other :: a
      }
  deriving (Generic, Show)

human :: Human Bool
human = Human {name = "Tunyasz", address = "London", other = False}

human' :: Human Int
human' = human & pry the.other .~ (42 :: Int)

org :: Org Bool
org = Org human 0

org' :: Org Int
org' = org & pry the.director.other .~ 42

--
--
data Person = Person
  { personName :: String,
    age :: Int,
    petName :: String
  }
  deriving (Generic, Show)

person :: Person
person = Person "John" 55 "Fido"

--
main :: IO ()
main = do
  print human
  print human'
  print org
  print org'
  print $ org ^. pry the.director.other
  print $ person & pry2 the.personName the.petName %~ Data.Tuple.swap
  pure ()