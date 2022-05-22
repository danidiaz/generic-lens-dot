{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Data.Generics.Product.Fields.Dot (FieldLens, pry, pry2, The, the) where

import Control.Lens (Lens, Lens')
import Control.Lens.Unsound (lensProduct)
import Data.Generics.Product.Fields qualified as G
import Data.Kind (Constraint, Type)
import Data.Type.Equality ( type (==) )
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.TypeLits ( TypeError, Symbol, ErrorMessage(Text) )

-- Basically a "Control.Lens.Reified.ReifiedLens" which also tracks the names of
-- the fields the lens goes through.
newtype FieldLens s t (path :: [Symbol]) a b = FieldLens (Lens s t a b)

pry :: FieldLens s t path a b -> Lens s t a b
pry (FieldLens l) = l

pry2 :: NonOverlapping2 path1 path2 => FieldLens s s path1 a a -> FieldLens s s path2 b b -> Lens' s (a, b)
pry2 (FieldLens l1) (FieldLens l2) = lensProduct l1 l2

type NonOverlapping2 :: [Symbol] -> [Symbol] -> Constraint
type family NonOverlapping2 path1 path2 where
  NonOverlapping2 (p1 ': rest1) (p2 ': rest2) = NonOverlapping2' (p1 == p2) rest1 rest2
  NonOverlapping2 _ _ = TypeError (Text "focuses overlap")

type NonOverlapping2' :: Bool -> [Symbol] -> [Symbol] -> Constraint
type family NonOverlapping2' b path1 path2 where
  NonOverlapping2' False _ _ = ()
  NonOverlapping2' True path1 path2 = NonOverlapping2 path1 path2

-- Just a dummy starting point for applying the overloaded dot.
type The :: Type -> Type -> Type
data The s t = The

the :: The s t
the = The

type Append :: [Symbol] -> Symbol -> [Symbol]
type family Append path t where
  Append '[] t = '[t]
  Append (head ': tail) t = head ': Append tail t

-- This GHC.Records.HasField produces lenses, not values.
instance G.HasField (field :: Symbol) s t a b => HasField field (The s t) (FieldLens s t '[field] a b) where
  getField _ = FieldLens (G.field @field)

instance (G.HasField (field :: Symbol) s t a b, path' ~ Append path field) => HasField field (FieldLens u v path s t) (FieldLens u v path' a b) where
  getField (FieldLens l) = FieldLens (l . G.field @field)

