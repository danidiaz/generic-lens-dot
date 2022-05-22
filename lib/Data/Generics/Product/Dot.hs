{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Data.Generics.Product.Dot (WrappedLens, pry, pry2, The, the) where

import Control.Applicative
import Control.Lens (Lens, Lens')
import Control.Lens.Unsound (lensProduct)
import Data.Function
import Data.Functor
import Data.Generics.Product.Fields qualified as G
import Data.Kind (Constraint, Type)
import Data.Type.Equality
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.TypeLits

-- Basically a 'Control.Lens.Reified.ReifiedLens'.
newtype WrappedLens s t (path :: [Symbol]) a b = WrappedLens (Lens s t a b)

pry :: WrappedLens s t path a b -> Lens s t a b
pry (WrappedLens l) = l

pry2 :: FocusesShouldNotOverlap2 path1 path2 => WrappedLens s s path1 a a -> WrappedLens s s path2 b b -> Lens' s (a, b)
pry2 (WrappedLens l1) (WrappedLens l2) = lensProduct l1 l2

type FocusesShouldNotOverlap2 :: [Symbol] -> [Symbol] -> Constraint
type family FocusesShouldNotOverlap2 path1 path2 where
  FocusesShouldNotOverlap2 (p1 ': rest1) (p2 ': rest2) = FocusesShouldNotOverlap2' (p1 == p2) rest1 rest2
  FocusesShouldNotOverlap2 _ _ = TypeError (Text "focuses overlap")

type FocusesShouldNotOverlap2' :: Bool -> [Symbol] -> [Symbol] -> Constraint
type family FocusesShouldNotOverlap2' b path1 path2 where
  FocusesShouldNotOverlap2' False _ _ = ()
  FocusesShouldNotOverlap2' True path1 path2 = FocusesShouldNotOverlap2 path1 path2

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
instance G.HasField (field :: Symbol) s t a b => HasField field (The s t) (WrappedLens s t '[field] a b) where
  getField _ = WrappedLens (G.field @field)

instance (G.HasField (field :: Symbol) s t a b, path' ~ Append path field) => HasField field (WrappedLens u v path s t) (WrappedLens u v path' a b) where
  getField (WrappedLens l) = WrappedLens (l . G.field @field)

