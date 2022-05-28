{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Generics.Product.Fields.Dot (FieldLens, pry, pry2, The, the) where

import Control.Lens (Lens, Lens', Prism, Prism')
import Control.Lens.Unsound (lensProduct)
import Data.Generics.Product.Fields qualified as G
import Data.Generics.Sum.Constructors qualified as G
import Data.Kind (Constraint, Type)
import Data.Type.Equality ( type (==) )
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.TypeLits ( TypeError, Symbol, ErrorMessage(Text) )

-- Basically a "Control.Lens.Reified.ReifiedLens" which also tracks the names of
-- the fields the lens goes through.
newtype FieldLens s t (path :: [Symbol]) a b = FieldLens (Lens s t a b)

newtype BranchPrism s t (path :: [Symbol]) a b = BranchPrism (Prism s t a b)

pry :: FieldLens s t path a b -> Lens s t a b
pry (FieldLens l) = l

-- | A safer "Control.Lens.Unsound.lensProduct" which fails to compile if the focuses of the two lenses overlap. 
pry2 :: NonOverlapping2 path1 path2 => FieldLens s s path1 a a -> FieldLens s s path2 b b -> Lens' s (a, b)
pry2 (FieldLens l1) (FieldLens l2) = lensProduct l1 l2

descry :: BranchPrism s t path a b -> Prism s t a b
descry (BranchPrism p) = p

type NonOverlapping2 :: [Symbol] -> [Symbol] -> Constraint
type family NonOverlapping2 path1 path2 where
  NonOverlapping2 (p1 ': rest1) (p2 ': rest2) = NonOverlapping2' (p1 == p2) rest1 rest2
  NonOverlapping2 _ _ = TypeError (Text "focuses overlap")

type NonOverlapping2' :: Bool -> [Symbol] -> [Symbol] -> Constraint
type family NonOverlapping2' b path1 path2 where
  NonOverlapping2' False _ _ = ()
  NonOverlapping2' True path1 path2 = NonOverlapping2 path1 path2

-- Just a dummy starting point for applying the overloaded dot.
type The :: (Type -> Type -> [Symbol] -> Type -> Type -> Type) -> Type -> Type -> Type
data The optic s t = The

the :: The optic s t
the = The

type Append :: [Symbol] -> Symbol -> [Symbol]
type family Append path t where
  Append '[] t = '[t]
  Append (head ': tail) t = head ': Append tail t

-- type HasOptic :: (Type -> Type -> [Symbol] -> Type -> Type -> Type)  -> Symbol -> Type -> Type -> Type -> Type -> Constraint
-- class HasOptic optic (field :: Symbol) s t a b | s field -> a, t field -> b, s field b -> t, t field a -> s, s -> optic, t -> optic where
--   optic :: optic s t a b

class HasField' optic x r a | x r -> a, r -> optic where
  getField' :: r -> a

instance (G.HasField (field :: Symbol) s t a b) => HasField' FieldLens field (The FieldLens s t) (FieldLens s t '[field] a b) where
  getField' _ = FieldLens (G.field @field)


instance (G.HasField (field :: Symbol) s t a b, HasField' o field (The o s t) (o s t '[field] a b)) => HasField field (The o s t) (o s t '[field] a b) where
  getField = getField' @o @field


-- This GHC.Records.HasField produces lenses, not values.
-- instance (G.HasField (field :: Symbol) s t a b) => HasField field (The FieldLens s t) (FieldLens s t '[field] a b) where
--   getField _ = undefined

instance (G.HasField (field :: Symbol) s t a b, path' ~ Append path field) => HasField field (FieldLens u v path s t) (FieldLens u v path' a b) where
  getField (FieldLens l) = FieldLens (l . G.field @field)


instance (G.AsConstructor  (branch :: Symbol) s t a b) => HasField branch (The BranchPrism s t) (BranchPrism s t '[branch] a b) where
  getField _ = BranchPrism (G._Ctor @branch)



