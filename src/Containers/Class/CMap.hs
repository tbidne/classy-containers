-- | Provides the 'CMap' type class.
--
-- @since 0.1
module Containers.Class.CMap
  ( CMap (..),
    φ,
    CFunctor (..),
  )
where

import Containers.Class.Internal (HashCompat)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.IntMap (IntMap)
import Data.Kind (Constraint)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set

-- | Generalized 'fmap'.
--
-- @since 0.1
class CMap f where
  -- | Constraint on the image, if any.
  type CMapC f a :: Constraint

  cmap :: CMapC f b => (a -> b) -> f a -> f b

-- | Alias for 'cmap'. U+03C6.
--
-- @since 0.1
φ :: (CMap f, CMapC f b) => (a -> b) -> f a -> f b
φ = cmap

-- | Adds a 'CMap' instance to any 'Functor'.
--
-- @since 0.1
newtype CFunctor f a = MkCFunctor {unCFunctor :: f a}
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Functor,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Functor f => CMap (CFunctor f) where
  type CMapC (CFunctor f) _ = ()
  cmap = fmap

-- | @since 0.1
instance CMap [] where
  type CMapC [] _ = ()
  cmap = fmap

-- | @since 0.1
instance CMap HashSet where
  type CMapC HashSet a = HashCompat a
  cmap = HSet.map

-- | @since 0.1
instance CMap (HashMap k) where
  type CMapC (HashMap k) _ = ()
  cmap = fmap

-- TODO: is this possible?

-- | @since 0.1
-- instance CMap IntSet where
--  type CMapC IntSet _ = ()
--  cmap = IntSet.map

-- | @since 0.1
instance CMap IntMap where
  type CMapC IntMap _ = ()
  cmap = fmap

-- | @since 0.1
instance CMap (Map k) where
  type CMapC (Map k) _ = ()
  cmap = fmap

instance CMap Seq where
  type CMapC Seq _ = ()
  cmap = fmap

-- | @since 0.1
instance CMap Set where
  type CMapC Set a = Ord a
  cmap = Set.map
