-- | Provides the 'Union' type class.
--
-- @since 0.1
module Containers.Class.Union
  ( Union (..),
    (∪),
    (⋃),
  )
where

import Containers.Class.Empty (Empty, (∅))
import Containers.Class.Internal (HashCompat)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set

-- | Types that have a notion of union.
--
-- @since 0.1
class Union a where
  -- | @since 0.1
  union :: a -> a -> a

-- | Operator alias for 'union'. U+222A.
--
-- @since 0.1
(∪) :: Union a => a -> a -> a
(∪) = union

infixl 6 ∪

-- | Fold over 'union'.
--
-- @since 0.1
(⋃) :: (Empty a, Foldable f, Union a) => f a -> a
(⋃) = foldr (∪) (∅)

-- | @since 0.1
instance Union [a] where
  union = (++)

-- | @since 0.1
instance HashCompat k => Union (HashMap k v) where
  union = HMap.union

-- | @since 0.1
instance HashCompat a => Union (HashSet a) where
  union = HSet.union

-- | @since 0.1
instance Union (IntMap a) where
  union = IntMap.union

-- | @since 0.1
instance Union IntSet where
  union = IntSet.union

-- | @since 0.1
instance Ord k => Union (Map k v) where
  union = Map.union

-- | @since 0.1
instance Union (Seq a) where
  union = (<>)

-- | @since 0.1
instance Ord k => Union (Set k) where
  union = Set.union
