-- | Provides the 'Difference' type class.
--
-- @since 0.1
module Containers.Class.Difference
  ( Difference (..),
    (∖),
    (∆),
  )
where

import Containers.Class.Union (Union, (∪))
import Data.Foldable (Foldable (foldl'))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq, (<|))
import Data.Set (Set)
import Data.Set qualified as Set

-- | Types that have a notion of difference.
--
-- @since 0.1
class Difference a where
  -- | @since 0.1
  difference :: a -> a -> a

-- | Operator alias for 'difference'. U+2216.
--
-- @since 0.1
(∖) :: Difference a => a -> a -> a
(∖) = difference

infixl 6 ∖

-- | Symmetric difference.
--
-- @since 0.1
(∆) :: (Difference a, Union a) => a -> a -> a
x ∆ y = (x ∖ y) ∪ (y ∖ x)

infixl 6 ∆

-- | @since 0.1
instance Eq a => Difference [a] where
  difference = diffViaFold (:)

-- | @since 0.1
instance Hashable k => Difference (HashMap k v) where
  difference = HMap.difference

-- | @since 0.1
instance Hashable a => Difference (HashSet a) where
  difference = HSet.difference

-- | @since 0.1
instance Difference (IntMap a) where
  difference = IntMap.difference

-- | @since 0.1
instance Difference IntSet where
  difference = IntSet.difference

-- | @since 0.1
instance Ord k => Difference (Map k v) where
  difference = Map.difference

-- | @since 0.1
instance Eq a => Difference (Seq a) where
  difference = diffViaFold (<|)

-- | @since 0.1
instance Ord k => Difference (Set k) where
  difference = Set.difference

-- NOTE: Performance here is obviously not great. If we restrict to 'Ord' we
-- can use an intermediate Set to improve this.
diffViaFold :: (Eq a, Foldable f, Monoid b) => (a -> b -> b) -> f a -> f a -> b
diffViaFold prepend xs ys = foldl' go mempty xs
  where
    go acc x
      | x `elem` ys = prepend x acc
      | otherwise = acc
