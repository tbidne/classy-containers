-- | Provides the 'Member' type class.
--
-- @since 0.1
module Containers.Class.Member
  ( Member (..),
    (∈),
    (∉),
  )
where

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

-- | Types that have a notion of member.
--
-- @since 0.1
class Member a where
  type MElem a

  -- | @since 0.1
  member :: MElem a -> a -> Bool

-- | @since 0.1
instance Eq a => Member [a] where
  type MElem [a] = a
  member = elem

-- | @since 0.1
instance HashCompat k => Member (HashMap k v) where
  type MElem (HashMap k v) = k
  member = HMap.member

-- | @since 0.1
instance HashCompat a => Member (HashSet a) where
  type MElem (HashSet a) = a
  member = HSet.member

-- | @since 0.1
instance Member (IntMap a) where
  type MElem (IntMap a) = Int
  member = IntMap.member

-- | @since 0.1
instance Member IntSet where
  type MElem IntSet = Int
  member = IntSet.member

-- | @since 0.1
instance Ord k => Member (Map k v) where
  type MElem (Map k v) = k
  member = Map.member

-- | @since 0.1
instance Eq a => Member (Seq a) where
  type MElem (Seq a) = a
  member = elem

-- | @since 0.1
instance Ord a => Member (Set a) where
  type MElem (Set a) = a
  member = Set.member

-- | Operator alias for 'member'. U+2216.
--
-- @since 0.1
(∈) :: Member a => MElem a -> a -> Bool
(∈) = member

infix 4 ∈

-- | Negation of '(∈)'.
--
-- @since 0.1
(∉) :: Member a => MElem a -> a -> Bool
(∉) x = not . (∈) x

infix 4 ∉
