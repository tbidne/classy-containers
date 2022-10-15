-- | Provides the 'Empty' type class.
--
-- @since 0.1
module Containers.Class.Empty
  ( Empty (..),
    (∅),
  )
where

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
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

-- | Types that have a notion of empty.
--
-- @since 0.1
class Empty a where
  -- | @since 0.1
  empty :: a

-- | @since 0.1
instance Empty [a] where
  empty = []

-- | @since 0.1
instance Empty (HashMap k v) where
  empty = HMap.empty

-- | @since 0.1
instance Empty (HashSet a) where
  empty = HSet.empty

-- | @since 0.1
instance Empty (IntMap a) where
  empty = IntMap.empty

-- | @since 0.1
instance Empty IntSet where
  empty = IntSet.empty

-- | @since 0.1
instance Empty (Map k v) where
  empty = Map.empty

-- | @since 0.1
instance Empty (Seq a) where
  empty = Seq.empty

-- | @since 0.1
instance Empty (Set k) where
  empty = Set.empty

-- | Operator alias for 'empty'. U+2205.
--
-- @since 0.1
(∅) :: Empty a => a
(∅) = empty
