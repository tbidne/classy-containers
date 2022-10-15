-- | Provides the 'Insert' type class.
--
-- @since 0.1
module Containers.Class.Insert
  ( Insert (..),
    (⟇),
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Constraint)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- TODO: infix, lazy maps/sets

-- | Types that support an "unordered" insert.
--
-- @since 0.1
class Insert a where
  -- | Constraint on the element.
  type InsertC a :: Constraint

  -- | Element type.
  type IElem a

  -- Inserts an element.
  --
  -- @since 0.1
  insert :: InsertC a => IElem a -> a -> a

-- | Operator alias for 'insert'. U+27C7.
--
-- @since 0.1
(⟇) :: (Insert a, InsertC a) => IElem a -> a -> a
(⟇) = insert

-- TODO: set infix

-- containers: IntMap, IntSet, Map, Seq, Set,

instance Insert (HashMap k v) where
  type InsertC (HashMap k v) = Hashable k
  type IElem (HashMap k v) = (k, v)
  insert (k, v) = HMap.insert k v

instance Insert (HashSet a) where
  type InsertC (HashSet a) = Hashable a
  type IElem (HashSet a) = a
  insert = HSet.insert

instance Insert (IntMap v) where
  type InsertC (IntMap v) = ()
  type IElem (IntMap v) = (Int, v)
  insert (k, v) = IntMap.insert k v

instance Insert IntSet where
  type InsertC IntSet = ()
  type IElem IntSet = Int
  insert = IntSet.insert

instance Insert (Map k v) where
  type InsertC (Map k v) = Ord k
  type IElem (Map k v) = (k, v)
  insert (k, v) = Map.insert k v

instance Insert (Set a) where
  type InsertC (Set a) = Ord a
  type IElem (Set a) = a
  insert = Set.insert
