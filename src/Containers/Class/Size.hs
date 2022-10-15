-- | Provides the 'Size' type class.
--
-- @since 0.1
module Containers.Class.Size
  ( Size (..),
    (♯),
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
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

-- | Types that have a notion of size. The 'HasCallStack' constraint
-- is for common @'Int' -> 'Natural'@ conversions.
--
-- @since 0.1
class Size a where
  -- | @since 0.1
  size :: HasCallStack => a -> Natural

-- | Operator alias for 'size'. U+222A.
--
-- @since 0.1
(♯) :: (HasCallStack, Size a) => a -> Natural
(♯) = size

-- | @since 0.1
instance Size [a] where
  size = fromIntegral . length

-- | @since 0.1
instance Size (HashMap k v) where
  size = fromIntegral . HMap.size

-- | @since 0.1
instance Size (HashSet a) where
  size = fromIntegral . HSet.size

-- | @since 0.1
instance Size (IntMap a) where
  size = fromIntegral . IntMap.size

-- | @since 0.1
instance Size IntSet where
  size = fromIntegral . IntSet.size

-- | @since 0.1
instance Size (Map k v) where
  size = fromIntegral . Map.size

-- | @since 0.1
instance Size (Seq a) where
  size = fromIntegral . length

-- | @since 0.1
instance Size (Set k) where
  size = fromIntegral . Set.size
