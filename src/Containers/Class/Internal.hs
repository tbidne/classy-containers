{-# LANGUAGE CPP #-}

-- | Internal module. Details can change at any time with no warning.
--
-- @since 0.1
module Containers.Class.Internal
  ( HashCompat,
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)

#if MIN_VERSION_hashable(1, 4, 0)
-- | Hashable has 'Eq' as a superclass for hashable 1.4.0.0+. This synonym
-- exists so that we do not run afoul of -Wredundant-constraints.
--
-- @since 0.1
type HashCompat :: Type -> Constraint
type HashCompat a = Hashable a
#else
-- | Hashable has 'Eq' as a superclass for hashable 1.4.0.0+. This synonym
-- exists so that we do not run afoul of -Wredundant-constraints.
--
-- @since 0.1
type HashCompat :: Type -> Constraint
type HashCompat a = (Eq a, Hashable a)
#endif
-- type HashCompat =
