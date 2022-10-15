-- | Provides the 'Sequenced' type class.
--
-- @since 0.1
module Containers.Class.Sequenced
  ( Sequenced (..),
    (⋗),
    (⋖),
  )
where

import Data.Sequence (Seq, (<|), (|>))

-- | Types that support an "ordered" insert.
--
-- @since 0.1
class Sequenced a where
  -- | Element type.
  type SElem a

  -- | Appends an element to the sequence.
  --
  -- @since 0.1
  append :: a -> SElem a -> a

  -- | Prepends an element to the sequence.
  --
  -- @since 0.1
  prepend :: SElem a -> a -> a

-- | Operator alias for 'append'. U+22D7.
--
-- @since 0.1
(⋗) :: Sequenced a => a -> SElem a -> a
(⋗) = append

-- | Operator alias for 'prepend'. U+22D6.
--
-- @since 0.1
(⋖) :: Sequenced a => SElem a -> a -> a
(⋖) = prepend

-- | @since 0.1
instance Sequenced [a] where
  type SElem [a] = a
  prepend = (:)
  append xs = (++) xs . pure

-- | @since 0.1
instance Sequenced (Seq a) where
  type SElem (Seq a) = a
  append = (|>)
  prepend = (<|)
