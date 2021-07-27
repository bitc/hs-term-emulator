{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module System.Terminal.Emulator.TermLines
  ( TermLine,
    TermLines,
    empty,
    length,
    replicate,
    vIndex,
    head,
    last,
    take,
    takeLast,
    drop,
    dropLast,
    traverseWithIndex,
  )
where

import Control.Exception (assert)
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import System.Terminal.Emulator.Attrs (Cell)
import Prelude hiding (drop, head, last, length, replicate, take)

type TermLine = VU.Vector Cell

type TermLines = StrictSeq TermLine

newtype StrictSeq a = StrictSeq (Seq a)
  deriving (Show, Eq, Ord, Functor, Semigroup)

-- | The empty sequence.
empty :: StrictSeq a
empty = StrictSeq Seq.empty
{-# INLINE empty #-}

-- | The number of elements in the sequence.
length :: StrictSeq a -> Int
length (StrictSeq v) = Seq.length v
{-# INLINE length #-}

-- | @replicate n x@ is a sequence consisting of n copies of x.
replicate :: Int -> a -> StrictSeq a
replicate n x = x `seq` (StrictSeq (Seq.replicate n x))
{-# INLINE replicate #-}

-- | A lens to the specified index of the sequence. Must be in range.
vIndex :: Int -> Lens' (StrictSeq a) a
vIndex i =
  lens getter setter
  where
    getter :: StrictSeq a -> a
    getter (StrictSeq v) = assert (i >= 0 && i < Seq.length v) $ (`Seq.index` i) v
    setter :: StrictSeq a -> a -> StrictSeq a
    setter (StrictSeq v) val = assert (i >= 0 && i < Seq.length v) $ val `seq` (StrictSeq (Seq.update i val v))
    {-# INLINE getter #-}
    {-# INLINE setter #-}
{-# INLINE vIndex #-}

-- | First element. Must be nonempty
head :: StrictSeq a -> a
head (StrictSeq v) = let x Seq.:< _ = Seq.viewl v in x
{-# INLINE head #-}

-- | Last element. Must be nonempty
last :: StrictSeq a -> a
last (StrictSeq v) = let _ Seq.:> x = Seq.viewr v in x
{-# INLINE last #-}

-- | The first @i@ elements of a sequence. If @i@ is negative, @take i s@
-- yields the empty sequence. If the sequence contains fewer than @i@
-- elements, the whole sequence is returned.
take :: Int -> StrictSeq a -> StrictSeq a
take i (StrictSeq v) = StrictSeq (Seq.take i v)
{-# INLINE take #-}

-- | The last @i@ elements of a sequence. If @i@ is negative, @takeLast i s@
-- yields the empty sequence. If the sequence contains fewer than @i@
-- elements, the whole sequence is returned.
takeLast :: Int -> StrictSeq a -> StrictSeq a
takeLast i (StrictSeq v) = StrictSeq (Seq.drop (Seq.length v - i) v)
{-# INLINE takeLast #-}

-- | Elements of a sequence after the first @i@. If @i@ is negative, @drop i
-- s@ yields the whole sequence. If the sequence contains fewer than @i@
-- elements, the empty sequence is returned.
drop :: Int -> StrictSeq a -> StrictSeq a
drop i (StrictSeq v) = StrictSeq (Seq.drop i v)
{-# INLINE drop #-}

-- | Elements of a sequence after the first @i@ last elements. If @i@ is
-- negative, @dropLast i s@ yields the whole sequence. If the sequence
-- contains fewer than @i@ elements, the empty sequence is returned.
dropLast :: Int -> StrictSeq a -> StrictSeq a
dropLast i (StrictSeq v) = StrictSeq (Seq.take (Seq.length v - i) v)
{-# INLINE dropLast #-}

-- | @traverseWithIndex@ is a version of @traverse@ that also offers access to
-- the index of each element.
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> StrictSeq a -> f (StrictSeq b)
traverseWithIndex f (StrictSeq v) = StrictSeq <$> (Seq.traverseWithIndex f v)
{-# INLINE traverseWithIndex #-}
