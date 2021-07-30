{-# LANGUAGE NamedFieldPuns #-}

module System.Terminal.Emulator.Term2 where

import Control.Monad.ST
import Data.STRef
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import System.Terminal.Emulator.Attrs (Cell)

data Term2 = Term2
  { screen :: !(Vector Cell),
    cursorX :: !Int,
    cursorY :: !Int
  }

data STTerm2 s = STTerm2
  { stScreen :: !(STVector s Cell),
    stCursorX :: !(STRef s Int),
    stCursorY :: !(STRef s Int)
  }

toST :: Term2 -> ST s (STTerm2 s)
toST Term2 {screen} = do
  scr <- V.thaw screen
  pure
    STTerm2
      { stScreen = scr,
        stCursorX = error "TODO",
        stCursorY = error "TODO"
      }

advanceCursor :: STTerm2 s -> ST s (Int)
advanceCursor STTerm2 {stCursorX} = do
  modifySTRef' stCursorX (+ 1)
  pure 1

test1 :: ST s Int
test1 = pure 5

test2 :: Int
test2 = runST $ do
  r <- test1
  pure r
