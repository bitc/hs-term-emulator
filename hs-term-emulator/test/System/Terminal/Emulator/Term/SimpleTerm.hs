module System.Terminal.Emulator.Term.SimpleTerm
  ( SimpleTerm (..),
    termToSimpleTerm,
  )
where

import Control.Lens
import Data.Foldable (toList)
import qualified Data.Vector.Unboxed as VU
import System.Terminal.Emulator.Term (Term, cursorPos, scrollBackLines, termScreen)
import System.Terminal.Emulator.TermLines (TermLine)

-- | A simplified terminal that doesn't have an alt screen or colors
data SimpleTerm = SimpleTerm
  { st_ScrollBackLines :: [[Char]],
    st_Screen :: [[Char]],
    st_CursorPos :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

termToSimpleTerm :: Term -> SimpleTerm
termToSimpleTerm term =
  SimpleTerm
    { st_ScrollBackLines = map termLineToStLine $ toList $ term ^. scrollBackLines,
      st_Screen = map termLineToStLine $ toList $ term ^. termScreen,
      st_CursorPos = term ^. cursorPos
    }

termLineToStLine :: TermLine -> [Char]
termLineToStLine = map fst . VU.toList
