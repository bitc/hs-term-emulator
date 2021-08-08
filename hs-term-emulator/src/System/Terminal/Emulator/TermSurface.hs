module System.Terminal.Emulator.TermSurface where

import Data.Text (Text)
import System.Terminal.Emulator.TermLines (TermLines)

data TermSurface = TermSurface
  { termSurface_screen :: !TermLines,
    termSurface_cursorPos :: !(Int, Int),
    termSurface_windowTitle :: !Text,
    termSurface_scrollBack :: !TermLines,
    termSurface_scrollBackVisible :: !Bool
  }
  deriving (Show, Eq, Ord)
