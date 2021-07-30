module System.Terminal.Emulator.TermSurface.FromTerm where

import Control.Lens
import System.Terminal.Emulator.Term (Term, activeScreen, altScreenActive, cursorPos, numScrollBackLines, scrollBackLines, windowTitle)
import qualified System.Terminal.Emulator.TermLines as TL
import System.Terminal.Emulator.TermSurface (TermSurface (..))

termSurfaceFromTerm :: Term -> TermSurface
termSurfaceFromTerm term =
  TermSurface
    { termSurface_screen = term ^. activeScreen,
      termSurface_cursorPos = term ^. cursorPos,
      termSurface_windowTitle = term ^. windowTitle,
      -- termSurface_scrollBack = term ^. scrollBackLines,
      termSurface_scrollBack = if term ^. altScreenActive then TL.empty else term ^. scrollBackLines,
      termSurface_numScrollBackLines = term ^. numScrollBackLines
    }
