module System.Terminal.Emulator.TermSurface.FromTerm where

import Control.Lens
import System.Terminal.Emulator.Term (Term, activeScreen, altScreenActive, cursorPos, scrollBackLines, windowTitle)
import System.Terminal.Emulator.TermSurface (TermSurface (..))

termSurfaceFromTerm :: Term -> TermSurface
termSurfaceFromTerm term =
  TermSurface
    { termSurface_screen = term ^. activeScreen,
      termSurface_cursorPos = term ^. cursorPos,
      termSurface_windowTitle = term ^. windowTitle,
      termSurface_scrollBackVisible = not (term ^. altScreenActive),
      termSurface_scrollBack = term ^. scrollBackLines
    }
