module System.Terminal.Emulator.TermSurface.ApplyTermSurfaceChange where

import Control.Lens
import qualified System.Terminal.Emulator.TermLines as TL
import System.Terminal.Emulator.TermSurface (TermSurface (..))
import System.Terminal.Emulator.TermSurface.TermSurfaceChange (TermSurfaceChange)
import qualified System.Terminal.Emulator.TermSurface.TermSurfaceChange as TermSurfaceChange

applySurfaceChange :: TermSurface -> TermSurfaceChange -> TermSurface
applySurfaceChange termSurface termSurfaceChange =
  case termSurfaceChange of
    TermSurfaceChange.SetWindowTitle title -> termSurface {termSurface_windowTitle = title}
    TermSurfaceChange.MoveCursor row col -> termSurface {termSurface_cursorPos = (row, col)}
    TermSurfaceChange.AppendScrollBack line ->
      let numScrollBackLines = termSurface_numScrollBackLines termSurface
          oldScrollBack = termSurface_scrollBack termSurface
          newScrollBack = TL.takeLast numScrollBackLines (oldScrollBack <> TL.singleton line)
       in termSurface {termSurface_scrollBack = newScrollBack}
    TermSurfaceChange.ClearScrollBack -> termSurface {termSurface_scrollBack = TL.empty}
    TermSurfaceChange.UpdateLine row line ->
      let oldScreen = termSurface_screen termSurface
          newScreen = (TL.vIndex row .~ line) oldScreen
       in termSurface {termSurface_screen = newScreen}
    TermSurfaceChange.Resize _width _height -> error "RESIZE"
