module System.Terminal.Emulator.TermSurface.ApplyTermSurfaceChange
  ( applySurfaceChange,
  )
where

import Control.Lens
import qualified Data.Vector.Unboxed as VU
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
      let newScrollBack = termSurface_scrollBack termSurface <> TL.singleton line
       in termSurface {termSurface_scrollBack = newScrollBack}
    TermSurfaceChange.SetScrollBackVisible v -> termSurface {termSurface_scrollBackVisible = v}
    TermSurfaceChange.ClearScrollBack -> termSurface {termSurface_scrollBack = TL.empty}
    TermSurfaceChange.ClearScrollBackStart n -> termSurface {termSurface_scrollBack = TL.drop n (termSurface_scrollBack termSurface)}
    TermSurfaceChange.ClearScrollBackEnd n -> termSurface {termSurface_scrollBack = TL.dropLast n (termSurface_scrollBack termSurface)}
    TermSurfaceChange.UpdateLine row line ->
      let oldScreen = termSurface_screen termSurface
          newScreen = (TL.vIndex row .~ line) oldScreen
       in termSurface {termSurface_screen = newScreen}
    TermSurfaceChange.Resize width height ->
      ( (resizeTermHeight height) . (resizeTermWidth width)
      )
        termSurface

resizeTermWidth :: Int -> TermSurface -> TermSurface
resizeTermWidth width termSurface =
  termSurface
    { termSurface_screen = fmap (resizeLine width) (termSurface_screen termSurface),
      termSurface_scrollBack = fmap (resizeLine width) (termSurface_scrollBack termSurface)
    }

resizeLine :: Int -> TL.TermLine -> TL.TermLine
resizeLine width line = VU.take width line <> VU.replicate (width - VU.length line) (' ', 0)

resizeTermHeight :: Int -> TermSurface -> TermSurface
resizeTermHeight height termSurface =
  termSurface
    { termSurface_screen = TL.take height oldLines <> TL.replicate numNewLines newBlankLine
    }
  where
    width = VU.length $ TL.head oldLines
    oldLines = termSurface_screen termSurface
    numNewLines = max 0 (height - TL.length oldLines)
    newBlankLine = VU.replicate width (' ', 0)
