module System.Terminal.Emulator.TermSurface.TermSurfaceChange where

import Data.Text (Text)
import System.Terminal.Emulator.TermLines (TermLine)

data TermSurfaceChange
  = SetWindowTitle !Text
  | MoveCursor !Int !Int
  | UpdateLine !Int !TermLine
  | AppendScrollBack !TermLine
  | ClearScrollBack
  | Resize !Int !Int
  deriving (Show, Eq, Ord)
