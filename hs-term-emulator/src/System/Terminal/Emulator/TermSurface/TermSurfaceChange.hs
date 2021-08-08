module System.Terminal.Emulator.TermSurface.TermSurfaceChange where

import Data.Text (Text)
import System.Terminal.Emulator.TermLines (TermLine)

data TermSurfaceChange
  = -- | Set the title of the terminal
    SetWindowTitle !Text
  | -- | Move the cursor to @row@, @column@
    MoveCursor !Int !Int
  | -- | Set the contents of row @n@
    UpdateLine !Int !TermLine
  | -- | Add a line to the end of the ScrollBack
    AppendScrollBack !TermLine
  | -- | Show or hide the ScrollBack
    SetScrollBackVisible !Bool
  | -- | Delete all the lines of the ScrollBack
    ClearScrollBack
  | -- | Delete @n@ lines from the beginning of the ScrollBack
    ClearScrollBackStart !Int
  | -- | Delete @n@ lines from the end of the ScrollBack
    ClearScrollBackEnd !Int
  | -- | Set the size of the terminal to @width@, @height@
    Resize !Int !Int
  deriving (Show, Eq, Ord)
