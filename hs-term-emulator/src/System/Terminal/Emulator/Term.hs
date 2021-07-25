{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module System.Terminal.Emulator.Term
  ( -- * Types
    Term,
    mkTerm,

    -- * Direct 'Term' Lenses
    termAttrs,
    cursorPos,
    cursorState,
    modeWrap,
    insertMode,
    altScreenActive,
    numCols,
    numRows,
    keyboardState,
    scrollTop,
    scrollBottom,
    scrollBackLines,
    numScrollBackLines,
    termScreen,
    termAlt,
    windowTitle,

    -- * Direct 'CursorState' Lenses
    wrapNext,
    origin,

    -- * Helper 'Term' Lenses
    cursorLine,
    activeScreen,

    -- * Misc
    addScrollBackLines,
    vuIndex,
    termGetKeyboardState,
  )
where

import Control.Category ((>>>))
import Control.Exception (assert)
import Control.Lens
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import System.Terminal.Emulator.Attrs (Attrs, blankAttrs)
import System.Terminal.Emulator.KeyboardInput (KeyboardState, initialKeyboardState)
import System.Terminal.Emulator.TermLines (TermLine, TermLines)
import qualified System.Terminal.Emulator.TermLines as TL
import Prelude hiding (lines)

data CursorState = CursorState
  { cursorState_WrapNext :: !Bool,
    cursorState_Origin :: !Bool
  }
  deriving (Show, Eq, Ord)

data CursorPos = CursorPos !Int !Int
  deriving (Show, Eq, Ord)

data Term = Term
  { term_Attrs :: !Attrs,
    -- | (line, column)
    term_CursorPos :: !CursorPos,
    term_CursorState :: !CursorState,
    -- | Set using Wraparound Mode (DECAWM)
    term_ModeWrap :: !Bool,
    -- | Set using Insert/Replace Mode (IRM)
    term_InsertMode :: !Bool,
    term_AltScreenActive :: !Bool,
    term_NumCols :: !Int,
    term_NumRows :: !Int,
    term_KeyboardState :: !KeyboardState,
    -- | Row index of the top of the scroll region
    term_ScrollTop :: !Int,
    -- | Row index of the bottom of the scroll region
    term_ScrollBottom :: !Int,
    -- | Scroll back lines of the Main screen
    term_ScrollBackLines :: !TermLines,
    -- | Maximum scroll back lines to be saved
    term_NumScrollBackLines :: !Int,
    -- | Main screen. This is always the size of the terminal.
    term_Screen :: !TermLines,
    -- | Alternate screen. This is always the size of the terminal.
    --
    -- The Alternate screen does not have any scroll back lines
    --
    -- See also:
    -- <https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-The-Alternate-Screen-Buffer>
    term_Alt :: !TermLines,
    term_WindowTitle :: !Text
  }
  deriving (Show, Eq, Ord)

-- | Create a new blank Terminal with the given size @(width, height)@
mkTerm :: (Int, Int) -> Term
mkTerm (width, height) =
  Term
    { term_Attrs = blankAttrs,
      term_CursorPos = CursorPos 0 0,
      term_CursorState =
        CursorState
          { cursorState_WrapNext = False,
            cursorState_Origin = False
          },
      term_ModeWrap = True,
      term_InsertMode = False,
      term_AltScreenActive = False,
      term_NumCols = width,
      term_NumRows = height,
      term_KeyboardState = initialKeyboardState,
      term_ScrollTop = 0,
      term_ScrollBottom = height - 1,
      term_ScrollBackLines = TL.empty,
      term_NumScrollBackLines = 1000,
      term_Screen = TL.replicate height (VU.replicate width ((' ', 0))),
      term_Alt = TL.replicate height (VU.replicate width ((' ', 0))),
      term_WindowTitle = "hs-term"
    }

-----------------------------------------------------------------------
-- Direct 'Term' Lenses
-----------------------------------------------------------------------

termAttrs :: Lens' Term Attrs
termAttrs = lens term_Attrs (\term newVal -> term {term_Attrs = newVal})

-- | Cursor line is always in the range [0..numRows-1]
--
-- Cursor col is always in the range [0..numCols-1]
cursorPos :: Lens' Term (Int, Int)
cursorPos = lens getter setter
  where
    getter :: Term -> (Int, Int)
    getter term = let CursorPos row col = term_CursorPos term in (row, col)
    setter :: Term -> (Int, Int) -> Term
    setter term (newRow, newCol) =
      assert (newCol >= minX) $
        assert (newCol <= maxX) $
          assert (newRow >= minY) $
            assert (newRow <= maxY) $
              term {term_CursorPos = CursorPos newRow newCol}
      where
        minX = 0
        maxX = (term ^. numCols) - 1
        minY = 0
        maxY = (term ^. numRows) - 1

cursorState :: Lens' Term CursorState
cursorState = lens term_CursorState (\term newVal -> term {term_CursorState = newVal})

-- | Wraparound Mode (DECAWM)
modeWrap :: Lens' Term Bool
modeWrap = lens term_ModeWrap (\term newVal -> term {term_ModeWrap = newVal})

-- | Insert/Replace Mode (IRM)
insertMode :: Lens' Term Bool
insertMode = lens term_InsertMode (\term newVal -> term {term_InsertMode = newVal})

altScreenActive :: Lens' Term Bool
altScreenActive = lens term_AltScreenActive (\term newVal -> term {term_AltScreenActive = newVal})

numCols :: Lens' Term Int
numCols = lens term_NumCols (\term newVal -> term {term_NumCols = newVal})

numRows :: Lens' Term Int
numRows = lens term_NumRows (\term newVal -> term {term_NumRows = newVal})

keyboardState :: Lens' Term KeyboardState
keyboardState = lens term_KeyboardState (\term newVal -> term {term_KeyboardState = newVal})

scrollTop :: Lens' Term Int
scrollTop = lens term_ScrollTop (\term newVal -> term {term_ScrollTop = newVal})

scrollBottom :: Lens' Term Int
scrollBottom = lens term_ScrollBottom (\term newVal -> term {term_ScrollBottom = newVal})

scrollBackLines :: Lens' Term TermLines
scrollBackLines = lens term_ScrollBackLines (\term newVal -> term {term_ScrollBackLines = newVal})

numScrollBackLines :: Lens' Term Int
numScrollBackLines = lens term_NumScrollBackLines (\term newVal -> term {term_NumScrollBackLines = newVal})

termScreen :: Lens' Term TermLines
termScreen = lens term_Screen (\term newVal -> term {term_Screen = newVal})

termAlt :: Lens' Term TermLines
termAlt = lens term_Alt (\term newVal -> term {term_Alt = newVal})

windowTitle :: Lens' Term Text
windowTitle = lens term_WindowTitle (\term newWindowTitle -> term {term_WindowTitle = newWindowTitle})

-----------------------------------------------------------------------
-- Direct 'Term' Lenses
-----------------------------------------------------------------------

wrapNext :: Lens' CursorState Bool
wrapNext = lens cursorState_WrapNext (\cs newWrapNext -> cs {cursorState_WrapNext = newWrapNext})

origin :: Lens' CursorState Bool
origin = lens cursorState_Origin (\cs newOrigin -> cs {cursorState_Origin = newOrigin})

-----------------------------------------------------------------------
-- Helper 'Term' Lenses
-----------------------------------------------------------------------

-- | A lens to the line where the cursor currently is
cursorLine :: Lens' Term TermLine
cursorLine = lens getter setter
  where
    getter :: Term -> TermLine
    getter term = term ^. activeScreen . TL.vIndex (term ^. cursorPos . _1)
    setter :: Term -> TermLine -> Term
    setter term newTermLine = ((activeScreen . TL.vIndex (term ^. cursorPos . _1)) .~ newTermLine) term

-- | Either the main screen or the alternate screen (depending on which is
-- active)
activeScreen :: Lens' Term TermLines
activeScreen = lens getter setter
  where
    getter :: Term -> TermLines
    getter term = (if term_AltScreenActive term then term_Alt else term_Screen) term
    setter :: Term -> TermLines -> Term
    setter term newLines = (if term_AltScreenActive term then term {term_Alt = newLines} else term {term_Screen = newLines})

-----------------------------------------------------------------------

termGetKeyboardState :: Term -> KeyboardState
termGetKeyboardState = term_KeyboardState

vuIndex :: VU.Unbox a => Int -> Lens' (VU.Vector a) a
vuIndex i = lens getter setter
  where
    getter :: VU.Unbox a => VU.Vector a -> a
    getter v = assert (i >= 0 && i <= VU.length v - 1) $ v VU.! i
    setter :: VU.Unbox a => VU.Vector a -> a -> VU.Vector a
    setter v val = assert (i >= 0 && i <= VU.length v - 1) $ v VU.// [(i, val)]

addScrollBackLines :: TermLines -> Term -> Term
addScrollBackLines newLines term =
  (scrollBackLines %~ ((<> newLines) >>> TL.takeLast (term ^. numScrollBackLines))) term
