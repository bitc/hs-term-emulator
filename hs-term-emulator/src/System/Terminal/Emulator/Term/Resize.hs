module System.Terminal.Emulator.Term.Resize
  ( resizeTerm,
  )
where

import Control.Category ((>>>))
import Control.Exception (assert)
import Control.Lens
import qualified Data.Vector.Unboxed as VU
import System.Terminal.Emulator.Term (Term, addScrollBackLines, altScreenActive, cursorPos, numCols, numRows, scrollBackLines, scrollBottom, scrollTop, termAlt, termAttrs, termScreen)
import System.Terminal.Emulator.TermLines (TermLine, TermLines)
import qualified System.Terminal.Emulator.TermLines as TL
import Prelude hiding (lines)

-- | This should be called when the user resizes the terminal window.
--
-- You should also call 'System.Posix.Pty.resizePty', but only afterwards
--
-- The tuple is in the shape @(newWidth, newHeight)@, both must be positive
resizeTerm :: Term -> (Int, Int) -> Term
resizeTerm term (newWidth, newHeight) =
  assert (newWidth > 0) $
    assert (newHeight > 0) $
      ( resizeTermWidth newWidth
          >>> resizeTermHeight newHeight
          >>> scrollTop .~ 0
          >>> scrollBottom .~ (newHeight - 1)
      )
        term

-- Internal function. Resize the terminal, only changing the width.
resizeTermWidth :: Int -> Term -> Term
resizeTermWidth newWidth term =
  ( numCols .~ newWidth
      >>> termScreen %~ fmap adjustLine
      >>> termAlt %~ fmap adjustLine
      >>> scrollBackLines %~ fmap adjustLine
      >>> cursorPos . _2 %~ min (newWidth - 1)
  )
    term
  where
    oldWidth = term ^. numCols

    expandLine :: TermLine -> TermLine
    expandLine = (<> (VU.replicate (newWidth - oldWidth) ((' ', 0))))

    shrinkLine :: TermLine -> TermLine
    shrinkLine = VU.take newWidth

    adjustLine :: TermLine -> TermLine
    adjustLine
      | newWidth > oldWidth = expandLine
      | otherwise = shrinkLine

-- Internal function. Resize the terminal, only changing the height.
resizeTermHeight :: Int -> Term -> Term
resizeTermHeight newHeight term
  | newHeight >= oldHeight = resizeTermHeight' newHeight term
  | otherwise =
    let term' = truncateTermScreenBottom term (oldHeight - newHeight)
     in resizeTermHeight' newHeight term'
  where
    oldHeight = term ^. numRows

resizeTermHeight' :: Int -> Term -> Term
resizeTermHeight' newHeight term =
  ( numRows .~ newHeight
      >>> adjustScreen
      >>> termAlt %~ adjustAltScreen
      >>> cursorPos . _1 %~ min (newHeight - 1)
  )
    term
  where
    oldHeight = term ^. numRows

    newBlankLine = VU.replicate (term ^. numCols) (' ', term ^. termAttrs)

    expandAltScreen :: TermLines -> TermLines
    expandAltScreen = (<> (TL.replicate (newHeight - oldHeight) newBlankLine))

    shrinkAltScreen :: TermLines -> TermLines
    shrinkAltScreen = TL.take newHeight

    adjustAltScreen :: TermLines -> TermLines
    adjustAltScreen
      | newHeight > oldHeight = expandAltScreen
      | otherwise = shrinkAltScreen

    expandScreen :: Term -> Term
    expandScreen =
      ( termScreen
          %~ ( \lines ->
                 TL.takeLast numHistoryLines (term ^. scrollBackLines)
                   <> lines
                   <> TL.replicate numNewBlankLines newBlankLine
             )
      )
        >>> scrollBackLines %~ TL.dropLast numHistoryLines
        >>> moveCursorDown
      where
        numHistoryLines = min (newHeight - oldHeight) (TL.length (term ^. scrollBackLines))
        numNewBlankLines = (newHeight - oldHeight) - numHistoryLines

        moveCursorDown :: Term -> Term
        moveCursorDown
          | term ^. altScreenActive = id
          | otherwise = cursorPos . _1 %~ (+ numHistoryLines)

    shrinkScreen :: Term -> Term
    shrinkScreen =
      (termScreen %~ TL.takeLast newHeight)
        >>> addScrollBackLines (TL.take numShrunkLines (term ^. termScreen))
        >>> moveCursorUp
      where
        numShrunkLines = oldHeight - newHeight
        moveCursorUp :: Term -> Term
        moveCursorUp
          | term ^. altScreenActive = id
          | otherwise = cursorPos . _1 %~ (\y -> max 0 (y - numShrunkLines))

    adjustScreen
      | newHeight > oldHeight = expandScreen
      | otherwise = shrinkScreen

-- | Chop off up to @n@ lines from the bottom of the main screen (only if they
-- are blank and not occupied by the cursor).
--
-- Also modifies the vertical size of the screen according to the number of
-- lines removed (numLines)
truncateTermScreenBottom :: Term -> Int -> Term
truncateTermScreenBottom term numLines
  | numLines == 0 = term
  | term ^. altScreenActive = term
  | term ^. cursorPos . _1 == term ^. numRows - 1 = term
  | not (lineIsBlank lastLine) = term
  | otherwise =
    let term' =
          ( (termScreen %~ (TL.dropLast 1))
              >>> (numRows %~ (subtract 1))
          )
            term
     in truncateTermScreenBottom term' (numLines - 1)
  where
    lastLine = TL.last (term ^. termScreen)
    lineIsBlank :: TermLine -> Bool
    lineIsBlank = VU.all (== (' ', 0))
