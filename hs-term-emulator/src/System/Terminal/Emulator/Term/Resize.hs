module System.Terminal.Emulator.Term.Resize
  ( resizeTerm,
  )
where

import Control.Category ((>>>))
import Control.Exception (assert)
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import System.Terminal.Emulator.Term (Term, activeScreen, addScrollBackLines, altScreenActive, cursorPos, numCols, numRows, numScrollBackLines, scrollBackLines, scrollBottom, scrollTop, termAlt, termAttrs, termScreen)
import System.Terminal.Emulator.TermLines (TermLine, TermLines)
import qualified System.Terminal.Emulator.TermLines as TL
import System.Terminal.Emulator.TermSurface.TermSurfaceChange (TermSurfaceChange)
import qualified System.Terminal.Emulator.TermSurface.TermSurfaceChange as TermSurfaceChange
import Prelude hiding (lines)

-- | This should be called when the user resizes the terminal window.
--
-- You should also call 'System.Posix.Pty.resizePty', but only afterwards
--
-- The tuple is in the shape @(newWidth, newHeight)@, both must be positive
resizeTerm :: Term -> (Int, Int) -> (Seq TermSurfaceChange, Term)
resizeTerm term (newWidth, newHeight) =
  assert (newWidth > 0) $
    assert (newHeight > 0) $
      ( resizeTermWidth newWidth
          >>>> resizeTermHeight newHeight
          >>>> silent (scrollTop .~ 0)
          >>>> silent (scrollBottom .~ (newHeight - 1))
      )
        term

-- Internal function. Resize the terminal, only changing the width.
resizeTermWidth :: Int -> Term -> (Seq TermSurfaceChange, Term)
resizeTermWidth newWidth term =
  let (surfaceChanges, term') =
        ( silent (numCols .~ newWidth)
            >>>> silent (termScreen %~ fmap adjustLine)
            >>>> silent (termAlt %~ fmap adjustLine)
            >>>> silent (scrollBackLines %~ fmap adjustLine)
            >>>> clampCursor
        )
          term
   in (surfaceChanges <> pure (TermSurfaceChange.Resize newWidth (term ^. numRows)), term')
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

    -- silent (cursorPos . _2 %~ min (newWidth - 1))
    clampCursor :: Term -> (Seq TermSurfaceChange, Term)
    clampCursor t
      | t ^. cursorPos . _2 > (newWidth - 1) = (pure (TermSurfaceChange.MoveCursor (t ^. cursorPos . _1) (newWidth - 1)), (cursorPos . _2 .~ newWidth - 1) t)
      | otherwise = (mempty, t)

-- Internal function. Resize the terminal, only changing the height.
resizeTermHeight :: Int -> Term -> (Seq TermSurfaceChange, Term)
resizeTermHeight newHeight term
  | newHeight >= oldHeight = resizeTermHeight' newHeight term
  | otherwise =
    let term' = truncateTermScreenBottom term (oldHeight - newHeight)
     in resizeTermHeight' newHeight term'
  where
    oldHeight = term ^. numRows

resizeTermHeight' :: Int -> Term -> (Seq TermSurfaceChange, Term)
resizeTermHeight' newHeight term =
  let (surfaceChanges, term') =
        ( silent (numRows .~ newHeight)
            >>>> adjustScreen
            >>>> silent (termAlt %~ adjustAltScreen)
            >>>> silent (cursorPos . _1 %~ min (newHeight - 1))
        )
          term
   in ( surfaceChanges
          <> Seq.fromList
            ( map
                (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen . TL.vIndex row))
                [0 .. (term' ^. numRows) - 1]
            ),
        term'
      )
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

    expandScreen :: Term -> (Seq TermSurfaceChange, Term)
    expandScreen t =
      let term' =
            ( ( termScreen
                  %~ ( \lines ->
                         TL.takeLast numHistoryLines (t ^. scrollBackLines)
                           <> lines
                           <> TL.replicate numNewBlankLines newBlankLine
                     )
              )
                >>> scrollBackLines %~ TL.dropLast numHistoryLines
                >>> moveCursorDown
            )
              t
       in ( Seq.fromList
              [ TermSurfaceChange.ClearScrollBackEnd numHistoryLines,
                TermSurfaceChange.Resize (term ^. numCols) newHeight,
                TermSurfaceChange.MoveCursor (term' ^. cursorPos . _1) (term' ^. cursorPos . _2)
              ],
            term'
          )
      where
        numHistoryLines = min (newHeight - oldHeight) (TL.length (term ^. scrollBackLines))
        numNewBlankLines = (newHeight - oldHeight) - numHistoryLines

        moveCursorDown :: Term -> Term
        moveCursorDown
          | term ^. altScreenActive = id
          | otherwise = cursorPos . _1 %~ (+ numHistoryLines)

    shrinkScreen :: Term -> (Seq TermSurfaceChange, Term)
    shrinkScreen t =
      let newScrollBackLines = TL.takeLast (term ^. numScrollBackLines) (TL.take numShrunkLines (term ^. termScreen))
          term' =
            ( (termScreen %~ TL.takeLast newHeight)
                >>> addScrollBackLines newScrollBackLines
                >>> moveCursorUp
            )
              t
          numToDelete = TL.length (t ^. scrollBackLines) + TL.length newScrollBackLines - term ^. numScrollBackLines
       in ( ( if numToDelete > 0
                then pure (TermSurfaceChange.ClearScrollBackStart numToDelete)
                else mempty
            )
              <> TL.toSeq (fmap TermSurfaceChange.AppendScrollBack newScrollBackLines)
              <> Seq.fromList
                [ TermSurfaceChange.MoveCursor (min (newHeight - 1) (term' ^. cursorPos . _1)) (term' ^. cursorPos . _2),
                  TermSurfaceChange.Resize (term ^. numCols) newHeight
                ],
            term'
          )
      where
        numShrunkLines = oldHeight - newHeight
        moveCursorUp :: Term -> Term
        moveCursorUp
          | term ^. altScreenActive = id
          | otherwise = cursorPos . _1 %~ (\y -> max 0 (y - numShrunkLines))

    adjustScreen :: Term -> (Seq TermSurfaceChange, Term)
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

infixr 1 >>>>

(>>>>) :: (Term -> (Seq TermSurfaceChange, Term)) -> (Term -> (Seq TermSurfaceChange, Term)) -> (Term -> (Seq TermSurfaceChange, Term))
f1 >>>> f2 = \term ->
  let (cs1, term') = f1 term
      (cs2, term'') = f2 term'
   in (cs1 <> cs2, term'')

silent :: (Term -> Term) -> Term -> (Seq TermSurfaceChange, Term)
silent = \f t -> (mempty, f t)
