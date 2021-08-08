{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.Term.Process
  ( Term,
    TermLine,
    processTermAtoms,
  )
where

import Control.Category ((>>>))
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Foldable (foldl')
import Data.List (iterate')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified System.Console.ANSI.Types as SGR
import System.Terminal.Emulator.Attrs (Attrs, attrsBg, attrsFg, attrsIntensity, attrsUnderline, blankAttrs)
import System.Terminal.Emulator.DECPrivateMode (DECPrivateMode)
import qualified System.Terminal.Emulator.DECPrivateMode as DECPrivateMode
import System.Terminal.Emulator.KeyboardInput (KeyboardState (keyboardState_CRLF, keyboardState_DECCKM, keyboardState_Locked))
import System.Terminal.Emulator.Parsing.Types (ControlSequenceIntroducer (..), DeviceStatusReport (..), EraseInDisplayParam (..), EraseInLineParam (..), EscapeSequence (..), Mode (..), OperatingSystemCommand (..), SendDeviceAttributesSecondary (RequestTerminalIdentificationCode), SingleCharacterFunction (..), TermAtom (..), WindowManipulation (..))
import System.Terminal.Emulator.Term (Term, activeScreen, addScrollBackLines, altScreenActive, cursorLine, cursorPos, cursorState, insertMode, keyboardState, mkTerm, modeWrap, numCols, numRows, origin, scrollBackLines, scrollBottom, scrollTop, termAttrs, termScreen, vuIndex, windowTitle, wrapNext)
import System.Terminal.Emulator.TermLines (TermLine)
import qualified System.Terminal.Emulator.TermLines as TL
import System.Terminal.Emulator.TermSurface.TermSurfaceChange (TermSurfaceChange)
import qualified System.Terminal.Emulator.TermSurface.TermSurfaceChange as TermSurfaceChange
import Prelude hiding (lines)

processTermAtoms :: [TermAtom] -> Term -> (ByteString, Seq TermSurfaceChange, Term)
processTermAtoms termAtoms term =
  foldl'
    ( \(!w1, !c1, !t) termAtom ->
        let (!w2, !c2, !t') = processTermAtom termAtom t
         in (w1 <> w2, c1 <> c2, t')
    )
    (B.empty, Seq.empty, term)
    termAtoms

processTermAtom :: TermAtom -> Term -> (ByteString, Seq TermSurfaceChange, Term)
processTermAtom (TermAtom_VisibleChar char) = nw $ processVisibleChar char
processTermAtom (TermAtom_SingleCharacterFunction Control_Bell) = ignore -- TODO
processTermAtom (TermAtom_SingleCharacterFunction Control_Backspace) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) - 1) term
processTermAtom (TermAtom_SingleCharacterFunction Control_Tab) = nw $ putTabs 1
processTermAtom (TermAtom_SingleCharacterFunction Control_LineFeed) = nw $ processLF
processTermAtom (TermAtom_SingleCharacterFunction Control_VerticalTab) = nw $ processLF
processTermAtom (TermAtom_SingleCharacterFunction Control_FormFeed) = nw $ processLF
processTermAtom (TermAtom_SingleCharacterFunction Control_CarriageReturn) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, 0) term
processTermAtom (TermAtom_SingleCharacterFunction Control_ReturnTerminalStatus) = ignore
processTermAtom (TermAtom_SingleCharacterFunction Control_SwitchToStandardCharacterSet) = ignore
processTermAtom (TermAtom_SingleCharacterFunction Control_SwitchToAlternateCharacterSet) = ignore
processTermAtom (TermAtom_EscapeSequence escapeSequence) = processEscapeSequence escapeSequence
processTermAtom (TermAtom_SingleCharacterFunctionUnknown x) = error $ "Unknown Character Function: " <> show x
processTermAtom (TermAtom_EscapeSequenceUnknown x)
  | isExpectedInvalidEscSequence x = ignore
  | otherwise = error $ "Unknown ESC seq: " <> show x

-- | No-write operation
nw :: (Term -> (Seq TermSurfaceChange, Term)) -> Term -> (ByteString, Seq TermSurfaceChange, Term)
nw f term =
  let (cs, term') = f term
   in (B.empty, cs, term')

ignore :: Term -> (ByteString, Seq TermSurfaceChange, Term)
ignore t = (B.empty, Seq.empty, t)

noSurfaceChanges :: (Term -> Term) -> Term -> (Seq TermSurfaceChange, Term)
noSurfaceChanges f = \term -> (Seq.empty, f term)

-- | I have observed some invalid ESC sequences in the wild, that I am
-- deciding to ignore for now
isExpectedInvalidEscSequence :: Text -> Bool
isExpectedInvalidEscSequence str
  | ("\ESC[" `T.isPrefixOf` str) && T.any (== '\r') str = True
  | str == "\ESC\r" = True
  | otherwise = False

processEscapeSequence :: EscapeSequence -> Term -> (ByteString, Seq TermSurfaceChange, Term)
processEscapeSequence Esc_ReverseIndex = nw reverseIndex
processEscapeSequence Esc_RIS = ignore -- TODO
processEscapeSequence Esc_DECPAM = ignore -- TODO
processEscapeSequence Esc_DECPNM = ignore -- TODO
processEscapeSequence (ESC_SetG0CharacterSet _) = ignore
processEscapeSequence (Esc_CSI (CSI_CursorUp n)) = nw $ \term -> cursorMoveTo ((term ^. cursorPos . _1) - n, term ^. cursorPos . _2) term
processEscapeSequence (Esc_CSI (CSI_CursorDown n)) = nw $ \term -> cursorMoveTo ((term ^. cursorPos . _1) + n, term ^. cursorPos . _2) term
processEscapeSequence (Esc_CSI (CSI_LinePositionRelative n)) = nw $ \term -> cursorMoveTo ((term ^. cursorPos . _1) + n, term ^. cursorPos . _2) term
processEscapeSequence (Esc_CSI (CSI_CharacterPositionRelative n)) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) + n) term
processEscapeSequence (Esc_CSI (CSI_CursorForward n)) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) + n) term
processEscapeSequence (Esc_CSI (CSI_CursorBack n)) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) - n) term
processEscapeSequence (Esc_CSI (CSI_EraseInLine param)) = nw $ eraseInLine param
processEscapeSequence (Esc_CSI (CSI_EraseCharacters n)) = nw $ eraseCharacters n
processEscapeSequence (Esc_CSI (CSI_InsertBlankCharacters n)) = nw $ insertBlankChars n
processEscapeSequence (Esc_CSI (CSI_InsertBlankLines n)) = nw $ insertBlankLines n
processEscapeSequence (Esc_CSI (CSI_DeleteChars n)) = nw $ deleteChars n
processEscapeSequence (Esc_CSI (CSI_DeleteLines n)) = nw $ deleteLines n
processEscapeSequence (Esc_CSI (CSI_CursorCharacterAbsolute col)) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, col - 1) term
processEscapeSequence (Esc_CSI (CSI_CharacterPositionAbsolute col)) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, col - 1) term
processEscapeSequence (Esc_CSI (CSI_CursorPosition row col)) = nw $ cursorMoveAbsoluteTo (row - 1, col - 1)
processEscapeSequence (Esc_CSI (CSI_HorizontalVerticalPosition row col)) = nw $ cursorMoveAbsoluteTo (row - 1, col - 1)
processEscapeSequence (Esc_CSI (CSI_LinePositionAbsolute row)) = nw $ \term -> cursorMoveAbsoluteTo (row - 1, term ^. cursorPos . _2) term
processEscapeSequence (Esc_CSI (CSI_ScrollUp n)) = nw $ \term -> scrollUp (term ^. scrollTop) n term
processEscapeSequence (Esc_CSI (CSI_ScrollDown n)) = nw $ \term -> scrollDown (term ^. scrollTop) n term
processEscapeSequence (Esc_CSI (CSI_EraseInDisplay param)) = nw $ eraseInDisplay param
processEscapeSequence (Esc_CSI (CSI_WindowManipulation param)) = nw $ windowManipulation param
processEscapeSequence (Esc_CSI (CSI_DeviceStatusReport param)) = deviceStatusReport param
processEscapeSequence (Esc_CSI (CSI_SoftTerminalReset)) = nw $ softTerminalReset
processEscapeSequence (Esc_CSI (CSI_SetMode param)) = nw $ noSurfaceChanges $ setMode param
processEscapeSequence (Esc_CSI (CSI_ResetMode param)) = nw $ noSurfaceChanges $ resetMode param
processEscapeSequence (Esc_CSI (CSI_SendDeviceAttributes)) = sendDeviceAttributes
processEscapeSequence (Esc_CSI (CSI_SendDeviceAttributesSecondary param)) = sendDeviceAttributesSecondary param
processEscapeSequence (Esc_CSI (CSI_RequestDECPrivateMode _i)) = ignore -- TODO (?)
processEscapeSequence (Esc_CSI (CSI_DECSTBM top bottom)) = nw $ (setScrollingRegion top bottom) >>> cursorMoveAbsoluteTo (0, 0)
processEscapeSequence (Esc_CSI (CSI_DECSET decset)) = nw $ termProcessDecset decset
processEscapeSequence (Esc_CSI (CSI_DECSET_Unknown _code)) = ignore -- TODO Log this
processEscapeSequence (Esc_CSI (CSI_DECRST decset)) = nw $ termProcessDecrst decset
processEscapeSequence (Esc_CSI (CSI_DECRST_Unknown _code)) = ignore -- TODO Log this
processEscapeSequence (Esc_CSI (CSI_SGR sgrs)) = nw $ noSurfaceChanges $ \term -> V.foldl' (flip termProcessSGR) term sgrs
processEscapeSequence (Esc_OSC osc) = processOsc osc

putTabs :: Int -> Term -> (Seq TermSurfaceChange, Term)
putTabs n term =
  let term' = putTabs' n term
   in (Seq.singleton (TermSurfaceChange.MoveCursor (term' ^. cursorPos . _1) (term' ^. cursorPos . _2)), term')

putTabs' :: Int -> Term -> Term
putTabs' n
  | n >= 0 = \term -> (iterate' putTabForward term) !! n
  | otherwise = \term -> (iterate' putTabBackward term) !! (negate n)
  where
    tabspaces = 8
    putTabForward :: Term -> Term
    putTabForward term = ((cursorPos . _2) .~ (limit 0 (term ^. numCols - 1) col')) term
      where
        col = term ^. cursorPos . _2
        col' = ((col + tabspaces) `div` tabspaces) * tabspaces
    putTabBackward :: Term -> Term
    putTabBackward term
      | col == 0 = term
      | otherwise = ((cursorPos . _2) .~ (limit 0 (term ^. numCols - 1) col')) term
      where
        col = term ^. cursorPos . _2
        col' = ((col - 1) `div` tabspaces) * tabspaces

processLF :: Term -> (Seq TermSurfaceChange, Term)
processLF term = addNewline (keyboardState_CRLF (term ^. keyboardState)) term

eraseInLine :: EraseInLineParam -> Term -> (Seq TermSurfaceChange, Term)
eraseInLine ClearFromCursorToEndOfLine term = clearRegion (term ^. cursorPos) (term ^. cursorPos ^. _1, (term ^. numCols) - 1) term
eraseInLine ClearFromCursorToBeginningOfLine term = clearRegion (term ^. cursorPos ^. _1, 0) (term ^. cursorPos) term
eraseInLine ClearEntireLine term = clearRegion (term ^. cursorPos ^. _1, 0) (term ^. cursorPos ^. _1, (term ^. numCols) - 1) term

eraseCharacters :: Int -> Term -> (Seq TermSurfaceChange, Term)
eraseCharacters n term = clearRegion (term ^. cursorPos) ((_2 %~ ((subtract 1) . (+ n))) (term ^. cursorPos)) term

reverseIndex :: Term -> (Seq TermSurfaceChange, Term)
reverseIndex term
  | term ^. cursorPos . _1 == term ^. scrollTop = scrollDown (term ^. scrollTop) 1 term
  | otherwise = cursorMoveTo ((term ^. cursorPos . _1) - 1, term ^. cursorPos . _2) term

eraseInDisplay :: EraseInDisplayParam -> Term -> (Seq TermSurfaceChange, Term)
eraseInDisplay EraseAbove term = noSurfaceChanges id term -- error "TODO EraseAbove"
eraseInDisplay EraseBelow term = (clearToEndOfLine >>>> clearBelow) term
  where
    clearToEndOfLine = clearRegion (term ^. cursorPos) ((_2 .~ ((term ^. numCols) - 1)) (term ^. cursorPos))
    clearBelow :: Term -> (Seq TermSurfaceChange, Term)
    clearBelow
      | term ^. cursorPos . _1 < term ^. numRows - 1 =
        clearRegion
          (((_1 %~ (+ 1)) >>> (_2 .~ 0)) (term ^. cursorPos))
          (((_1 .~ ((term ^. numRows) - 1)) >>> (_2 .~ ((term ^. numCols) - 1))) (term ^. cursorPos))
      | otherwise = noSurfaceChanges id
eraseInDisplay EraseAll term = clearRegion (0, 0) ((term ^. numRows) - 1, (term ^. numCols) - 1) term
eraseInDisplay EraseSavedLines term = (Seq.singleton TermSurfaceChange.ClearScrollBack, (scrollBackLines .~ TL.empty) term)

windowManipulation :: WindowManipulation -> Term -> (Seq TermSurfaceChange, Term)
windowManipulation SaveIconAndWindowTitleOnStack = noSurfaceChanges id -- TODO We could add a stack to our 'Term' data structure and save this
windowManipulation RestoreIconAndWindowTitleOnStack = noSurfaceChanges id -- TODO We could add a stack to our 'Term' data structure and save this

deviceStatusReport :: DeviceStatusReport -> Term -> (ByteString, Seq TermSurfaceChange, Term)
deviceStatusReport param term = case param of
  StatusReport ->
    let ok = "\ESC[0n"
     in (ok, mempty, term)
  ReportCursorPosition ->
    let (line, col) = term ^. cursorPos
        lineStr = BC8.pack (show (line + 1))
        colStr = BC8.pack (show (col + 1))
        cpr = "\ESC[" <> lineStr <> ";" <> colStr <> "R"
     in (cpr, mempty, term)

sendDeviceAttributes :: Term -> (ByteString, Seq TermSurfaceChange, Term)
sendDeviceAttributes term =
  let identification = "\ESC[?1;2c" -- TODO or maybe "\ESC[?6c" ?
   in (identification, mempty, term)

sendDeviceAttributesSecondary :: SendDeviceAttributesSecondary -> Term -> (ByteString, Seq TermSurfaceChange, Term)
sendDeviceAttributesSecondary RequestTerminalIdentificationCode term =
  let identification = "\ESC[>0;0;0c"
   in (identification, mempty, term)

softTerminalReset :: Term -> (Seq TermSurfaceChange, Term)
softTerminalReset term =
  let term' = mkTerm (term ^. numCols, term ^. numRows)
   in ( Seq.fromList
          ( map
              (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen . TL.vIndex row))
              [0 .. (term' ^. numRows) - 1]
          )
          <> Seq.singleton (TermSurfaceChange.MoveCursor (term' ^. cursorPos . _1) (term' ^. cursorPos . _2))
          <> Seq.singleton TermSurfaceChange.ClearScrollBack
          <> Seq.singleton (TermSurfaceChange.SetWindowTitle (term' ^. windowTitle)),
        term'
      )

setMode :: Mode -> Term -> Term
setMode KeyboardActionMode = keyboardState %~ (\state -> state {keyboardState_Locked = True})
setMode InsertReplaceMode = insertMode .~ True
setMode SendReceive = id -- error "TODO Send/receive (SRM) Not Supported"
setMode AutomaticNewlineNormalLinefeed = keyboardState %~ (\state -> state {keyboardState_CRLF = True})

resetMode :: Mode -> Term -> Term
resetMode KeyboardActionMode = keyboardState %~ (\state -> state {keyboardState_Locked = False})
resetMode InsertReplaceMode = insertMode .~ False
resetMode SendReceive = id
resetMode AutomaticNewlineNormalLinefeed = keyboardState %~ (\state -> state {keyboardState_CRLF = False})

processOsc :: OperatingSystemCommand -> Term -> (ByteString, Seq TermSurfaceChange, Term)
processOsc (OSC_SetTitle _ True str) = nw $ \term -> (Seq.singleton (TermSurfaceChange.SetWindowTitle str), (windowTitle .~ str) term)
processOsc (OSC_SetTitle _ False _) = ignore -- set window icon not supported
processOsc (OSC_ChangeTextForegroundColor _) = ignore -- Ignore
processOsc (OSC_ChangeTextBackgroundColor _) = ignore -- Ignore
processOsc OSC_RequestTextForegroundColor = \term -> ("\ESC]10;0\a", mempty, term)
processOsc OSC_RequestTextBackgroundColor = \term -> ("\ESC]11;0\a", mempty, term)
processOsc OSC_ResetTextCursorColor = ignore

insertBlankChars :: Int -> Term -> (Seq TermSurfaceChange, Term)
insertBlankChars n term =
  let term' = (cursorLine %~ updateLine) term
   in (Seq.singleton (TermSurfaceChange.UpdateLine (term' ^. cursorPos . _1) (term' ^. cursorLine)), term')
  where
    n' = limit 0 (term ^. numCols - term ^. cursorPos . _2) n
    col = term ^. cursorPos . _2
    updateLine :: TermLine -> TermLine
    updateLine termLine =
      start <> blanks <> rest
      where
        start = VU.take col termLine
        blanks = VU.replicate n' (' ', term ^. termAttrs)
        rest = VU.slice col (term ^. numCols - col - n') termLine

insertBlankLines :: Int -> Term -> (Seq TermSurfaceChange, Term)
insertBlankLines n term
  | between (term ^. scrollTop, term ^. scrollBottom) (term ^. cursorPos . _1) = scrollDown (term ^. cursorPos . _1) n term
  | otherwise = (mempty, term)

deleteChars :: Int -> Term -> (Seq TermSurfaceChange, Term)
deleteChars n term =
  let term' = (cursorLine %~ updateLine) term
   in (Seq.singleton (TermSurfaceChange.UpdateLine (term' ^. cursorPos . _1) (term' ^. cursorLine)), term')
  where
    n' = limit 0 ((term ^. numCols) - (term ^. cursorPos . _2)) n
    srcCol = col + n'
    size = term ^. numCols - srcCol
    col = term ^. cursorPos . _2
    updateLine :: TermLine -> TermLine
    updateLine termLine =
      start <> slice <> VU.replicate n' (' ', term ^. termAttrs)
      where
        start = VU.take col termLine
        slice = VU.slice srcCol size termLine

deleteLines :: Int -> Term -> (Seq TermSurfaceChange, Term)
deleteLines n term
  | between (term ^. scrollTop, term ^. scrollBottom) (term ^. cursorPos . _1) = scrollUp (term ^. cursorPos . _1) n term
  | otherwise = (mempty, term)

setScrollingRegion :: Maybe Int -> Maybe Int -> Term -> Term
setScrollingRegion mbTop mbBottom term =
  ((scrollTop .~ top) >>> (scrollBottom .~ bottom)) term
  where
    top1 = case mbTop of
      Nothing -> 0
      Just t -> t - 1
    bottom1 = case mbBottom of
      Nothing -> term ^. numRows - 1
      Just b -> b - 1
    minY = 0
    maxY = term ^. numRows - 1
    top2 = limit minY maxY top1
    bottom2 = limit minY maxY bottom1
    (top, bottom) = if top2 > bottom2 then (bottom2, top2) else (top2, bottom2)

termProcessDecset :: DECPrivateMode -> Term -> (Seq TermSurfaceChange, Term)
termProcessDecset DECPrivateMode.DECCKM = noSurfaceChanges $ keyboardState %~ (\state -> state {keyboardState_DECCKM = True})
termProcessDecset DECPrivateMode.DECOM = (cursorState . origin .~ True) >>> (cursorMoveAbsoluteTo (0, 0))
termProcessDecset DECPrivateMode.ReportButtonPress = noSurfaceChanges id
termProcessDecset DECPrivateMode.BracketedPasteMode = noSurfaceChanges id -- TODO Set flag on 'Term'
termProcessDecset DECPrivateMode.SaveCursorAsInDECSCAndUseAlternateScreenBuffer = \term ->
  let term' = (altScreenActive .~ True) term
   in ( Seq.fromList
          ( map
              (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen . TL.vIndex row))
              [0 .. (term ^. numRows) - 1]
          )
          <> Seq.singleton (TermSurfaceChange.ClearScrollBack),
        term'
      )
termProcessDecset DECPrivateMode.Att610 = noSurfaceChanges id -- TODO Set flag on 'Term'
termProcessDecset DECPrivateMode.DECTCEM = noSurfaceChanges id -- TODO Set flag on 'Term'
termProcessDecset DECPrivateMode.DECAWM = noSurfaceChanges $ modeWrap .~ True
termProcessDecset _other = noSurfaceChanges id -- error $ "TODO: DECSET: " <> show other

termProcessDecrst :: DECPrivateMode -> Term -> (Seq TermSurfaceChange, Term)
termProcessDecrst DECPrivateMode.DECCKM = noSurfaceChanges $ keyboardState %~ (\state -> state {keyboardState_DECCKM = False})
termProcessDecrst DECPrivateMode.DECOM = noSurfaceChanges (cursorState . origin .~ False) >>>> (cursorMoveAbsoluteTo (0, 0))
termProcessDecrst DECPrivateMode.Att610 = noSurfaceChanges id -- TODO Unset flag on 'Term'
termProcessDecrst DECPrivateMode.DECTCEM = noSurfaceChanges id -- TODO Unset flag on 'Term'
termProcessDecrst DECPrivateMode.DECCOLM = noSurfaceChanges id -- Ignored
termProcessDecrst DECPrivateMode.ReportButtonPress = noSurfaceChanges id
termProcessDecrst DECPrivateMode.BracketedPasteMode = noSurfaceChanges id -- TODO Unset flag on 'Term'
termProcessDecrst DECPrivateMode.SaveCursorAsInDECSCAndUseAlternateScreenBuffer = \term ->
  let term' = (altScreenActive .~ False) term
   in ( Seq.fromList
          ( map
              (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen . TL.vIndex row))
              [0 .. (term ^. numRows) - 1]
          )
          <> Seq.singleton TermSurfaceChange.ClearScrollBack
          <> fmap TermSurfaceChange.AppendScrollBack (TL.toSeq (term' ^. scrollBackLines)),
        term'
      )
termProcessDecrst DECPrivateMode.DECAWM = noSurfaceChanges $ modeWrap .~ False
termProcessDecrst DECPrivateMode.EnableAllMouseMotions = noSurfaceChanges id
termProcessDecrst DECPrivateMode.ReportMotionOnButtonPress = noSurfaceChanges id
termProcessDecrst _other = noSurfaceChanges id -- error $ "TODO: DECRST: " <> show other

termProcessSGR :: SGR.SGR -> Term -> Term
termProcessSGR = over termAttrs . applySGR

-- | For absolute user moves, when DECOM is set
cursorMoveAbsoluteTo :: (Int, Int) -> Term -> (Seq TermSurfaceChange, Term)
cursorMoveAbsoluteTo (row, col) term =
  cursorMoveTo (row + rowOffset, col) term
  where
    rowOffset
      | term ^. cursorState . origin = term ^. scrollTop
      | otherwise = 0

cursorMoveTo :: (Int, Int) -> Term -> (Seq TermSurfaceChange, Term)
cursorMoveTo (row, col) term =
  let newRow = limit minY maxY row
      newCol = limit minX maxX col
   in ( Seq.singleton (TermSurfaceChange.MoveCursor newRow newCol),
        ( cursorPos . _1 .~ newRow
            >>> cursorPos . _2 .~ newCol
            >>> cursorState . wrapNext .~ False
        )
          term
      )
  where
    minX = 0
    maxX = term ^. numCols - 1
    (minY, maxY)
      | term ^. cursorState . origin = (term ^. scrollTop, term ^. scrollBottom)
      | otherwise = (0, term ^. numRows - 1)

applySGR :: SGR.SGR -> Attrs -> Attrs
applySGR SGR.Reset = const blankAttrs
applySGR (SGR.SetConsoleIntensity intensity) = set attrsIntensity intensity
applySGR (SGR.SetItalicized _) = id -- TODO Not Supported
applySGR (SGR.SetUnderlining underlining) = set attrsUnderline underlining
applySGR (SGR.SetBlinkSpeed _) = id -- TODO Not Supported
applySGR (SGR.SetVisible _) = id -- TODO Not Supported
applySGR (SGR.SetSwapForegroundBackground _) = id -- TODO Not Supported
applySGR (SGR.SetColor SGR.Foreground intensity color) = set attrsFg (Just (intensity, color))
applySGR (SGR.SetColor SGR.Background intensity color) = set attrsBg (Just (intensity, color))
applySGR (SGR.SetRGBColor _ _) = id -- TODO Not Supported
applySGR (SGR.SetPaletteColor _ _) = id -- TODO Not Supported
applySGR (SGR.SetDefaultColor SGR.Foreground) = set attrsFg Nothing
applySGR (SGR.SetDefaultColor SGR.Background) = set attrsBg Nothing

(>>>>) :: (Term -> (Seq TermSurfaceChange, Term)) -> (Term -> (Seq TermSurfaceChange, Term)) -> (Term -> (Seq TermSurfaceChange, Term))
f1 >>>> f2 = \term ->
  let (cs1, term') = f1 term
      (cs2, term'') = f2 term'
   in (cs1 <> cs2, term'')

processVisibleChar :: Char -> Term -> (Seq TermSurfaceChange, Term)
processVisibleChar c =
  moveCursorBefore
    >>>> moveChars
    >>>> moveCursorDown
    >>>> setChar
    >>>> moveCursorAfter
  where
    moveCursorBefore :: Term -> (Seq TermSurfaceChange, Term)
    moveCursorBefore term
      | (term ^. modeWrap) && (term ^. cursorState ^. wrapNext) = addNewline True term
      | otherwise = (mempty, term)
    moveChars :: Term -> (Seq TermSurfaceChange, Term)
    moveChars term
      | (term ^. insertMode) && (col < (term ^. numCols) - 1) =
        ( mempty,
          ( cursorLine
              %~ ( \line ->
                     VU.take
                       (term ^. numCols)
                       (VU.take col line <> VU.singleton (' ', 0) <> VU.drop col line)
                 )
          )
            term
        )
      | otherwise = (mempty, term)
      where
        col = term ^. cursorPos . _2
    moveCursorDown :: Term -> (Seq TermSurfaceChange, Term)
    moveCursorDown term
      | term ^. cursorPos . _2 > (term ^. numCols) - 1 = addNewline True term
      | otherwise = (mempty, term)
    setChar :: Term -> (Seq TermSurfaceChange, Term)
    setChar term =
      let row = term ^. cursorPos . _1
          oldLine = term ^. cursorLine
          newLine = ((vuIndex (term ^. cursorPos . _2)) .~ (c, term ^. termAttrs)) oldLine
       in (Seq.singleton (TermSurfaceChange.UpdateLine row newLine), ((cursorLine .~ newLine) term))
    moveCursorAfter :: Term -> (Seq TermSurfaceChange, Term)
    moveCursorAfter term
      | term ^. cursorPos . _2 < (term ^. numCols) - 1 = cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) + 1) term
      | otherwise = (mempty, ((cursorState . wrapNext) .~ True) term)

addNewline ::
  -- | first column
  Bool ->
  Term ->
  (Seq TermSurfaceChange, Term)
addNewline firstCol = doScrollUp >>>> moveCursor
  where
    doScrollUp :: Term -> (Seq TermSurfaceChange, Term)
    doScrollUp term
      | term ^. cursorPos . _1 == term ^. scrollBottom = scrollUp (term ^. scrollTop) 1 term
      | otherwise = (mempty, term)
    moveCursor :: Term -> (Seq TermSurfaceChange, Term)
    moveCursor term = cursorMoveTo (newRow, newCol) term
      where
        newRow
          | term ^. cursorPos . _1 == term ^. scrollBottom = term ^. cursorPos . _1
          | otherwise = (term ^. cursorPos . _1) + 1
        newCol
          | firstCol = 0
          | otherwise = term ^. cursorPos . _2

scrollDown :: Int -> Int -> Term -> (Seq TermSurfaceChange, Term)
scrollDown orig n term = scrollLines term
  where
    n' = limit 0 (term ^. scrollBottom - orig + 1) n
    scrollLines :: Term -> (Seq TermSurfaceChange, Term)
    scrollLines t =
      let term' =
            ( activeScreen
                %~ ( \lines ->
                       TL.take orig lines
                         <> TL.replicate n' newBlankLine
                         <> TL.take ((term ^. scrollBottom) - orig - n' + 1) (TL.drop orig lines)
                         <> TL.drop ((term ^. scrollBottom) + 1) lines
                   )
            )
              t
       in (Seq.fromList (map (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen ^. TL.vIndex row)) [orig .. (term ^. scrollBottom)]), term')
    newBlankLine = VU.replicate (term ^. numCols) (' ', term ^. termAttrs)

scrollUp :: Int -> Int -> Term -> (Seq TermSurfaceChange, Term)
scrollUp orig n term =
  (copyLinesToScrollBack >>>> scrollLines) term
  where
    n' = limit 0 (term ^. scrollBottom - orig + 1) n
    copyLinesToScrollBack :: Term -> (Seq TermSurfaceChange, Term)
    copyLinesToScrollBack
      | not (term ^. altScreenActive) && orig == 0 = \t ->
        let newScrollBackLines = TL.take n' (t ^. termScreen)
         in (TL.toSeq (fmap TermSurfaceChange.AppendScrollBack newScrollBackLines), addScrollBackLines newScrollBackLines t)
      | otherwise = noSurfaceChanges id
    scrollLines :: Term -> (Seq TermSurfaceChange, Term)
    scrollLines t =
      let term' =
            ( activeScreen
                %~ ( \lines ->
                       TL.take orig lines
                         <> TL.take ((term ^. scrollBottom) - orig - n' + 1) (TL.drop (orig + n') lines)
                         <> TL.replicate n' newBlankLine
                         <> TL.drop ((term ^. scrollBottom) + 1) lines
                   )
            )
              t
       in (Seq.fromList (map (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen ^. TL.vIndex row)) [orig .. (term ^. scrollBottom)]), term')
    newBlankLine = VU.replicate (term ^. numCols) (' ', term ^. termAttrs)

clearRegion :: (Int, Int) -> (Int, Int) -> Term -> (Seq TermSurfaceChange, Term)
clearRegion (line1, col1) (line2, col2) term =
  let term' =
        foldl'
          (\t line -> clearRow line startCol endCol t)
          term
          [startLine .. endLine]
   in (Seq.fromList (map (\row -> TermSurfaceChange.UpdateLine row (term' ^. activeScreen . TL.vIndex row)) [startLine .. endLine]), term')
  where
    line1' = min line1 line2
    line2' = max line1 line2
    col1' = min col1 col2
    col2' = max col1 col2
    minX = 0
    maxX = term ^. numCols - 1
    minY = 0
    maxY = term ^. numRows - 1
    startLine = limit minY maxY line1'
    endLine = limit minY maxY line2'
    startCol = limit minX maxX col1'
    endCol = limit minX maxX col2'

clearRow :: Int -> Int -> Int -> Term -> Term
clearRow line startCol endCol term =
  foldl'
    (\t col -> (activeScreen . TL.vIndex line . vuIndex col .~ (' ', attrs)) t)
    term
    [startCol .. endCol]
  where
    attrs = term ^. termAttrs

limit ::
  -- | minimum allowed value
  Int ->
  -- | maximum allowed value
  Int ->
  -- | value to limit
  Int ->
  Int
limit minVal maxVal val
  | val < minVal = minVal
  | val > maxVal = maxVal
  | otherwise = val

between :: Ord a => (a, a) -> a -> Bool
between (low, high) val = val >= low && val <= high
