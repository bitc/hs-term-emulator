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
import Prelude hiding (lines)

processTermAtoms :: [TermAtom] -> Term -> (ByteString, Term)
processTermAtoms termAtoms term =
  foldl'
    ( \(!w1, !t) termAtom ->
        let (!w2, !t') = processTermAtom termAtom t
         in (w1 <> w2, t')
    )
    (B.empty, term)
    termAtoms

processTermAtom :: TermAtom -> Term -> (ByteString, Term)
processTermAtom (TermAtom_VisibleChar char) = nw $ processVisibleChar char
processTermAtom (TermAtom_SingleCharacterFunction Control_Bell) = nw id -- TODO
processTermAtom (TermAtom_SingleCharacterFunction Control_Backspace) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) - 1) term
processTermAtom (TermAtom_SingleCharacterFunction Control_Tab) = nw $ putTabs 1
processTermAtom (TermAtom_SingleCharacterFunction Control_LineFeed) = nw $ processLF
processTermAtom (TermAtom_SingleCharacterFunction Control_VerticalTab) = nw $ processLF
processTermAtom (TermAtom_SingleCharacterFunction Control_FormFeed) = nw $ processLF
processTermAtom (TermAtom_SingleCharacterFunction Control_CarriageReturn) = nw $ \term -> cursorMoveTo (term ^. cursorPos . _1, 0) term
processTermAtom (TermAtom_SingleCharacterFunction Control_ReturnTerminalStatus) = nw id
processTermAtom (TermAtom_SingleCharacterFunction Control_SwitchToStandardCharacterSet) = nw id
processTermAtom (TermAtom_SingleCharacterFunction Control_SwitchToAlternateCharacterSet) = nw id
processTermAtom (TermAtom_EscapeSequence escapeSequence) = processEscapeSequence escapeSequence
processTermAtom (TermAtom_SingleCharacterFunctionUnknown x) = error $ "Unknown Character Function: " <> show x
processTermAtom (TermAtom_EscapeSequenceUnknown x)
  | isExpectedInvalidEscSequence x = nw id
  | otherwise = error $ "Unknown ESC seq: " <> show x

-- | No-write operation
nw :: (Term -> Term) -> Term -> (ByteString, Term)
nw f term = (B.empty, f term)

-- | I have observed some invalid ESC sequences in the wild, that I am
-- deciding to ignore for now
isExpectedInvalidEscSequence :: Text -> Bool
isExpectedInvalidEscSequence str
  | ("\ESC[" `T.isPrefixOf` str) && T.any (== '\r') str = True
  | str == "\ESC\r" = True
  | otherwise = False

processEscapeSequence :: EscapeSequence -> Term -> (ByteString, Term)
processEscapeSequence Esc_ReverseIndex = nw reverseIndex
processEscapeSequence Esc_RIS = nw id -- TODO
processEscapeSequence Esc_DECPAM = nw id -- TODO
processEscapeSequence Esc_DECPNM = nw id -- TODO
processEscapeSequence (ESC_SetG0CharacterSet _) = nw id -- Ignore
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
processEscapeSequence (Esc_CSI (CSI_SetMode param)) = nw $ setMode param
processEscapeSequence (Esc_CSI (CSI_ResetMode param)) = nw $ resetMode param
processEscapeSequence (Esc_CSI (CSI_SendDeviceAttributes)) = sendDeviceAttributes
processEscapeSequence (Esc_CSI (CSI_SendDeviceAttributesSecondary param)) = sendDeviceAttributesSecondary param
processEscapeSequence (Esc_CSI (CSI_RequestDECPrivateMode _i)) = nw id -- TODO (?)
processEscapeSequence (Esc_CSI (CSI_DECSTBM top bottom)) = nw $ (setScrollingRegion top bottom) >>> cursorMoveAbsoluteTo (0, 0)
processEscapeSequence (Esc_CSI (CSI_DECSET decset)) = nw $ termProcessDecset decset
processEscapeSequence (Esc_CSI (CSI_DECSET_Unknown _code)) = nw id -- TODO Log this
processEscapeSequence (Esc_CSI (CSI_DECRST decset)) = nw $ termProcessDecrst decset
processEscapeSequence (Esc_CSI (CSI_DECRST_Unknown _code)) = nw id -- TODO Log this
processEscapeSequence (Esc_CSI (CSI_SGR sgrs)) = nw $ \term -> V.foldl' (flip termProcessSGR) term sgrs
processEscapeSequence (Esc_OSC osc) = processOsc osc

putTabs :: Int -> Term -> Term
putTabs n
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

processLF :: Term -> Term
processLF term = addNewline (keyboardState_CRLF (term ^. keyboardState)) term

eraseInLine :: EraseInLineParam -> Term -> Term
eraseInLine ClearFromCursorToEndOfLine term = clearRegion (term ^. cursorPos) (term ^. cursorPos ^. _1, (term ^. numCols) - 1) term
eraseInLine ClearFromCursorToBeginningOfLine term = clearRegion (term ^. cursorPos ^. _1, 0) (term ^. cursorPos) term
eraseInLine ClearEntireLine term = clearRegion (term ^. cursorPos ^. _1, 0) (term ^. cursorPos ^. _1, (term ^. numCols) - 1) term

eraseCharacters :: Int -> Term -> Term
eraseCharacters n term = clearRegion (term ^. cursorPos) ((_2 %~ ((subtract 1) . (+ n))) (term ^. cursorPos)) term

reverseIndex :: Term -> Term
reverseIndex term
  | term ^. cursorPos . _1 == term ^. scrollTop = scrollDown (term ^. scrollTop) 1 term
  | otherwise = cursorMoveTo ((term ^. cursorPos . _1) - 1, term ^. cursorPos . _2) term

eraseInDisplay :: EraseInDisplayParam -> Term -> Term
eraseInDisplay EraseAbove _ = error "TODO EraseAbove"
eraseInDisplay EraseBelow term = (clearToEndOfLine >>> clearBelow) term
  where
    clearToEndOfLine = clearRegion (term ^. cursorPos) ((_2 .~ ((term ^. numCols) - 1)) (term ^. cursorPos))
    clearBelow
      | term ^. cursorPos . _1 < term ^. numRows - 1 =
        clearRegion
          (((_1 %~ (+ 1)) >>> (_2 .~ 0)) (term ^. cursorPos))
          (((_1 .~ ((term ^. numRows) - 1)) >>> (_2 .~ ((term ^. numCols) - 1))) (term ^. cursorPos))
      | otherwise = id
eraseInDisplay EraseAll term = clearRegion (0, 0) ((term ^. numRows) - 1, (term ^. numCols) - 1) term
eraseInDisplay EraseSavedLines term = (scrollBackLines .~ TL.empty) term

windowManipulation :: WindowManipulation -> Term -> Term
windowManipulation SaveIconAndWindowTitleOnStack = id -- TODO We could add a stack to our 'Term' data structure and save this
windowManipulation RestoreIconAndWindowTitleOnStack = id -- TODO We could add a stack to our 'Term' data structure and save this

deviceStatusReport :: DeviceStatusReport -> Term -> (ByteString, Term)
deviceStatusReport param term = case param of
  StatusReport ->
    let ok = "\ESC[0n"
     in (ok, term)
  ReportCursorPosition ->
    let (line, col) = term ^. cursorPos
        lineStr = BC8.pack (show (line + 1))
        colStr = BC8.pack (show (col + 1))
        cpr = "\ESC[" <> lineStr <> ";" <> colStr <> "R"
     in (cpr, term)

sendDeviceAttributes :: Term -> (ByteString, Term)
sendDeviceAttributes term =
  let identification = "\ESC[?1;2c" -- TODO or maybe "\ESC[?6c" ?
   in (identification, term)

sendDeviceAttributesSecondary :: SendDeviceAttributesSecondary -> Term -> (ByteString, Term)
sendDeviceAttributesSecondary RequestTerminalIdentificationCode term =
  let identification = "\ESC[>0;0;0c"
   in (identification, term)

softTerminalReset :: Term -> Term
softTerminalReset term = mkTerm (term ^. numCols, term ^. numRows)

setMode :: Mode -> Term -> Term
setMode KeyboardActionMode = keyboardState %~ (\state -> state {keyboardState_Locked = True})
setMode InsertReplaceMode = insertMode .~ True
setMode SendReceive = error "TODO Send/receive (SRM) Not Supported"
setMode AutomaticNewlineNormalLinefeed = keyboardState %~ (\state -> state {keyboardState_CRLF = True})

resetMode :: Mode -> Term -> Term
resetMode KeyboardActionMode = keyboardState %~ (\state -> state {keyboardState_Locked = False})
resetMode InsertReplaceMode = insertMode .~ False
resetMode SendReceive = id
resetMode AutomaticNewlineNormalLinefeed = keyboardState %~ (\state -> state {keyboardState_CRLF = False})

processOsc :: OperatingSystemCommand -> Term -> (ByteString, Term)
processOsc (OSC_SetTitle _ True str) = nw $ windowTitle .~ str
processOsc (OSC_SetTitle _ False _) = nw id -- set window icon not supported
processOsc (OSC_ChangeTextForegroundColor _) = nw id -- Ignore
processOsc (OSC_ChangeTextBackgroundColor _) = nw id -- Ignore
processOsc OSC_RequestTextForegroundColor = \term -> ("\ESC]10;0\a", term)
processOsc OSC_RequestTextBackgroundColor = \term -> ("\ESC]11;0\a", term)
processOsc OSC_ResetTextCursorColor = nw id

insertBlankChars :: Int -> Term -> Term
insertBlankChars n term = (cursorLine %~ updateLine) term
  where
    n' = limit 0 (term ^. numCols - term ^. cursorPos . _2) n
    col = term ^. cursorPos . _2
    updateLine :: TermLine -> TermLine
    updateLine termLine =
      start <> blanks <> rest
      where
        start = VU.take col termLine
        blanks = VU.replicate n' (' ', term ^. termAttrs)
        rest = VU.slice col (term ^. numCols - col) termLine

insertBlankLines :: Int -> Term -> Term
insertBlankLines n term
  | between (term ^. scrollTop, term ^. scrollBottom) (term ^. cursorPos . _1) = scrollDown (term ^. cursorPos . _1) n term
  | otherwise = term

deleteChars :: Int -> Term -> Term
deleteChars n term = (cursorLine %~ updateLine) term
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

deleteLines :: Int -> Term -> Term
deleteLines n term
  | between (term ^. scrollTop, term ^. scrollBottom) (term ^. cursorPos . _1) = scrollUp (term ^. cursorPos . _1) n term
  | otherwise = term

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

termProcessDecset :: DECPrivateMode -> Term -> Term
termProcessDecset DECPrivateMode.DECCKM = keyboardState %~ (\state -> state {keyboardState_DECCKM = True})
termProcessDecset DECPrivateMode.DECOM = (cursorState . origin .~ True) >>> (cursorMoveAbsoluteTo (0, 0))
termProcessDecset DECPrivateMode.ReportButtonPress = id
termProcessDecset DECPrivateMode.BracketedPasteMode = id -- TODO Set flag on 'Term'
termProcessDecset DECPrivateMode.SaveCursorAsInDECSCAndUseAlternateScreenBuffer = altScreenActive .~ True
termProcessDecset DECPrivateMode.Att610 = id -- TODO Set flag on 'Term'
termProcessDecset DECPrivateMode.DECTCEM = id -- TODO Set flag on 'Term'
termProcessDecset DECPrivateMode.DECAWM = modeWrap .~ True
termProcessDecset other = error $ "TODO: DECSET: " <> show other

termProcessDecrst :: DECPrivateMode -> Term -> Term
termProcessDecrst DECPrivateMode.DECCKM = keyboardState %~ (\state -> state {keyboardState_DECCKM = False})
termProcessDecrst DECPrivateMode.DECOM = (cursorState . origin .~ False) >>> (cursorMoveAbsoluteTo (0, 0))
termProcessDecrst DECPrivateMode.Att610 = id -- TODO Unset flag on 'Term'
termProcessDecrst DECPrivateMode.DECTCEM = id -- TODO Unset flag on 'Term'
termProcessDecrst DECPrivateMode.DECCOLM = id -- Ignored
termProcessDecrst DECPrivateMode.ReportButtonPress = id
termProcessDecrst DECPrivateMode.BracketedPasteMode = id -- TODO Unset flag on 'Term'
termProcessDecrst DECPrivateMode.SaveCursorAsInDECSCAndUseAlternateScreenBuffer = altScreenActive .~ False
termProcessDecrst DECPrivateMode.DECAWM = modeWrap .~ False
termProcessDecrst DECPrivateMode.EnableAllMouseMotions = id
termProcessDecrst DECPrivateMode.ReportMotionOnButtonPress = id
termProcessDecrst other = error $ "TODO: DECRST: " <> show other

termProcessSGR :: SGR.SGR -> Term -> Term
termProcessSGR = over termAttrs . applySGR

-- | For absolute user moves, when DECOM is set
cursorMoveAbsoluteTo :: (Int, Int) -> Term -> Term
cursorMoveAbsoluteTo (row, col) term =
  cursorMoveTo (row + rowOffset, col) term
  where
    rowOffset
      | term ^. cursorState . origin = term ^. scrollTop
      | otherwise = 0

cursorMoveTo :: (Int, Int) -> Term -> Term
cursorMoveTo (row, col) term =
  ( cursorPos . _1 .~ (limit minY maxY row)
      >>> cursorPos . _2 .~ (limit minX maxX col)
      >>> cursorState . wrapNext .~ False
  )
    term
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

processVisibleChar :: Char -> Term -> Term
processVisibleChar c =
  moveCursorBefore
    >>> moveChars
    >>> moveCursorDown
    >>> setChar
    >>> moveCursorAfter
  where
    moveCursorBefore :: Term -> Term
    moveCursorBefore term
      | (term ^. modeWrap) && (term ^. cursorState ^. wrapNext) = addNewline True term
      | otherwise = term
    moveChars :: Term -> Term
    moveChars term
      | (term ^. insertMode) && (col < (term ^. numCols) - 1) =
        ( cursorLine
            %~ ( \line ->
                   VU.take
                     (term ^. numCols)
                     (VU.take col line <> VU.singleton (' ', 0) <> VU.drop col line)
               )
        )
          term
      | otherwise = term
      where
        col = term ^. cursorPos . _2
    moveCursorDown :: Term -> Term
    moveCursorDown term
      | term ^. cursorPos . _2 > (term ^. numCols) - 1 = addNewline True term
      | otherwise = term
    setChar :: Term -> Term
    setChar term = ((cursorLine . (vuIndex (term ^. cursorPos . _2))) .~ (c, term ^. termAttrs)) term
    moveCursorAfter :: Term -> Term
    moveCursorAfter term
      | term ^. cursorPos . _2 < (term ^. numCols) - 1 = cursorMoveTo (term ^. cursorPos . _1, (term ^. cursorPos . _2) + 1) term
      | otherwise = ((cursorState . wrapNext) .~ True) term

addNewline ::
  -- | first column
  Bool ->
  Term ->
  Term
addNewline firstCol = doScrollUp >>> moveCursor
  where
    doScrollUp :: Term -> Term
    doScrollUp term
      | term ^. cursorPos . _1 == term ^. scrollBottom = scrollUp (term ^. scrollTop) 1 term
      | otherwise = term
    moveCursor :: Term -> Term
    moveCursor term = cursorMoveTo (newRow, newCol) term
      where
        newRow
          | term ^. cursorPos . _1 == term ^. scrollBottom = term ^. cursorPos . _1
          | otherwise = (term ^. cursorPos . _1) + 1
        newCol
          | firstCol = 0
          | otherwise = term ^. cursorPos . _2

scrollDown :: Int -> Int -> Term -> Term
scrollDown orig n term = scrollLines term
  where
    n' = limit 0 (term ^. scrollBottom - orig + 1) n
    scrollLines =
      activeScreen
        %~ ( \lines ->
               TL.take orig lines
                 <> TL.replicate n' newBlankLine
                 <> TL.take ((term ^. scrollBottom) - orig - n' + 1) (TL.drop orig lines)
                 <> TL.drop ((term ^. scrollBottom) + 1) lines
           )
    newBlankLine = VU.replicate (term ^. numCols) (' ', term ^. termAttrs)

scrollUp :: Int -> Int -> Term -> Term
scrollUp orig n term =
  (copyLinesToScrollBack >>> scrollLines) term
  where
    n' = limit 0 (term ^. scrollBottom - orig + 1) n
    copyLinesToScrollBack
      | not (term ^. altScreenActive) && orig == 0 = addScrollBackLines (TL.take n' (term ^. termScreen))
      | otherwise = id
    scrollLines =
      activeScreen
        %~ ( \lines ->
               TL.take orig lines
                 <> TL.take ((term ^. scrollBottom) - orig - n' + 1) (TL.drop (orig + n') lines)
                 <> TL.replicate n' newBlankLine
                 <> TL.drop ((term ^. scrollBottom) + 1) lines
           )
    newBlankLine = VU.replicate (term ^. numCols) (' ', term ^. termAttrs)

clearRegion :: (Int, Int) -> (Int, Int) -> Term -> Term
clearRegion (line1, col1) (line2, col2) term =
  foldl'
    (\t line -> clearRow line (limit minX maxX col1') (limit minX maxX col2') t)
    term
    [(limit minY maxY line1') .. (limit minY maxY line2')]
  where
    line1' = min line1 line2
    line2' = max line1 line2
    col1' = min col1 col2
    col2' = max col1 col2
    minX = 0
    maxX = term ^. numCols - 1
    minY = 0
    maxY = term ^. numRows - 1

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
