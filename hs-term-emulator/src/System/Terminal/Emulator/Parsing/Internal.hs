{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.Parsing.Internal where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import System.Terminal.Emulator.DECPrivateMode (intToDECPrivateMode)
import System.Terminal.Emulator.Parsing.Types (ControlSequenceIntroducer (..), DeviceStatusReport (..), EraseInDisplayParam (..), EraseInLineParam (..), EscapeSequence (..), Mode (..), OperatingSystemCommand (..), SendDeviceAttributesSecondary (RequestTerminalIdentificationCode), SingleCharacterFunction (..), TermAtom (..), WindowManipulation (..), codeToSGR)
import Prelude hiding (takeWhile)

parseTermAtom :: Parser TermAtom
parseTermAtom =
  parseVisibleChar <|> parseControl

parseVisibleChar :: Parser TermAtom
parseVisibleChar = TermAtom_VisibleChar <$> satisfy (not . isControl)

-- | This parser always succeeds
parseControl :: Parser TermAtom
parseControl = do
  c <- anyChar
  if c == '\ESC'
    then parseEscape
    else pure $ case singleCharacterFunction c of
      Nothing -> TermAtom_SingleCharacterFunctionUnknown c
      Just f -> TermAtom_SingleCharacterFunction f

singleCharacterFunction :: Char -> Maybe SingleCharacterFunction
singleCharacterFunction '\a' = Just Control_Bell
singleCharacterFunction '\b' = Just Control_Backspace
singleCharacterFunction '\r' = Just Control_CarriageReturn
singleCharacterFunction '\ENQ' = Just Control_ReturnTerminalStatus
singleCharacterFunction '\f' = Just Control_FormFeed
singleCharacterFunction '\n' = Just Control_LineFeed
singleCharacterFunction '\SI' = Just Control_SwitchToStandardCharacterSet
singleCharacterFunction '\SO' = Just Control_SwitchToAlternateCharacterSet
singleCharacterFunction '\t' = Just Control_Tab
singleCharacterFunction '\v' = Just Control_VerticalTab
singleCharacterFunction _ = Nothing

-- | This parser always succeeds
parseEscape :: Parser TermAtom
parseEscape = do
  c <- anyChar
  case c of
    '[' -> handleCsi
    ']' -> handleOsc
    '(' -> handleSetG0CharacterSet
    _ -> handleSingle c
  where
    handleCsi :: Parser TermAtom
    handleCsi = do
      csiInput <- parseControlSequenceIntroducer
      pure $ case processControlSequenceIntroducer csiInput of
        Nothing -> case processOtherControlSequenceIntroducer csiInput of
          Nothing -> TermAtom_EscapeSequenceUnknown (renderCsi csiInput)
          Just csi -> TermAtom_EscapeSequence (Esc_CSI csi)
        Just csi -> TermAtom_EscapeSequence (Esc_CSI csi)

    handleOsc :: Parser TermAtom
    handleOsc = do
      oscInput <- parseOperatingSystemCommand
      pure $ case processOperatingSystemCommand oscInput of
        Nothing -> TermAtom_EscapeSequenceUnknown (renderOsc oscInput)
        Just osc -> TermAtom_EscapeSequence (Esc_OSC osc)

    handleSingle :: Char -> Parser TermAtom
    handleSingle c = pure $ case singleCharacterEscapeSequence c of
      Just e -> TermAtom_EscapeSequence e
      Nothing -> TermAtom_EscapeSequenceUnknown ("\ESC" <> T.singleton c)

    handleSetG0CharacterSet :: Parser TermAtom
    handleSetG0CharacterSet =
      ( choice
          [ string "A",
            string "B",
            string "C",
            string "5",
            string "H",
            string "7",
            string "K",
            string "Q",
            string "9",
            string "R",
            string "f",
            string "Y",
            string "Z",
            string "4",
            string "\">",
            string "%2",
            string "%6",
            string "%=",
            string "=",
            string "`",
            string "E",
            string "6",
            string "0",
            string "<",
            string ">",
            string "\"4",
            string "\"?",
            string "%0",
            string "%5",
            string "&4",
            string "%3",
            string "&5"
          ]
          >>= pure . TermAtom_EscapeSequence . ESC_SetG0CharacterSet
      )
        <|> ( anyChar >>= \c ->
                pure (TermAtom_EscapeSequenceUnknown ("\ESC(" <> T.singleton c))
            )

-----------------------------------------------------------------------
-- CSI (Control Sequence Introducer) sequences
-----------------------------------------------------------------------

data ControlSequenceIntroducerInput = ControlSequenceIntroducerInput !Text
  deriving (Show, Eq)

data ControlSequenceIntroducerComponents
  = ControlSequenceIntroducerComponents
      !Bool
      -- ^ Private?
      !(NonEmpty Int)
      -- ^ Args
      !Char
      -- ^ Mode
  deriving (Show, Eq)

-- | Should be run after reading the sequence @ESC [@
--
-- This parser always succeeds
parseControlSequenceIntroducer :: Parser ControlSequenceIntroducerInput
parseControlSequenceIntroducer = do
  str <- takeTill ((`between` (0x40, 0x7E)) . fromEnum)
  c <- anyChar
  pure (ControlSequenceIntroducerInput ((str) <> T.singleton c))

parseControlSequenceIntroducerComponents :: ControlSequenceIntroducerInput -> Maybe ControlSequenceIntroducerComponents
parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput str) =
  case parseOnly (parser <* endOfInput) str of
    Left _ -> Nothing
    Right val -> Just val
  where
    parser :: Parser ControlSequenceIntroducerComponents
    parser = do
      private <- option False (char '?' >> pure True)
      first <- peekChar'
      args <-
        if isDigit first || first == ';'
          then sepBy (option 0 decimal) (char ';')
          else pure []
      mode <- anyChar
      pure (ControlSequenceIntroducerComponents private (listToNonEmpty 0 args) mode)

listToNonEmpty :: a -> [a] -> NonEmpty a
listToNonEmpty def [] = def :| []
listToNonEmpty _ (x : xs) = x :| xs

processControlSequenceIntroducerComponents :: ControlSequenceIntroducerComponents -> Maybe ControlSequenceIntroducer
processControlSequenceIntroducerComponents (ControlSequenceIntroducerComponents False args mode) = parseCsi mode args
processControlSequenceIntroducerComponents (ControlSequenceIntroducerComponents True args mode) = parsePrivCsi mode args

changeZero :: Int -> Int -> Int
changeZero toVal 0 = toVal
changeZero _ val = val

headChangeZero :: Int -> NonEmpty Int -> Int
headChangeZero toVal args = changeZero toVal (NE.head args)

parseCsi :: Char -> NonEmpty Int -> Maybe ControlSequenceIntroducer
parseCsi mode args = case mode of
  'A' -> Just (CSI_CursorUp (headChangeZero 1 args))
  'B' -> Just (CSI_CursorDown (headChangeZero 1 args))
  'C' -> Just (CSI_CursorForward (headChangeZero 1 args))
  'D' -> Just (CSI_CursorBack (headChangeZero 1 args))
  'K' ->
    CSI_EraseInLine <$> case NE.head args of
      0 -> Just ClearFromCursorToEndOfLine
      1 -> Just ClearFromCursorToBeginningOfLine
      2 -> Just ClearEntireLine
      _ -> Nothing
  '@' -> Just (CSI_InsertBlankCharacters (headChangeZero 1 args))
  'P' -> Just (CSI_DeleteChars (headChangeZero 1 args))
  'G' -> Just (CSI_CursorCharacterAbsolute (headChangeZero 1 args))
  'H' ->
    let (row, col) = case args of
          r :| [] -> (r, 0)
          r :| (c : _) -> (r, c)
     in Just (CSI_CursorPosition (changeZero 1 row) (changeZero 1 col))
  'J' -> case NE.head args of
    0 -> Just (CSI_EraseInDisplay EraseBelow)
    1 -> Just (CSI_EraseInDisplay EraseAbove)
    2 -> Just (CSI_EraseInDisplay EraseAll)
    3 -> Just (CSI_EraseInDisplay EraseSavedLines)
    _ -> Nothing
  'L' -> Just (CSI_InsertBlankLines (headChangeZero 1 args))
  'M' -> Just (CSI_DeleteLines (headChangeZero 1 args))
  'S' -> Just (CSI_ScrollUp (headChangeZero 1 args))
  'T' -> Just (CSI_ScrollDown (headChangeZero 1 args))
  'X' -> Just (CSI_EraseCharacters (headChangeZero 1 args))
  '`' -> Just (CSI_CharacterPositionAbsolute (headChangeZero 1 args))
  'a' -> Just (CSI_CharacterPositionRelative (headChangeZero 1 args))
  'c' -> case args of
    0 :| [] -> Just CSI_SendDeviceAttributes
    _ -> Nothing
  'd' -> Just (CSI_LinePositionAbsolute (headChangeZero 1 args))
  'e' -> Just (CSI_LinePositionRelative (headChangeZero 1 args))
  'f' ->
    let (row, col) = case args of
          r :| [] -> (r, 0)
          r :| (c : _) -> (r, c)
     in Just (CSI_HorizontalVerticalPosition (changeZero 1 row) (changeZero 1 col))
  't' -> case args of
    22 :| 0 : _ -> Just (CSI_WindowManipulation SaveIconAndWindowTitleOnStack)
    23 :| 0 : _ -> Just (CSI_WindowManipulation RestoreIconAndWindowTitleOnStack)
    _ -> Nothing
  'h' -> case args of
    2 :| [] -> Just (CSI_SetMode KeyboardActionMode)
    4 :| [] -> Just (CSI_SetMode InsertReplaceMode)
    12 :| [] -> Just (CSI_SetMode SendReceive)
    20 :| [] -> Just (CSI_SetMode AutomaticNewlineNormalLinefeed)
    _ -> Nothing
  'l' -> case args of
    2 :| [] -> Just (CSI_ResetMode KeyboardActionMode)
    4 :| [] -> Just (CSI_ResetMode InsertReplaceMode)
    12 :| [] -> Just (CSI_ResetMode SendReceive)
    20 :| [] -> Just (CSI_ResetMode AutomaticNewlineNormalLinefeed)
    _ -> Nothing
  'n' -> case NE.head args of
    5 -> Just (CSI_DeviceStatusReport StatusReport)
    6 -> Just (CSI_DeviceStatusReport ReportCursorPosition)
    _ -> Nothing
  'r' ->
    let (top, bottom) = case args of
          t :| [] -> (t, 0)
          t :| (b : _) -> (t, b)
     in Just
          ( CSI_DECSTBM
              (if top == 0 then Nothing else Just top)
              (if bottom == 0 then Nothing else Just bottom)
          )
  'm' -> Just $ CSI_SGR (V.fromList (mapMaybe codeToSGR (NE.toList args)))
  _ -> Nothing

parsePrivCsi :: Char -> NonEmpty Int -> Maybe ControlSequenceIntroducer
parsePrivCsi mode args = case mode of
  'h' ->
    let n = (headChangeZero 1 args)
     in Just $ case intToDECPrivateMode n of
          Just decset -> CSI_DECSET decset
          Nothing -> CSI_DECSET_Unknown n
  'l' ->
    let n = (headChangeZero 1 args)
     in Just $ case intToDECPrivateMode n of
          Just decset -> CSI_DECRST decset
          Nothing -> CSI_DECRST_Unknown n
  _ -> Nothing

processControlSequenceIntroducer :: ControlSequenceIntroducerInput -> Maybe ControlSequenceIntroducer
processControlSequenceIntroducer csiInput =
  parseControlSequenceIntroducerComponents csiInput
    >>= processControlSequenceIntroducerComponents

processOtherControlSequenceIntroducer :: ControlSequenceIntroducerInput -> Maybe ControlSequenceIntroducer
processOtherControlSequenceIntroducer (ControlSequenceIntroducerInput str) =
  case str of
    "!p" -> Just CSI_SoftTerminalReset
    ">c" -> Just (CSI_SendDeviceAttributesSecondary RequestTerminalIdentificationCode)
    ">0c" -> Just (CSI_SendDeviceAttributesSecondary RequestTerminalIdentificationCode)
    _
      | "?" `T.isPrefixOf` str && "$p" `T.isSuffixOf` str ->
        let modeStr = T.init (T.init (T.tail str))
         in case T.decimal modeStr of
              Left _ -> Nothing
              Right (mode, "") -> Just (CSI_RequestDECPrivateMode mode)
              Right (_, _) -> Nothing
    _ -> Nothing

-- | Used for error reporting
renderCsi :: ControlSequenceIntroducerInput -> Text
renderCsi (ControlSequenceIntroducerInput str) = "\ESC[" <> str

-----------------------------------------------------------------------
-- OSC (Operating System Command)
-----------------------------------------------------------------------

data OperatingSystemCommandInput = OperatingSystemCommandInput !Text

-- | Should be run after reading the sequence @ESC ]@
--
-- This parser always succeeds
parseOperatingSystemCommand :: Parser OperatingSystemCommandInput
parseOperatingSystemCommand = do
  str <-
    manyTill'
      anyChar
      ( (char '\a' >> pure ())
          <|> (string "\ESC\\" >> pure ())
      )
  pure (OperatingSystemCommandInput (T.pack str))

-- | Used for error reporting
renderOsc :: OperatingSystemCommandInput -> Text
renderOsc (OperatingSystemCommandInput str) = "\ESC]" <> str <> "\a"

processOperatingSystemCommand :: OperatingSystemCommandInput -> Maybe OperatingSystemCommand
processOperatingSystemCommand (OperatingSystemCommandInput str) =
  case parseOnly (parser <* endOfInput) str of
    Left _ -> Nothing
    Right val -> Just val
  where
    parser :: Parser OperatingSystemCommand
    parser =
      parseSetTitle
        <|> parseChangeTextForegroundColor
        <|> parseRequestTextForegroundColor
        <|> parseChangeTextBackgroundColor
        <|> parseRequestTextBackgroundColor
        <|> parseResetTextCursorColor

    parseSetTitle :: Parser OperatingSystemCommand
    parseSetTitle = do
      (icon, window) <- parseSetTitleMode
      _ <- char ';'
      title <- takeText
      pure (OSC_SetTitle icon window title)

    parseSetTitleMode :: Parser (Bool, Bool)
    parseSetTitleMode =
      (char '0' >> pure (True, True))
        <|> (char '1' >> pure (True, False))
        <|> (char '2' >> pure (False, True))

    parseChangeTextForegroundColor :: Parser OperatingSystemCommand
    parseChangeTextForegroundColor = do
      _ <- string "10;"
      c <- satisfy (/= '?')
      color <- takeText
      pure (OSC_ChangeTextForegroundColor (T.singleton c <> color))

    parseRequestTextForegroundColor :: Parser OperatingSystemCommand
    parseRequestTextForegroundColor = do
      _ <- string "10;?"
      pure OSC_RequestTextForegroundColor

    parseChangeTextBackgroundColor :: Parser OperatingSystemCommand
    parseChangeTextBackgroundColor = do
      _ <- string "11;"
      c <- satisfy (/= '?')
      color <- takeText
      pure (OSC_ChangeTextBackgroundColor (T.singleton c <> color))

    parseRequestTextBackgroundColor :: Parser OperatingSystemCommand
    parseRequestTextBackgroundColor = do
      _ <- string "11;?"
      pure OSC_RequestTextBackgroundColor

    parseResetTextCursorColor :: Parser OperatingSystemCommand
    parseResetTextCursorColor = do
      _ <- string "112"
      pure OSC_ResetTextCursorColor

-----------------------------------------------------------------------
-- Single Character Escape Sequence
-----------------------------------------------------------------------

singleCharacterEscapeSequence :: Char -> Maybe EscapeSequence
singleCharacterEscapeSequence c =
  case c of
    'M' -> Just Esc_ReverseIndex
    'c' -> Just Esc_RIS
    '=' -> Just Esc_DECPAM
    '>' -> Just Esc_DECPNM
    _ -> Nothing

-----------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------

between :: Ord a => a -> (a, a) -> Bool
between val (low, high) = val >= low && val <= high

isControlC0 :: Char -> Bool
isControlC0 c = fromEnum c `between` (0, 0x1F) || c == '\DEL'

isControlC1 :: Char -> Bool
isControlC1 c = fromEnum c `between` (0x80, 0x9f)

isControl :: Char -> Bool
isControl c = isControlC0 c || isControlC1 c
