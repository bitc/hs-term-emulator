{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.KeyboardInput.KeyPressToPty
  ( keyPressToPty,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Char (isControl)
import System.Terminal.Emulator.KeyboardInput (KeyModifiers (..), KeyPress (..), KeyboardState (..), SpecialKey (..))

keyPressToPty :: KeyboardState -> KeyPress -> ByteString
keyPressToPty KeyboardState {keyboardState_Locked = True} _ = ""
keyPressToPty _ (KeyPress_Char c modifiers)
  | isControl c = error $ "Invalid Control Char for KeyPress: " ++ show c
  | otherwise = keyToPty c modifiers
keyPressToPty keyboardState (KeyPress_SpecialKey specialKey modifiers) = specialKeyToPty keyboardState specialKey (pressedModifiers modifiers)

keyToPty :: Char -> KeyModifiers -> ByteString
keyToPty c KeyModifiers {ctrl, alt}
  | ctrl && c >= 'a' && c <= 'z' = escapePrefix <> BC8.singleton (toEnum (fromEnum c - 96))
  | ctrl && c >= 'A' && c <= 'Z' = escapePrefix <> BC8.singleton (toEnum (fromEnum c - 64))
  | otherwise = escapePrefix <> charToByteString c
  where
    escapePrefix
      | alt = "\ESC"
      | otherwise = ""

data ModKey = Alt | Ctrl | Shift
  deriving (Eq, Show)

pressedModifiers :: KeyModifiers -> [ModKey]
pressedModifiers KeyModifiers {alt, ctrl, shift} =
  (if alt then [Alt] else [])
    <> (if ctrl then [Ctrl] else [])
    <> (if shift then [Shift] else [])

specialKeyToPty :: KeyboardState -> SpecialKey -> [ModKey] -> ByteString
specialKeyToPty KeyboardState {keyboardState_DECCKM, keyboardState_DECPAM} specialKey modKeys =
  case specialKey of
    SpecialKey_Escape
      | Alt `elem` modKeys -> "\ESC\ESC"
      | otherwise -> "\ESC"
    SpecialKey_F1
      | modKeys == [Alt] -> "\ESC[1;3P"
      | modKeys == [Ctrl] -> "\ESC[1;5P"
      | modKeys == [Shift] -> "\ESC[1;2P"
      | modKeys == [] -> "\ESCOP"
      | otherwise -> ""
    SpecialKey_F2
      | modKeys == [Alt] -> "\ESC[1;3Q"
      | modKeys == [Ctrl] -> "\ESC[1;5Q"
      | modKeys == [Shift] -> "\ESC[1;2Q"
      | modKeys == [] -> "\ESCOQ"
      | otherwise -> ""
    SpecialKey_F3
      | modKeys == [Alt] -> "\ESC[1;3R"
      | modKeys == [Ctrl] -> "\ESC[1;5R"
      | modKeys == [Shift] -> "\ESC[1;2R"
      | modKeys == [] -> "\ESCOR"
      | otherwise -> ""
    SpecialKey_F4
      | modKeys == [Alt] -> "\ESC[1;3S"
      | modKeys == [Ctrl] -> "\ESC[1;5S"
      | modKeys == [Shift] -> "\ESC[1;2S"
      | modKeys == [] -> "\ESCOS"
      | otherwise -> ""
    SpecialKey_F5
      | modKeys == [Alt] -> "\ESC[15;3~"
      | modKeys == [Ctrl] -> "\ESC[15;5~"
      | modKeys == [Shift] -> "\ESC[15;2~"
      | modKeys == [] -> "\ESC[15~"
      | otherwise -> ""
    SpecialKey_F6
      | modKeys == [Alt] -> "\ESC[17;3~"
      | modKeys == [Ctrl] -> "\ESC[17;5~"
      | modKeys == [Shift] -> "\ESC[17;2~"
      | modKeys == [] -> "\ESC[17~"
      | otherwise -> ""
    SpecialKey_F7
      | modKeys == [Alt] -> "\ESC[18;3~"
      | modKeys == [Ctrl] -> "\ESC[18;5~"
      | modKeys == [Shift] -> "\ESC[18;2~"
      | modKeys == [] -> "\ESC[18~"
      | otherwise -> ""
    SpecialKey_F8
      | modKeys == [Alt] -> "\ESC[19;3~"
      | modKeys == [Ctrl] -> "\ESC[19;5~"
      | modKeys == [Shift] -> "\ESC[19;2~"
      | modKeys == [] -> "\ESC[19~"
      | otherwise -> ""
    SpecialKey_F9
      | modKeys == [Alt] -> "\ESC[20;3~"
      | modKeys == [Ctrl] -> "\ESC[20;5~"
      | modKeys == [Shift] -> "\ESC[20;2~"
      | modKeys == [] -> "\ESC[20~"
      | otherwise -> ""
    SpecialKey_F10
      | modKeys == [Alt] -> "\ESC[21;3~"
      | modKeys == [Ctrl] -> "\ESC[21;5~"
      | modKeys == [Shift] -> "\ESC[21;2~"
      | modKeys == [] -> "\ESC[21~"
      | otherwise -> ""
    SpecialKey_F11
      | modKeys == [Alt] -> "\ESC[23;3~"
      | modKeys == [Ctrl] -> "\ESC[23;5~"
      | modKeys == [Shift] -> "\ESC[23;2~"
      | modKeys == [] -> "\ESC[23~"
      | otherwise -> ""
    SpecialKey_F12
      | modKeys == [Alt] -> "\ESC[24;3~"
      | modKeys == [Ctrl] -> "\ESC[24;5~"
      | modKeys == [Shift] -> "\ESC[24;2~"
      | modKeys == [] -> "\ESC[24~"
      | otherwise -> ""
    SpecialKey_Insert
      | modKeys == [Shift] && keyboardState_DECPAM -> "\ESC[2;2~"
      | modKeys == [Shift] -> "\ESC[4l"
      | modKeys == [Ctrl] && keyboardState_DECPAM -> "\ESC[2;5~"
      | modKeys == [Ctrl] -> "\ESC[L"
      | keyboardState_DECPAM -> "\ESC[2~"
      | otherwise -> "\ESC[4h"
    SpecialKey_Delete
      | modKeys == [Shift] -> "\ESC[3;2~"
      | modKeys == [Ctrl] -> "\ESC[3;5~"
      | otherwise -> "\ESC[3~"
    SpecialKey_Home
      | modKeys == [Shift] && keyboardState_DECCKM -> "\ESC[1;2H"
      | modKeys == [Shift] -> "\ESC[2J"
      | keyboardState_DECCKM -> "\ESC[1~"
      | otherwise -> "\ESC[H"
    SpecialKey_End
      | modKeys == [Shift] && keyboardState_DECPAM -> "\ESC[1;2F"
      | modKeys == [Shift] -> "\ESC[K"
      | modKeys == [Ctrl] && keyboardState_DECPAM -> "\ESC[1;5F"
      | modKeys == [Ctrl] -> "\ESC[J"
      | otherwise -> "\ESC[4~"
    SpecialKey_PageUp
      | modKeys == [Shift] -> "\ESC[5;2~"
      | modKeys == [Ctrl] -> "\ESC[5;5~"
      | otherwise -> "\ESC[5~"
    SpecialKey_PageDown
      | modKeys == [Shift] -> "\ESC[6;2~"
      | modKeys == [Ctrl] -> "\ESC[6;5~"
      | otherwise -> "\ESC[6~"
    SpecialKey_Tab
      | modKeys == [Shift] -> "\ESC[Z"
      | modKeys == [Alt] -> "\ESC\t"
      | otherwise -> "\t"
    SpecialKey_Enter
      | modKeys == [Alt] -> "\ESC\r"
      | otherwise -> "\r"
    SpecialKey_Backspace
      | Alt `elem` modKeys && Shift `elem` modKeys -> "\ESC\b"
      | Alt `elem` modKeys && Ctrl `elem` modKeys -> "\ESC\b"
      | Alt `elem` modKeys -> "\ESC\DEL"
      | Shift `elem` modKeys -> "\b"
      | Ctrl `elem` modKeys -> "\b"
      | otherwise -> "\DEL"
    SpecialKey_ArrowLeft
      | modKeys == [Shift] -> "\ESC[1;2D"
      | modKeys == [Alt] -> "\ESC[1;3D"
      | modKeys == [Alt, Shift] -> "\ESC[1;4D"
      | modKeys == [Ctrl] -> "\ESC[1;5D"
      | modKeys == [Ctrl, Shift] -> "\ESC[1;6D"
      | modKeys == [Alt, Ctrl] -> "\ESC[1;7D"
      | modKeys == [Alt, Ctrl, Shift] -> "\ESC[1;8D"
      | keyboardState_DECCKM -> "\ESCOD"
      | otherwise -> "\ESC[D"
    SpecialKey_ArrowRight
      | modKeys == [Shift] -> "\ESC[1;2C"
      | modKeys == [Alt] -> "\ESC[1;3C"
      | modKeys == [Alt, Shift] -> "\ESC[1;4C"
      | modKeys == [Ctrl] -> "\ESC[1;5C"
      | modKeys == [Ctrl, Shift] -> "\ESC[1;6C"
      | modKeys == [Alt, Ctrl] -> "\ESC[1;7C"
      | modKeys == [Alt, Ctrl, Shift] -> "\ESC[1;8C"
      | keyboardState_DECCKM -> "\ESCOC"
      | otherwise -> "\ESC[C"
    SpecialKey_ArrowUp
      | modKeys == [Shift] -> "\ESC[1;2A"
      | modKeys == [Alt] -> "\ESC[1;3A"
      | modKeys == [Alt, Shift] -> "\ESC[1;4A"
      | modKeys == [Ctrl] -> "\ESC[1;5A"
      | modKeys == [Ctrl, Shift] -> "\ESC[1;6A"
      | modKeys == [Alt, Ctrl] -> "\ESC[1;7A"
      | modKeys == [Alt, Ctrl, Shift] -> "\ESC[1;8A"
      | keyboardState_DECCKM -> "\ESCOA"
      | otherwise -> "\ESC[A"
    SpecialKey_ArrowDown
      | modKeys == [Shift] -> "\ESC[1;2B"
      | modKeys == [Alt] -> "\ESC[1;3B"
      | modKeys == [Alt, Shift] -> "\ESC[1;4B"
      | modKeys == [Ctrl] -> "\ESC[1;5B"
      | modKeys == [Ctrl, Shift] -> "\ESC[1;6B"
      | modKeys == [Alt, Ctrl] -> "\ESC[1;7B"
      | modKeys == [Alt, Ctrl, Shift] -> "\ESC[1;8B"
      | keyboardState_DECCKM -> "\ESCOB"
      | otherwise -> "\ESC[B"

charToByteString :: Char -> ByteString
charToByteString =
  -- TODO Encode as UTF-8
  B.singleton . fromIntegral . fromEnum
