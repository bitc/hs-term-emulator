{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.KeyboardInput.KeyPressToPty
  ( keyPressToPty,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isControl)
import System.Terminal.Emulator.KeyboardInput (KeyModifiers (..), KeyPress (..), KeyboardState (..), SpecialKey (..))

keyPressToPty :: KeyboardState -> KeyPress -> ByteString
keyPressToPty KeyboardState {keyboardState_Locked = True} _ = ""
keyPressToPty _ (KeyPress_Char c modifiers)
  | isControl c = error $ "Invalid Control Char for KeyPress: " ++ show c
  | otherwise = keyToPty c modifiers
keyPressToPty keyboardState (KeyPress_SpecialKey specialKey modifiers) = specialKeyToPty keyboardState specialKey modifiers

keyToPty :: Char -> KeyModifiers -> ByteString
keyToPty 'a' KeyModifiers {ctrl = True} = "\1"
keyToPty 'b' KeyModifiers {ctrl = True} = "\2"
keyToPty 'c' KeyModifiers {ctrl = True} = "\3"
keyToPty 'd' KeyModifiers {ctrl = True} = "\4"
keyToPty 'e' KeyModifiers {ctrl = True} = "\5"
keyToPty 'f' KeyModifiers {ctrl = True} = "\6"
keyToPty 'r' KeyModifiers {ctrl = True} = "\18"
keyToPty char _ = charToByteString char

specialKeyToPty :: KeyboardState -> SpecialKey -> KeyModifiers -> ByteString
specialKeyToPty keyboardState specialKey KeyModifiers {alt, ctrl, shift, capsLock} =
  case specialKey of
    SpecialKey_Escape
      | alt -> "\ESC\ESC"
      | otherwise -> "\ESC"
    SpecialKey_F1
      | alt -> "\ESC[1;3P"
      | ctrl -> "\ESC[1;5P"
      | shift -> "\ESC[1;2P"
      | otherwise -> "\ESCOP"
    SpecialKey_F2
      | alt -> "\ESC[1;3Q"
      | ctrl -> "\ESC[1;5Q"
      | shift -> "\ESC[1;2Q"
      | otherwise -> "\ESCOQ"
    SpecialKey_F3
      | alt -> "\ESC[1;3R"
      | ctrl -> "\ESC[1;5R"
      | shift -> "\ESC[1;2R"
      | otherwise -> "\ESCOR"
    SpecialKey_F4
      | alt -> "\ESC[1;3S"
      | ctrl -> "\ESC[1;5S"
      | shift -> "\ESC[1;2S"
      | otherwise -> "\ESCOS"
    SpecialKey_F5
      | alt -> "\ESC[15;3~"
      | ctrl -> "\ESC[15;5~"
      | shift -> "\ESC[15;2~"
      | otherwise -> "\ESC[15~"
    SpecialKey_F6
      | alt -> "\ESC[17;3~"
      | ctrl -> "\ESC[17;5~"
      | shift -> "\ESC[17;2~"
      | otherwise -> "\ESC[17~"
    SpecialKey_F7
      | alt -> "\ESC[18;3~"
      | ctrl -> "\ESC[18;5~"
      | shift -> "\ESC[18;2~"
      | otherwise -> "\ESC[18~"
    SpecialKey_F8
      | alt -> "\ESC[19;3~"
      | ctrl -> "\ESC[19;5~"
      | shift -> "\ESC[19;2~"
      | otherwise -> "\ESC[19~"
    SpecialKey_F9
      | alt -> "\ESC[20;3~"
      | ctrl -> "\ESC[20;5~"
      | shift -> "\ESC[20;2~"
      | otherwise -> "\ESC[20~"
    SpecialKey_F10
      | alt -> "\ESC[21;3~"
      | ctrl -> "\ESC[21;5~"
      | shift -> "\ESC[21;2~"
      | otherwise -> "\ESC[21~"
    SpecialKey_F11
      | alt -> "\ESC[23;3~"
      | ctrl -> "\ESC[23;5~"
      | shift -> "\ESC[23;2~"
      | otherwise -> "\ESC[23~"
    SpecialKey_F12
      | alt -> "\ESC[24;3~"
      | ctrl -> "\ESC[24;5~"
      | shift -> "\ESC[24;2~"
      | otherwise -> "\ESC[24~"
    SpecialKey_Insert -> error "TODO"
    SpecialKey_Delete -> "\ESC[3~"
    SpecialKey_Home -> error "TODO"
    SpecialKey_End -> error "TODO"
    SpecialKey_PageUp -> error "TODO"
    SpecialKey_PageDown -> error "TODO"
    SpecialKey_Tab -> "\t" -- TODO modifiers
    SpecialKey_Enter
      | alt && not shift && not ctrl && not capsLock -> "\ESC\r"
      | otherwise -> "\r"
    SpecialKey_Backspace
      | alt && shift -> "\ESC\b"
      | alt && ctrl -> "\ESC\b"
      | alt -> "\ESC\DEL"
      | shift -> "\b"
      | ctrl -> "\b"
      | capsLock -> "\b"
      | otherwise -> "\DEL"
    SpecialKey_ArrowLeft
      | keyboardState_DECCKM keyboardState -> "\ESCOD"
      | otherwise -> "\ESC[D"
    SpecialKey_ArrowRight
      | keyboardState_DECCKM keyboardState -> "\ESCOC"
      | otherwise -> "\ESC[C"
    SpecialKey_ArrowUp
      | keyboardState_DECCKM keyboardState -> "\ESCOA"
      | otherwise -> "\ESC[A"
    SpecialKey_ArrowDown
      | keyboardState_DECCKM keyboardState -> "\ESCOB"
      | otherwise -> "\ESC[B"

charToByteString :: Char -> ByteString
charToByteString =
  -- TODO Encode as UTF-8
  B.singleton . fromIntegral . fromEnum
