module System.Terminal.Emulator.SDL.KeyboardTranslate where

import Data.Char (isAlpha, toUpper)
import qualified SDL as SDL
import System.Terminal.Emulator.KeyboardInput (KeyModifiers (..), KeyPress (..), SpecialKey (..))

translateSDLKey :: SDL.Keysym -> Maybe KeyPress
translateSDLKey keysym
  | keycode `elem` [SDL.KeycodeReturn, SDL.KeycodeReturn2, SDL.KeycodeKPEnter] = Just $ KeyPress_SpecialKey SpecialKey_Enter modifiers
  | keycode `elem` [SDL.KeycodeBackspace] = Just $ KeyPress_SpecialKey SpecialKey_Backspace modifiers
  | keycode `elem` [SDL.KeycodeTab, SDL.KeycodeKPTab] = Just $ KeyPress_SpecialKey SpecialKey_Tab modifiers
  | keycode `elem` [SDL.KeycodeEscape] = Just $ KeyPress_SpecialKey SpecialKey_Escape modifiers
  | keycode `elem` [SDL.KeycodeUp, SDL.KeycodeKP8] = Just $ KeyPress_SpecialKey SpecialKey_ArrowUp modifiers
  | keycode `elem` [SDL.KeycodeDown, SDL.KeycodeKP2] = Just $ KeyPress_SpecialKey SpecialKey_ArrowDown modifiers
  | keycode `elem` [SDL.KeycodeLeft, SDL.KeycodeKP4] = Just $ KeyPress_SpecialKey SpecialKey_ArrowLeft modifiers
  | keycode `elem` [SDL.KeycodeRight, SDL.KeycodeKP6] = Just $ KeyPress_SpecialKey SpecialKey_ArrowRight modifiers
  | keycode `elem` [SDL.KeycodeInsert, SDL.KeycodeKP0] = Just $ KeyPress_SpecialKey SpecialKey_Insert modifiers
  | keycode `elem` [SDL.KeycodeDelete, SDL.KeycodeKPBackspace] = Just $ KeyPress_SpecialKey SpecialKey_Delete modifiers
  | keycode `elem` [SDL.KeycodeHome, SDL.KeycodeKP7] = Just $ KeyPress_SpecialKey SpecialKey_Home modifiers
  | keycode `elem` [SDL.KeycodeEnd, SDL.KeycodeKP1] = Just $ KeyPress_SpecialKey SpecialKey_End modifiers
  | keycode `elem` [SDL.KeycodePageUp, SDL.KeycodeKP9] = Just $ KeyPress_SpecialKey SpecialKey_PageUp modifiers
  | keycode `elem` [SDL.KeycodePageDown, SDL.KeycodeKP3] = Just $ KeyPress_SpecialKey SpecialKey_PageDown modifiers
  | keycode == SDL.KeycodeF1 = Just $ KeyPress_SpecialKey SpecialKey_F1 modifiers
  | keycode == SDL.KeycodeF2 = Just $ KeyPress_SpecialKey SpecialKey_F2 modifiers
  | keycode == SDL.KeycodeF3 = Just $ KeyPress_SpecialKey SpecialKey_F3 modifiers
  | keycode == SDL.KeycodeF4 = Just $ KeyPress_SpecialKey SpecialKey_F4 modifiers
  | keycode == SDL.KeycodeF5 = Just $ KeyPress_SpecialKey SpecialKey_F5 modifiers
  | keycode == SDL.KeycodeF6 = Just $ KeyPress_SpecialKey SpecialKey_F6 modifiers
  | keycode == SDL.KeycodeF7 = Just $ KeyPress_SpecialKey SpecialKey_F7 modifiers
  | keycode == SDL.KeycodeF8 = Just $ KeyPress_SpecialKey SpecialKey_F8 modifiers
  | keycode == SDL.KeycodeF9 = Just $ KeyPress_SpecialKey SpecialKey_F9 modifiers
  | keycode == SDL.KeycodeF10 = Just $ KeyPress_SpecialKey SpecialKey_F10 modifiers
  | keycode == SDL.KeycodeF11 = Just $ KeyPress_SpecialKey SpecialKey_F11 modifiers
  | keycode == SDL.KeycodeF12 = Just $ KeyPress_SpecialKey SpecialKey_F12 modifiers
  | keycodeNum >= 32 && keycodeNum <= 126 = Just $ KeyPress_Char (keySymToChar keysym) modifiers
  | otherwise = Nothing
  where
    keycode = SDL.keysymKeycode keysym
    SDL.Keycode keycodeNum = keycode
    modifiers = sdlKeyModifiers (SDL.keysymModifier keysym)

sdlKeyModifiers :: SDL.KeyModifier -> KeyModifiers
sdlKeyModifiers keyModifier =
  KeyModifiers
    { shift = SDL.keyModifierLeftShift keyModifier || SDL.keyModifierRightShift keyModifier,
      ctrl = SDL.keyModifierLeftCtrl keyModifier || SDL.keyModifierRightCtrl keyModifier,
      alt = SDL.keyModifierLeftAlt keyModifier || SDL.keyModifierRightAlt keyModifier,
      capsLock = SDL.keyModifierCapsLock keyModifier
    }

keySymToChar :: SDL.Keysym -> Char
keySymToChar keysym
  | isAlpha char =
    if shiftPressed /= SDL.keyModifierCapsLock keyModifier
      then toUpper char
      else char
  | otherwise = if shiftPressed then uppercaseNonAlpha char else char
  where
    keyModifier = SDL.keysymModifier keysym
    char = toEnum (fromIntegral keycode)
    SDL.Keycode keycode = SDL.keysymKeycode keysym
    shiftPressed = SDL.keyModifierLeftShift keyModifier || SDL.keyModifierRightShift keyModifier

uppercaseNonAlpha :: Char -> Char
uppercaseNonAlpha char | isAlpha char = toUpper char
uppercaseNonAlpha '`' = '~'
uppercaseNonAlpha '1' = '!'
uppercaseNonAlpha '2' = '@'
uppercaseNonAlpha '3' = '#'
uppercaseNonAlpha '4' = '$'
uppercaseNonAlpha '5' = '%'
uppercaseNonAlpha '6' = '^'
uppercaseNonAlpha '7' = '&'
uppercaseNonAlpha '8' = '*'
uppercaseNonAlpha '9' = '('
uppercaseNonAlpha '0' = ')'
uppercaseNonAlpha '-' = '_'
uppercaseNonAlpha '=' = '+'
uppercaseNonAlpha '[' = '{'
uppercaseNonAlpha ']' = '}'
uppercaseNonAlpha ';' = ':'
uppercaseNonAlpha '\'' = '"'
uppercaseNonAlpha '\\' = '|'
uppercaseNonAlpha ',' = '<'
uppercaseNonAlpha '.' = '>'
uppercaseNonAlpha '/' = '?'
uppercaseNonAlpha char = char
