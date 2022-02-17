module System.Terminal.Emulator.KeyboardInput
  ( KeyboardState (..),
    initialKeyboardState,
    KeyPress (..),
    KeyModifiers (..),
    SpecialKey (..),
  )
where

data KeyboardState = KeyboardState
  { keyboardState_DECPAM :: !Bool,
    keyboardState_DECCKM :: !Bool,
    -- | Set using Keyboard Action Mode (KAM)
    keyboardState_Locked :: !Bool,
    -- | Set using Automatic Newline / Normal Linefeed (LNM)
    keyboardState_CRLF :: !Bool
  }
  deriving (Show, Eq, Ord)

initialKeyboardState :: KeyboardState
initialKeyboardState =
  KeyboardState
    { keyboardState_DECPAM = False,
      keyboardState_DECCKM = False,
      keyboardState_Locked = False,
      keyboardState_CRLF = False
    }

data KeyPress
  = -- | The char must be a plain-old regular "visible" character (or ' ').
    -- Specifically, you should not put '\n' or '\b' (use 'SpecialKey' for
    -- that)
    KeyPress_Char !Char !KeyModifiers
  | -- | Used for a key press of a 'SpecialKey'. If a 'SpecialKey' doesn't
    -- exist (for example "Ctrl", or "CapsLock") then no 'KeyPress' event
    -- should be generated
    KeyPress_SpecialKey !SpecialKey !KeyModifiers
  deriving (Eq, Ord, Show)

data KeyModifiers = KeyModifiers
  { shift :: !Bool,
    ctrl :: !Bool,
    alt :: !Bool,
    capsLock :: !Bool
  }
  deriving (Eq, Ord, Show)

data SpecialKey
  = SpecialKey_Escape
  | SpecialKey_F1
  | SpecialKey_F2
  | SpecialKey_F3
  | SpecialKey_F4
  | SpecialKey_F5
  | SpecialKey_F6
  | SpecialKey_F7
  | SpecialKey_F8
  | SpecialKey_F9
  | SpecialKey_F10
  | SpecialKey_F11
  | SpecialKey_F12
  | SpecialKey_Insert
  | SpecialKey_Delete
  | SpecialKey_Home
  | SpecialKey_End
  | SpecialKey_PageUp
  | SpecialKey_PageDown
  | SpecialKey_Tab
  | SpecialKey_Enter
  | SpecialKey_Backspace
  | SpecialKey_ArrowLeft
  | SpecialKey_ArrowRight
  | SpecialKey_ArrowUp
  | SpecialKey_ArrowDown
  deriving (Eq, Ord, Show)
