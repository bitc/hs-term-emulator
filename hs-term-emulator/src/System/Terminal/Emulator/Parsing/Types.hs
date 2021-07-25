module System.Terminal.Emulator.Parsing.Types where

import Data.Text (Text)
import Data.Vector (Vector)
import System.Console.ANSI.Types (SGR)
import qualified System.Console.ANSI.Types as SGR
import System.Terminal.Emulator.DECPrivateMode (DECPrivateMode)

data TermAtom
  = TermAtom_VisibleChar !Char
  | TermAtom_SingleCharacterFunction !SingleCharacterFunction
  | TermAtom_SingleCharacterFunctionUnknown !Char
  | TermAtom_EscapeSequence !EscapeSequence
  | TermAtom_EscapeSequenceUnknown !Text
  deriving (Eq, Show)

data SingleCharacterFunction
  = -- | @BEL@ Bell (BEL  is Ctrl-G).
    Control_Bell
  | -- | @BS@ Backspace (BS  is Ctrl-H).
    Control_Backspace
  | -- | @CR@ Carriage Return (CR  is Ctrl-M).
    Control_CarriageReturn
  | -- | @ENQ@ Return Terminal Status (ENQ  is Ctrl-E).  Default response is an empty string
    Control_ReturnTerminalStatus
  | -- | @FF@ Form Feed or New Page (NP ).  (FF  is Ctrl-L).  FF  is treated the same as LF .
    Control_FormFeed
  | -- | @LF@ Line Feed or New Line (NL).  (LF  is Ctrl-J).
    Control_LineFeed
  | -- | @SI@ Switch to Standard Character Set (Ctrl-O is Shift In or LS0). This invokes the G0 character set (the default) as GL. VT200 and up implement LS0.
    Control_SwitchToStandardCharacterSet
  | -- | @SO@ Switch to Alternate Character Set (Ctrl-N is Shift Out or LS1).  This invokes the G1 character set as GL. VT200 and up implement LS1.
    Control_SwitchToAlternateCharacterSet
  | -- | @TAB@ Horizontal Tab (HTS  is Ctrl-I).
    Control_Tab
  | -- | @VT@ Vertical Tab (VT  is Ctrl-K).  This is treated the same as LF.
    Control_VerticalTab
  deriving (Eq, Show)

data EscapeSequence
  = -- | @ESC M@ Reverse Index (RI  is 0x8d).
    Esc_ReverseIndex
  | -- | @ESC c@ Reset terminal to initial state (RIS)
    Esc_RIS
  | -- | @ESC =@ Application Keypad (DECPAM)
    Esc_DECPAM
  | -- | @ESC >@ Set numeric keypad mode (DECPNM)
    Esc_DECPNM
  | -- | @ESC (@ Designate G0 Character Set, VT100, ISO 2022.
    ESC_SetG0CharacterSet !Text
  | Esc_CSI !ControlSequenceIntroducer
  | Esc_OSC !OperatingSystemCommand
  deriving (Eq, Show)

data ControlSequenceIntroducer
  = -- | @CSI Ps `@  Character Position Absolute  [column] (default = [row,1]) (HPA).
    CSI_CharacterPositionAbsolute !Int
  | -- | @CSI Ps a@ Character Position Relative  [columns] (default = [row,col+1]) (HPR).
    CSI_CharacterPositionRelative !Int
  | -- | @CSI Ps A@ Cursor Up Ps Times (default = 1) (CUU).
    CSI_CursorUp !Int
  | -- | @CSI Ps B@ Cursor Down Ps Times (default = 1) (CUD).
    CSI_CursorDown !Int
  | -- | @CSI Ps C@ Cursor Forward Ps Times (default = 1) (CUF).
    CSI_CursorForward !Int
  | -- | @CSI Ps D@ Cursor Backward Ps Times (default = 1) (CUB).
    CSI_CursorBack !Int
  | -- | @CSI Ps K@ Erase in Line (EL), VT100.
    CSI_EraseInLine !EraseInLineParam
  | -- | @CSI Ps \@@ Insert Ps (Blank) Character(s) (default = 1) (ICH)
    CSI_InsertBlankCharacters !Int
  | -- | @CSI Ps L@ Insert Ps Line(s) (default = 1) (IL)
    CSI_InsertBlankLines !Int
  | -- | @CSI Ps P@ Delete Ps Character(s) (default = 1) (DCH).
    CSI_DeleteChars !Int
  | -- | @CSI Ps M@ Delete Ps Line(s) (default = 1) (DL).
    CSI_DeleteLines !Int
  | -- | @CSI Ps G@ Cursor Character Absolute  [column] (default = [row,1]) (CHA).
    CSI_CursorCharacterAbsolute !Int
  | -- | @CSI Ps ; Ps H@ Cursor Position [row;column] (default = [1,1]) (CUP).
    CSI_CursorPosition !Int !Int
  | -- | @CSI Ps ; Ps f@ Horizontal and Vertical Position [row;column] (default = [1,1]) (HVP).
    CSI_HorizontalVerticalPosition !Int !Int
  | -- | @CSI Ps d@ Line Position Absolute  [row] (default = [1,column]) (VPA).
    CSI_LinePositionAbsolute !Int
  | -- | @CSI Ps e@ Line Position Relative  [rows] (default = [row+1,column]) (VPR).
    CSI_LinePositionRelative !Int
  | -- | @CSI Ps S@ Scroll up Ps lines (default = 1) (SU), VT420, ECMA-48.
    CSI_ScrollUp !Int
  | -- | @CSI Ps T@ Scroll down Ps lines (default = 1) (SD), VT420.
    CSI_ScrollDown !Int
  | -- | @CSI Ps J@ Erase in Display (ED), VT100
    CSI_EraseInDisplay !EraseInDisplayParam
  | -- | @CSI Ps X@ Erase Ps Character(s) (default = 1) (ECH).
    CSI_EraseCharacters !Int
  | -- | @CSI Ps ; Ps ; Ps t@ Window manipulation (XTWINOPS), dtterm, extended by xterm. These controls may be disabled using the allowWindowOps resource.
    CSI_WindowManipulation !WindowManipulation
  | -- | @CSI Ps n@ Device Status Report (DSR).
    CSI_DeviceStatusReport !DeviceStatusReport
  | -- | @CSI ! p@ Soft terminal reset (DECSTR), VT220 and up.
    CSI_SoftTerminalReset
  | -- | @CSI Pm h@ Set Mode (SM).
    CSI_SetMode !Mode
  | -- | @CSI Pm l@ Reset Mode (RM).
    CSI_ResetMode !Mode
  | -- | @CSI Ps c@ Send Device Attributes (Primary DA).
    CSI_SendDeviceAttributes
  | -- | @CSI > Ps c@ Send Device Attributes (Secondary DA).
    CSI_SendDeviceAttributesSecondary !SendDeviceAttributesSecondary
  | -- | @CSI ? Ps $ p@ Request DEC private mode (DECRQM).
    CSI_RequestDECPrivateMode !Int
  | -- | Set Scrolling Region [top;bottom] (default = full size of window) (DECSTBM)
    CSI_DECSTBM !(Maybe Int) !(Maybe Int)
  | -- | DEC Private Mode Set
    CSI_DECSET !DECPrivateMode
  | -- | Unknown DECSET (DEC Private Mode Set) code
    CSI_DECSET_Unknown !Int
  | -- | DEC Private Mode Reset
    CSI_DECRST !DECPrivateMode
  | -- | Unknown DECRST (DEC Private Mode Reset) code
    CSI_DECRST_Unknown !Int
  | CSI_SGR !(Vector SGR)
  deriving (Eq, Show)

data EraseInLineParam
  = -- | @Ps = 0@ Erase to Right (default).
    ClearFromCursorToEndOfLine
  | -- | @Ps = 1@  Erase to Left.
    ClearFromCursorToBeginningOfLine
  | -- | @Ps = 2@  Erase All.
    ClearEntireLine
  deriving (Eq, Show)

data EraseInDisplayParam
  = -- | @Ps = 0@ Erase Below (default).
    EraseBelow
  | -- | @Ps = 1@ Erase Above.
    EraseAbove
  | -- | @Ps = 2@ Erase All.
    EraseAll
  | -- | @Ps = 3@ Erase Saved Lines, xterm.
    EraseSavedLines
  deriving (Eq, Show)

data WindowManipulation
  = -- | @22;0@ Save xterm icon and window title on stack.
    SaveIconAndWindowTitleOnStack
  | -- | @23;0@ Restore xterm icon and window title from stack.
    RestoreIconAndWindowTitleOnStack
  deriving (Eq, Show)

data DeviceStatusReport
  = -- | Status Report. Result ("OK") is @CSI 0 n@
    StatusReport
  | -- | Report Cursor Position (CPR) [row;column]. Result is @CSI r ; c R@
    ReportCursorPosition
  deriving (Eq, Show)

data Mode
  = -- | Keyboard Action Mode (KAM)
    KeyboardActionMode
  | -- | Insert/Replace Mode (IRM)
    InsertReplaceMode
  | -- | Send/receive (SRM)
    SendReceive
  | -- | Automatic Newline / Normal Linefeed (LNM).
    AutomaticNewlineNormalLinefeed
  deriving (Eq, Show)

data SendDeviceAttributesSecondary
  = RequestTerminalIdentificationCode
  deriving (Eq, Show)

data OperatingSystemCommand
  = -- | Change Icon Name and Window Title
    OSC_SetTitle
      !Bool
      -- ^ Set icon name to the string
      !Bool
      -- ^ Set window title to the string
      !Text
      -- ^ The string that should be used for the title
  | -- | Change VT100 text foreground color
    OSC_ChangeTextForegroundColor !Text
  | -- | Request VT100 text foreground color
    OSC_RequestTextForegroundColor
  | -- | Change VT100 text background color
    OSC_ChangeTextBackgroundColor !Text
  | -- | Request VT100 text background color
    OSC_RequestTextBackgroundColor
  | -- | @Ps = 112@ Reset text cursor color.
    OSC_ResetTextCursorColor
  deriving (Eq, Show)

codeToSGR :: Int -> Maybe SGR.SGR
codeToSGR 0 = Just SGR.Reset
codeToSGR 1 = Just $ SGR.SetConsoleIntensity SGR.BoldIntensity
codeToSGR 2 = Just $ SGR.SetConsoleIntensity SGR.FaintIntensity
codeToSGR 4 = Just $ SGR.SetUnderlining SGR.SingleUnderline
codeToSGR 21 = Just $ SGR.SetUnderlining SGR.DoubleUnderline
codeToSGR 22 = Just $ SGR.SetConsoleIntensity SGR.NormalIntensity
codeToSGR 24 = Just $ SGR.SetUnderlining SGR.NoUnderline
codeToSGR 39 = Just $ SGR.SetDefaultColor SGR.Foreground
codeToSGR 49 = Just $ SGR.SetDefaultColor SGR.Background
codeToSGR code
  | code `between` (30, 37) = do
    color <- codeToColor (code - 30)
    Just $ SGR.SetColor SGR.Foreground SGR.Dull color
  | code `between` (90, 97) = do
    color <- codeToColor (code - 90)
    Just $ SGR.SetColor SGR.Foreground SGR.Vivid color
  | code `between` (40, 47) = do
    color <- codeToColor (code - 40)
    Just $ SGR.SetColor SGR.Background SGR.Dull color
  | code `between` (100, 107) = do
    color <- codeToColor (code - 100)
    Just $ SGR.SetColor SGR.Background SGR.Vivid color
  | otherwise = Nothing

codeToColor :: Int -> Maybe SGR.Color
codeToColor 0 = Just SGR.Black
codeToColor 1 = Just SGR.Red
codeToColor 2 = Just SGR.Green
codeToColor 3 = Just SGR.Yellow
codeToColor 4 = Just SGR.Blue
codeToColor 5 = Just SGR.Magenta
codeToColor 6 = Just SGR.Cyan
codeToColor 7 = Just SGR.White
codeToColor _ = Nothing

between :: Ord a => a -> (a, a) -> Bool
between val (low, high) = val >= low && val <= high
