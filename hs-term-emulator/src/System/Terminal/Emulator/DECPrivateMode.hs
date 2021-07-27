module System.Terminal.Emulator.DECPrivateMode
  ( DECPrivateMode (..),
    intToDECPrivateMode,
  )
where

data DECPrivateMode
  = -- | @1@
    --
    -- @DECSET@: Application Cursor Keys (DECCKM)
    --
    -- @DECRST@: Normal Cursor Keys (DECCKM)
    DECCKM
  | -- | @2@
    --
    -- @DECSET@: Designate USASCII for character sets G0-G3 (DECANM), and set VT100 mode.
    --
    -- @DECRST@: Designate VT52 mode (DECANM).
    DECANM
  | -- | @3@
    --
    -- @DECSET@: 132 Column Mode (DECCOLM)
    --
    -- @DECRST@: 0 Column Mode (DECCOLM)
    DECCOLM
  | -- | @4@
    --
    -- @DECSET@: Smooth (Slow) Scroll (DECSCLM)
    --
    -- @DECRST@: Jump (Fast) Scroll (DECSCLM)
    DECSCLM
  | -- | @5@
    --
    -- @DECSET@: Reverse Video (DECSCNM)
    --
    -- @DECRST@: Normal Video (DECSCNM)
    DECSCNM
  | -- | @6@
    --
    -- @DECSET@: Origin Mode (DECOM)
    --
    -- @DECRST@: Normal Cursor Mode (DECOM)
    DECOM
  | -- | @7@
    --
    -- @DECSET@: Wraparound Mode (DECAWM)
    --
    -- @DECRST@: No Wraparound Mode (DECAWM)
    DECAWM
  | -- | @8@
    --
    -- @DECSET@: Auto-repeat Keys (DECARM)
    --
    -- @DECRST@: No Auto-repeat Keys (DECARM)
    DECARM
  | -- | @9@
    --
    -- @DECSET@: Send Mouse X & Y on button press. See the section Mouse Tracking.
    --
    -- @DECRST@: Don’t Send Mouse X & Y on button press
    X10MouseCompatibilityMode
  | -- | @12@
    --
    -- @DECSET@: Start Blinking Cursor (att610)
    --
    -- @DECRST@: Stop Blinking Cursor (att610)
    Att610
  | -- | @18@
    --
    -- @DECSET@: Print form feed (DECPFF)
    --
    -- @DECRST@: Don’t print form feed (DECPFF)
    DECPFF
  | -- | @19@
    --
    -- @DECSET@: Set print extent to full screen (DECPEX)
    --
    -- @DECRST@: Limit print to scrolling region (DECPEX)
    DECPEX
  | -- | @25@
    --
    -- @DECSET@: Show Cursor (DECTCEM)
    --
    -- @DECRST@: Hide Cursor (DECTCEM)
    DECTCEM
  | -- | @42@
    --
    -- @DECSET@: Enable Nation Replacement Character sets (DECNRCM)
    --
    -- @DECRST@: Disable Nation Replacement Character sets (DECNRCM)
    DECNRCM
  | -- | @1000@
    --
    -- @DECSET@: Send Mouse X & Y on button press and release. See the section Mouse Tracking.
    --
    -- @DECRST@: Don’t Send Mouse X & Y on button press and release. See the section Mouse Tracking.
    ReportButtonPress
  | -- | @1001@
    --
    -- @DECSET@: Use Hilite Mouse Tracking
    --
    -- @DECRST@: Don’t Use Hilite Mouse Tracking
    MouseHighlightMode
  | -- | @1002@
    --
    -- @DECSET@: Use Cell Motion Mouse Tracking.
    --
    -- @DECRST@: Don’t Use Cell Motion Mouse Tracking
    ReportMotionOnButtonPress
  | -- | @1003@
    --
    -- @DECSET@: Use All Motion Mouse Tracking.
    --
    -- @DECRST@: Don’t Use All Motion Mouse Tracking
    EnableAllMouseMotions
  | -- | @47@ / @1047@
    --
    -- @DECSET@: Use Alternate Screen Buffer (unless disabled by the titeInhibit resource)
    --
    -- @DECRST@: Use Normal Screen Buffer, clearing screen first if in the Alternate Screen (unless disabled by the titeInhibit resource)
    UseAlternateScreenBuffer
  | -- | @1048@
    --
    -- @DECSET@: Save cursor as in DECSC (unless disabled by the titeInhibit resource)
    --
    -- @DECRST@: Restore cursor as in DECRC (unless disabled by the titeInhibit resource)
    SaveCursorAsInDECSC
  | -- | @1049@
    --
    -- @DECSET@: Save cursor as in DECSC and use Alternate Screen Buffer, clearing it first (unless disabled by the titeInhibit resource). This combines the effects of the 1047 and 1048 modes. Use this with terminfo-based applications rather than the 47 mode.
    --
    -- @DECRST@: Use Normal Screen Buffer and restore cursor as in DECRC (unless disabled by the titeInhibit resource). This combines the effects of the 1047 and 1048 modes. Use this with terminfo-based applications rather than the 47 mode.
    SaveCursorAsInDECSCAndUseAlternateScreenBuffer
  | -- | @2004@
    --
    -- @DECSET@: Set bracketed paste mode.
    --
    -- @DECRST@: Reset bracketed paste mode.
    BracketedPasteMode
  deriving (Show, Eq, Ord, Enum, Bounded)

intToDECPrivateMode :: Int -> Maybe DECPrivateMode
intToDECPrivateMode 1 = Just DECCKM
intToDECPrivateMode 2 = Just DECANM
intToDECPrivateMode 3 = Just DECCOLM
intToDECPrivateMode 4 = Just DECSCLM
intToDECPrivateMode 5 = Just DECSCNM
intToDECPrivateMode 6 = Just DECOM
intToDECPrivateMode 7 = Just DECAWM
intToDECPrivateMode 8 = Just DECARM
intToDECPrivateMode 9 = Just X10MouseCompatibilityMode
intToDECPrivateMode 12 = Just Att610
intToDECPrivateMode 18 = Just DECPFF
intToDECPrivateMode 19 = Just DECPEX
intToDECPrivateMode 25 = Just DECTCEM
intToDECPrivateMode 42 = Just DECNRCM
intToDECPrivateMode 1000 = Just ReportButtonPress
intToDECPrivateMode 1001 = Just MouseHighlightMode
intToDECPrivateMode 1002 = Just ReportMotionOnButtonPress
intToDECPrivateMode 1003 = Just EnableAllMouseMotions
intToDECPrivateMode 47 = Just UseAlternateScreenBuffer
intToDECPrivateMode 1047 = Just UseAlternateScreenBuffer
intToDECPrivateMode 1048 = Just SaveCursorAsInDECSC
intToDECPrivateMode 1049 = Just SaveCursorAsInDECSCAndUseAlternateScreenBuffer
intToDECPrivateMode 2004 = Just BracketedPasteMode
intToDECPrivateMode _ = Nothing
