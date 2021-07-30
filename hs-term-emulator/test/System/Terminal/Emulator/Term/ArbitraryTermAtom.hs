{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.Term.ArbitraryTermAtom where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Console.ANSI.Types (SGR)
import qualified System.Console.ANSI.Types as SGR
import System.Terminal.Emulator.Parsing.Types (ControlSequenceIntroducer (..), EscapeSequence (..), OperatingSystemCommand (..), TermAtom (..))
import Test.QuickCheck

arbitraryVisibleChar, arbitrarySingleCharacterFunction, arbitraryEscapeSequence :: Gen TermAtom
arbitraryVisibleChar = TermAtom_VisibleChar <$> arbitraryPrintableChar
arbitrarySingleCharacterFunction = TermAtom_SingleCharacterFunction <$> arbitraryBoundedEnum
arbitraryEscapeSequence =
  TermAtom_EscapeSequence
    <$> oneof
      [ pure Esc_ReverseIndex,
        pure Esc_RIS,
        pure Esc_DECPAM,
        pure (ESC_SetG0CharacterSet "%6"),
        (Esc_CSI <$> arbitraryCSI),
        (Esc_OSC <$> arbitraryOSI)
      ]

chooseNat :: Gen Int
chooseNat = chooseInt (1, 30)

arbitraryCSI :: Gen ControlSequenceIntroducer
arbitraryCSI =
  oneof
    [ CSI_CharacterPositionAbsolute <$> chooseNat,
      CSI_CharacterPositionRelative <$> chooseNat,
      CSI_CursorUp <$> chooseNat,
      CSI_CursorDown <$> chooseNat,
      CSI_CursorForward <$> chooseNat,
      CSI_CursorBack <$> chooseNat,
      CSI_EraseInLine <$> arbitraryBoundedEnum,
      CSI_InsertBlankCharacters <$> chooseNat,
      CSI_InsertBlankLines <$> chooseNat,
      CSI_DeleteChars <$> chooseNat,
      CSI_DeleteLines <$> chooseNat,
      CSI_CursorCharacterAbsolute <$> chooseNat,
      CSI_CursorPosition <$> chooseNat <*> chooseNat,
      CSI_HorizontalVerticalPosition <$> chooseNat <*> chooseNat,
      CSI_LinePositionAbsolute <$> chooseNat,
      CSI_LinePositionRelative <$> chooseNat,
      CSI_ScrollUp <$> chooseNat,
      CSI_ScrollDown <$> chooseNat,
      CSI_EraseInDisplay <$> arbitraryBoundedEnum,
      CSI_EraseCharacters <$> chooseNat,
      CSI_WindowManipulation <$> arbitraryBoundedEnum,
      CSI_DeviceStatusReport <$> arbitraryBoundedEnum,
      pure CSI_SoftTerminalReset,
      CSI_SetMode <$> arbitraryBoundedEnum,
      CSI_ResetMode <$> arbitraryBoundedEnum,
      pure $ CSI_SendDeviceAttributes,
      CSI_SendDeviceAttributesSecondary <$> arbitraryBoundedEnum,
      CSI_RequestDECPrivateMode <$> arbitrary {- TODO ??? -},
      CSI_DECSTBM <$> liftArbitrary chooseNat <*> liftArbitrary chooseNat,
      CSI_DECSET <$> arbitraryBoundedEnum,
      CSI_DECRST <$> arbitraryBoundedEnum,
      CSI_SGR <$> (V.fromList <$> (listOf arbitrarySGR))
    ]

arbitrarySGR :: Gen SGR
arbitrarySGR =
  oneof
    [ pure SGR.Reset,
      SGR.SetConsoleIntensity <$> elements [SGR.BoldIntensity, SGR.FaintIntensity, SGR.NormalIntensity],
      SGR.SetUnderlining <$> elements [SGR.SingleUnderline, SGR.DoubleUnderline, SGR.NoUnderline],
      SGR.SetDefaultColor <$> arbConsoleLayer,
      SGR.SetColor <$> arbConsoleLayer <*> elements [SGR.Dull, SGR.Vivid] <*> arbitraryBoundedEnum
    ]
  where
    arbConsoleLayer = elements [SGR.Foreground, SGR.Background]

arbitraryOSI :: Gen OperatingSystemCommand
arbitraryOSI =
  oneof
    [ OSC_SetTitle <$> arbitrary <*> arbitrary <*> arbitraryPrintableText,
      OSC_ChangeTextForegroundColor <$> arbitraryPrintableText,
      pure $ OSC_RequestTextForegroundColor,
      OSC_ChangeTextBackgroundColor <$> arbitraryPrintableText,
      pure $ OSC_RequestTextBackgroundColor,
      pure $ OSC_ResetTextCursorColor
    ]

arbitraryPrintableText :: Gen Text
arbitraryPrintableText = T.pack <$> (listOf arbitraryPrintableChar)

arbitraryTermAtom :: Gen TermAtom
arbitraryTermAtom =
  oneof
    [ arbitraryVisibleChar,
      arbitrarySingleCharacterFunction,
      arbitraryEscapeSequence
    ]
