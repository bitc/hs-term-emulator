{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.Parsing.InternalSpec where

import Control.Applicative (many)
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified System.Console.ANSI.Types as SGR
import System.Terminal.Emulator.Parsing (parseTermAtom)
import System.Terminal.Emulator.Parsing.Internal (ControlSequenceIntroducerComponents (..), ControlSequenceIntroducerInput (..), parseControlSequenceIntroducer, parseControlSequenceIntroducerComponents)
import System.Terminal.Emulator.Parsing.Types (ControlSequenceIntroducer (..), EscapeSequence (..), Mode (..), OperatingSystemCommand (..), SingleCharacterFunction (..), TermAtom (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "CSI" $ do
    it "Test 1" $
      (parseControlSequenceIntroducer, "0;1m")
        `shouldParseTo` [ControlSequenceIntroducerInput "0;1m"]
    it "Test 2" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput "1A") `shouldBe` Just (ControlSequenceIntroducerComponents False [1] 'A')
    it "Test 3" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput "2A") `shouldBe` Just (ControlSequenceIntroducerComponents False [2] 'A')
    it "Test 4" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput "A") `shouldBe` Just (ControlSequenceIntroducerComponents False [0] 'A')
    it "Test 6" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput "1;2A") `shouldBe` Just (ControlSequenceIntroducerComponents False [1, 2] 'A')
    it "Test 7" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput ";A") `shouldBe` Just (ControlSequenceIntroducerComponents False [0, 0] 'A')
    it "Test 8" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput ";2A") `shouldBe` Just (ControlSequenceIntroducerComponents False [0, 2] 'A')
    it "Test 9" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput ";;2A") `shouldBe` Just (ControlSequenceIntroducerComponents False [0, 0, 2] 'A')
    it "Test 10" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput ";2;A") `shouldBe` Just (ControlSequenceIntroducerComponents False [0, 2, 0] 'A')
    it "Test 11" $
      parseControlSequenceIntroducerComponents (ControlSequenceIntroducerInput ";;A") `shouldBe` Just (ControlSequenceIntroducerComponents False [0, 0, 0] 'A')
  describe
    "TermParse Parser"
    $ do
      it "TermAtom_VisibleChar A" $
        (parseTermAtom, "A")
          `shouldParseTo` [TermAtom_VisibleChar 'A']
      it "TermAtom_VisibleChar AB" $
        (parseTermAtom, "AB")
          `shouldParseTo` [TermAtom_VisibleChar 'A', TermAtom_VisibleChar 'B']
      it "TermAtom_VisibleChar ' '" $
        (parseTermAtom, " ")
          `shouldParseTo` [TermAtom_VisibleChar ' ']
      it "Control_Backspace" $
        (parseTermAtom, "\b")
          `shouldParseTo` [TermAtom_SingleCharacterFunction Control_Backspace]
      it "Multi chars" $
        (parseTermAtom, "A\b B")
          `shouldParseTo` [ TermAtom_VisibleChar 'A',
                            TermAtom_SingleCharacterFunction Control_Backspace,
                            TermAtom_VisibleChar ' ',
                            TermAtom_VisibleChar 'B'
                          ]
      it "ESC_RIS" $
        (parseTermAtom, "\ESCc")
          `shouldParseTo` [TermAtom_EscapeSequence Esc_RIS]
      it "Multi chars with simple escape" $
        (parseTermAtom, "A\ESCcB\bC")
          `shouldParseTo` [ TermAtom_VisibleChar 'A',
                            TermAtom_EscapeSequence Esc_RIS,
                            TermAtom_VisibleChar 'B',
                            TermAtom_SingleCharacterFunction Control_Backspace,
                            TermAtom_VisibleChar 'C'
                          ]
      it "OSC set window title" $
        (parseTermAtom, "\ESC]0;Hello\a")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_OSC (OSC_SetTitle True True "Hello"))]
      it "OSC set window title ST" $
        (parseTermAtom, "\ESC]0;Hello\ESC\\")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_OSC (OSC_SetTitle True True "Hello"))]
      it "OSC mix" $
        (parseTermAtom, "A\ESC]0;Hello\a\aB")
          `shouldParseTo` [ TermAtom_VisibleChar 'A',
                            TermAtom_EscapeSequence (Esc_OSC (OSC_SetTitle True True "Hello")),
                            TermAtom_SingleCharacterFunction Control_Bell,
                            TermAtom_VisibleChar 'B'
                          ]
      it "CSI Cursor up default" $
        (parseTermAtom, "\ESC[A")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_CursorUp 1))]
      it "CSI Cursor up zero" $
        (parseTermAtom, "\ESC[0A")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_CursorUp 1))]
      it "CSI Cursor up default 1" $
        (parseTermAtom, "\ESC[1A")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_CursorUp 1))]
      it "CSI Cursor up default 2" $
        (parseTermAtom, "\ESC[2A")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_CursorUp 2))]
      it "SGR Set bold" $
        (parseTermAtom, "\ESC[1m")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_SGR [SGR.SetConsoleIntensity SGR.BoldIntensity]))]
      it "SGR Reset 1" $
        (parseTermAtom, "\ESC[0m")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_SGR [SGR.Reset]))]
      it "SGR Reset 1" $
        (parseTermAtom, "\ESC[m")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_SGR [SGR.Reset]))]
      it "DECSTR" $
        (parseTermAtom, "\ESC[!p")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI CSI_SoftTerminalReset)]
      it "RM 0" $
        (parseTermAtom, "\ESC[l")
          `shouldParseTo` [TermAtom_EscapeSequenceUnknown "\ESC[l"]
      it "RM 2" $
        (parseTermAtom, "\ESC[2l")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_ResetMode KeyboardActionMode))]
      it "RM 4" $
        (parseTermAtom, "\ESC[4l")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_ResetMode InsertReplaceMode))]
      it "RM 12" $
        (parseTermAtom, "\ESC[12l")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_ResetMode SendReceive))]
      it "RM 20" $
        (parseTermAtom, "\ESC[20l")
          `shouldParseTo` [TermAtom_EscapeSequence (Esc_CSI (CSI_ResetMode AutomaticNewlineNormalLinefeed))]

shouldParseTo :: (Show a, Eq a) => (Parser a, Text) -> [a] -> IO ()
shouldParseTo (parser, str) expected =
  case parseOnly (many parser <* endOfInput) str of
    Left err -> fail err
    Right ts -> ts `shouldBe` expected
