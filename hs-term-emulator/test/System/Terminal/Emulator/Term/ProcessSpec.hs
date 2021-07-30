{-# LANGUAGE OverloadedStrings #-}

module System.Terminal.Emulator.Term.ProcessSpec where

import Control.Applicative (many)
import Data.Attoparsec.Text
import Data.Text (Text)
import System.Terminal.Emulator.Parsing (parseTermAtom)
import System.Terminal.Emulator.Term (Term, mkTerm)
import System.Terminal.Emulator.Term.Process (processTermAtoms)
import System.Terminal.Emulator.Term.SimpleTerm (SimpleTerm (..), termToSimpleTerm)
import Test.Hspec

blankTerm :: Term
blankTerm = mkTerm (10, 4)

runTerminal :: Term -> Text -> Term
runTerminal term input =
  case parseOnly (many parseTermAtom <* endOfInput) input of
    Left err -> error $ "Error parsing terminal input: " <> err
    Right ts ->
      let (_, term') = processTermAtoms ts term
       in term'

testCase :: (Term, Text) -> SimpleTerm -> Expectation
testCase (initialTerm, input) expected =
  let term' = runTerminal initialTerm input
      actual = termToSimpleTerm term'
   in actual `shouldBe` expected

spec :: Spec
spec = do
  describe "ProcessSpec" $ do
    it "No Input" $
      testCase
        (blankTerm, "")
        SimpleTerm
          { st_ScrollBackLines = [],
            st_Screen =
              [ "          ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (0, 0)
          }

    it "Single char" $
      testCase
        (blankTerm, "A")
        SimpleTerm
          { st_ScrollBackLines = [],
            st_Screen =
              [ "A         ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (0, 1)
          }

    it "Multiple chars" $
      testCase
        (blankTerm, "AB")
        SimpleTerm
          { st_ScrollBackLines = [],
            st_Screen =
              [ "AB        ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (0, 2)
          }

    it "Newlines" $
      testCase
        (blankTerm, "A\n\n\n")
        SimpleTerm
          { st_ScrollBackLines = [],
            st_Screen =
              [ "A         ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (3, 1)
          }

    it "Scroll up" $
      testCase
        (blankTerm, "A\n\n\n\n")
        SimpleTerm
          { st_ScrollBackLines =
              [ "A         "
              ],
            st_Screen =
              [ "          ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (3, 1)
          }

    it "Scroll up 2" $
      testCase
        (blankTerm, "A\nB\n\n\n\n")
        SimpleTerm
          { st_ScrollBackLines =
              [ "A         ",
                " B        "
              ],
            st_Screen =
              [ "          ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (3, 2)
          }

    it "Cursor Down 1" $
      testCase
        (blankTerm, "A\ESC[B")
        SimpleTerm
          { st_ScrollBackLines = [],
            st_Screen =
              [ "A         ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (1, 1)
          }

    it "Cursor Down past bottom" $
      testCase
        (blankTerm, "A\ESC[B\ESC[B\ESC[B\ESC[B")
        SimpleTerm
          { st_ScrollBackLines = [],
            st_Screen =
              [ "A         ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (3, 1)
          }

    -- TODO modern terminals don't add to the scrollback
    it "Scroll up 2" $
      testCase
        (blankTerm, "A\ESC[2S")
        SimpleTerm
          { st_ScrollBackLines =
              [ "A         ",
                "          "
              ],
            st_Screen =
              [ "          ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (0, 1)
          }

    -- TODO modern terminals don't add to the scrollback
    it "Delete lines 2" $
      testCase
        (blankTerm, "A\ESC[2M")
        SimpleTerm
          { st_ScrollBackLines =
              [ "A         ",
                "          "
              ],
            st_Screen =
              [ "          ",
                "          ",
                "          ",
                "          "
              ],
            st_CursorPos = (0, 1)
          }
