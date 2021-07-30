module System.Terminal.Emulator.Term.TermChange where

data TermChange
  = MoveCursor !Int !Int
  | X
