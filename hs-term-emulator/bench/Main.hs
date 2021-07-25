module Main where

import Criterion.Main
import System.Terminal.Emulator.Parsing.Types
import System.Terminal.Emulator.Term (mkTerm)
import System.Terminal.Emulator.Term.Process (processTermAtoms)

lotsOfAAAAA :: Int -> [TermAtom]
lotsOfAAAAA numRows =
  concat
    ( replicate
        numRows
        ( ( replicate 30 (TermAtom_VisibleChar 'A')
              <> [ TermAtom_SingleCharacterFunction Control_CarriageReturn,
                   TermAtom_SingleCharacterFunction Control_LineFeed
                 ]
          )
        )
    )

main :: IO ()
main =
  defaultMain
    [ bgroup
        "term"
        [ bench "As 10" $ whnf (processTermAtoms (lotsOfAAAAA 10)) (mkTerm (40, 40)),
          bench "As 100" $ whnf (processTermAtoms (lotsOfAAAAA 100)) (mkTerm (40, 40)),
          bench "As 1000" $ whnf (processTermAtoms (lotsOfAAAAA 1000)) (mkTerm (40, 40)),
          bench "As 10000" $ whnf (processTermAtoms (lotsOfAAAAA 10000)) (mkTerm (40, 40))
        ]
    ]
