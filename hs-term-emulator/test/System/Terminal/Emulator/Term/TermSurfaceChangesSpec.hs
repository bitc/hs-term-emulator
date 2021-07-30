{-# LANGUAGE ScopedTypeVariables #-}

module System.Terminal.Emulator.Term.TermSurfaceChangesSpec where

import Data.Foldable (foldl')
import System.Terminal.Emulator.Parsing.Types (TermAtom (..))
import System.Terminal.Emulator.Term (Term, mkTerm)
import System.Terminal.Emulator.Term.ArbitraryTermAtom (arbitraryTermAtom)
import System.Terminal.Emulator.Term.Process (processTermAtoms)
import System.Terminal.Emulator.TermSurface.ApplyTermSurfaceChange (applySurfaceChange)
import System.Terminal.Emulator.TermSurface.FromTerm (termSurfaceFromTerm)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import Test.QuickCheck

newtype TestTermAtom = TestTermAtom {unTestTermAtom :: TermAtom}
  deriving (Show)

instance Arbitrary TestTermAtom where
  arbitrary = TestTermAtom <$> arbitraryTermAtom

blankTerm :: Term
blankTerm = mkTerm (10, 4)

spec :: Spec
spec = do
  describe "TestSurfaceChanges" $ do
    modifyMaxSuccess (const 10000) $
      modifyMaxSize (const 4000) $
        it "TermSurface Property Test" $
          property $
            \(atoms :: [TestTermAtom]) ->
              let initialTerm = blankTerm
                  initialTermSurface = termSurfaceFromTerm initialTerm
                  (_, surfaceChanges, term') = processTermAtoms (map unTestTermAtom atoms) initialTerm
                  expected = termSurfaceFromTerm term'
                  termSurface' = foldl' applySurfaceChange initialTermSurface surfaceChanges
               in termSurface' === expected
