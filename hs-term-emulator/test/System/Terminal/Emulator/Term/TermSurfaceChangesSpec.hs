{-# LANGUAGE ScopedTypeVariables #-}

module System.Terminal.Emulator.Term.TermSurfaceChangesSpec where

import Control.Lens
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import System.Terminal.Emulator.Parsing.Types (TermAtom (..))
import System.Terminal.Emulator.Term (Term, mkTerm, numCols, numRows)
import System.Terminal.Emulator.Term.ArbitraryTermAtom (arbitraryTermAtom)
import System.Terminal.Emulator.Term.Process (processTermAtoms)
import System.Terminal.Emulator.Term.Resize (resizeTerm)
import System.Terminal.Emulator.TermSurface.ApplyTermSurfaceChange (applySurfaceChange)
import System.Terminal.Emulator.TermSurface.FromTerm (termSurfaceFromTerm)
import System.Terminal.Emulator.TermSurface.TermSurfaceChange (TermSurfaceChange)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import Test.QuickCheck

newtype TestTermAtom = TestTermAtom {unTestTermAtom :: TermAtom}
  deriving (Show)

instance Arbitrary TestTermAtom where
  arbitrary = TestTermAtom <$> arbitraryTermAtom

data TermResize = TermResize (Int, Int)
  deriving (Show)

data TermInput
  = TermInputAtoms [TermAtom]
  | TermInputResize TermResize
  deriving (Show)

arbitraryTermInput :: Int -> Gen TermResize -> Gen TermInput
arbitraryTermInput termAtomsWeight resizeGen =
  frequency
    [ (termAtomsWeight, TermInputAtoms <$> listOf arbitraryTermAtom),
      (1, TermInputResize <$> resizeGen)
    ]

resizeWidth :: Gen TermResize
resizeWidth = do
  width <- chooseInt (1, 44)
  pure $ TermResize (width, blankTerm ^. numRows)

resizeHeight :: Gen TermResize
resizeHeight = do
  height <- chooseInt (1, 44)
  pure $ TermResize (blankTerm ^. numCols, height)

resizeWidthHeight :: Gen TermResize
resizeWidthHeight = do
  width <- chooseInt (1, 44)
  height <- chooseInt (1, 44)
  pure $ TermResize (width, height)

blankTerm :: Term
blankTerm = mkTerm (10, 4)

spec :: Spec
spec = do
  describe "TestSurfaceChanges" $ do
    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 400) $
        it "TermSurface Property Test: Only TermAtoms" $
          property $
            \(atoms :: [TestTermAtom]) ->
              let initialTerm = blankTerm
                  initialTermSurface = termSurfaceFromTerm initialTerm
                  (_, surfaceChanges, term') = processTermAtoms (map unTestTermAtom atoms) initialTerm
                  expected = termSurfaceFromTerm term'
                  termSurface' = foldl' applySurfaceChange initialTermSurface surfaceChanges
               in termSurface' === expected

    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 400) $
        it "TermSurface Property Test: Only Resize (width)" $
          forAll (listOf resizeWidth) termSurfaceOnlyResizeProp

    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 400) $
        it "TermSurface Property Test: Only Resize (height)" $
          forAll (listOf resizeHeight) termSurfaceOnlyResizeProp

    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 400) $
        it "TermSurface Property Test: Only Resize" $
          forAll (listOf resizeWidthHeight) termSurfaceOnlyResizeProp

    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 100) $
        it "TermSurface Property Test: TermAtoms and Resize (width)" $
          forAll (listOf (arbitraryTermInput 5 resizeWidth)) $ termSurfaceTermAtomsAndResizeProp

    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 100) $
        it "TermSurface Property Test: TermAtoms and Resize (height)" $
          forAll (listOf (arbitraryTermInput 5 resizeHeight)) $ termSurfaceTermAtomsAndResizeProp

    modifyMaxSuccess (const 5000) $
      modifyMaxSize (const 100) $
        it "TermSurface Property Test: TermAtoms and Resize" $
          forAll (listOf (arbitraryTermInput 5 resizeWidthHeight)) $ termSurfaceTermAtomsAndResizeProp

termSurfaceOnlyResizeProp :: [TermResize] -> Property
termSurfaceOnlyResizeProp = \(resizes :: [TermResize]) ->
  let initialTerm = blankTerm
      initialTermSurface = termSurfaceFromTerm initialTerm
      (surfaceChanges, term') = foldl' (\(cs1, t) (TermResize s) -> let (cs2, t') = resizeTerm t s in (cs1 <> cs2, t')) (mempty, initialTerm) resizes
      expected = termSurfaceFromTerm term'
      termSurface' = foldl' applySurfaceChange initialTermSurface surfaceChanges
   in termSurface' === expected

termSurfaceTermAtomsAndResizeProp :: [TermInput] -> Property
termSurfaceTermAtomsAndResizeProp = \(inputs :: [TermInput]) ->
  let initialTerm = blankTerm
      initialTermSurface = termSurfaceFromTerm initialTerm
      (surfaceChanges, term') = foldl' (\(cs1, t) i -> let (cs2, t') = processTermInput t i in (cs1 <> cs2, t')) (mempty, initialTerm) inputs
      expected = termSurfaceFromTerm term'
      termSurface' = foldl' applySurfaceChange initialTermSurface surfaceChanges
   in termSurface' === expected

processTermInput :: Term -> TermInput -> (Seq TermSurfaceChange, Term)
processTermInput term (TermInputAtoms termAtoms) =
  let (_, scs, term') = processTermAtoms termAtoms term
   in (scs, term')
processTermInput term (TermInputResize (TermResize size)) = resizeTerm term size
