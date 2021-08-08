module System.Terminal.Emulator.TermSurface.CollapseTermSurfaceChanges
  ( collapseTermSurfaceChanges,
  )
where

import Data.Foldable (fold, foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import System.Terminal.Emulator.TermSurface.TermSurfaceChange (TermSurfaceChange (..))

splitAtResizes :: Seq TermSurfaceChange -> (Seq (Seq TermSurfaceChange, TermSurfaceChange), Seq TermSurfaceChange)
splitAtResizes =
  foldl' go (mempty, mempty)
  where
    go ::
      (Seq (Seq TermSurfaceChange, TermSurfaceChange), Seq TermSurfaceChange) ->
      TermSurfaceChange ->
      (Seq (Seq TermSurfaceChange, TermSurfaceChange), Seq TermSurfaceChange)
    go (acc, current) change
      | isResize change = (acc |> (current, change), mempty)
      | otherwise = (acc, current |> change)

isResize :: TermSurfaceChange -> Bool
isResize (Resize _ _) = True
isResize _ = False

data Collapse = Collapse
  { setWindowTitle :: !(Maybe TermSurfaceChange),
    updateLines :: !(IntMap TermSurfaceChange),
    setScrollBackVisible :: !(Maybe TermSurfaceChange),
    moveCursor :: !(Maybe TermSurfaceChange)
  }

collapseTermSurfaceChanges :: Seq TermSurfaceChange -> Seq TermSurfaceChange
collapseTermSurfaceChanges termSurfaceChanges =
  let x :: Seq TermSurfaceChange
      x = fold $ (fmap (\(changes, resize) -> changes |> resize)) chunks
   in x <> collapseTermSurfaceChangesNoResizes rest
  where
    (chunks, rest) = splitAtResizes termSurfaceChanges

collapseTermSurfaceChangesNoResizes :: Seq TermSurfaceChange -> Seq TermSurfaceChange
collapseTermSurfaceChangesNoResizes termSurfaceChanges =
  let (collapsed, allScrollBackChanges) =
        foldl'
          ( \(collapse, scrollBackChanges) change -> case change of
              c@(SetWindowTitle _) -> (collapse {setWindowTitle = Just c}, scrollBackChanges)
              c@(UpdateLine line _) -> (collapse {updateLines = IntMap.insert (fromIntegral line) c (updateLines collapse)}, scrollBackChanges)
              c@ClearScrollBack -> (collapse, scrollBackChanges |> c)
              c@(SetScrollBackVisible _) -> (collapse {setScrollBackVisible = Just c}, scrollBackChanges)
              c@(ClearScrollBackStart _) -> (collapse, scrollBackChanges |> c)
              c@(ClearScrollBackEnd _) -> (collapse, scrollBackChanges |> c)
              c@(AppendScrollBack _) -> (collapse, scrollBackChanges |> c)
              c@(MoveCursor _ _) -> (collapse {moveCursor = Just c}, scrollBackChanges)
              Resize _ _ -> error "Invalid TermSurfaceChange.Resize"
          )
          ( Collapse
              { setWindowTitle = Nothing,
                updateLines = IntMap.empty,
                setScrollBackVisible = Nothing,
                moveCursor = Nothing
              },
            mempty
          )
          termSurfaceChanges
   in maybe Seq.empty Seq.singleton (setWindowTitle collapsed)
        <> Seq.fromList (IntMap.elems (updateLines collapsed))
        <> maybe Seq.empty Seq.singleton (setScrollBackVisible collapsed)
        <> maybe Seq.empty Seq.singleton (moveCursor collapsed)
        <> allScrollBackChanges
