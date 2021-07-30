{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Terminal.Emulator.SDL.LibMain where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Lens
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import Data.Conduit.Combinators (decodeUtf8Lenient)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.TQueue (sinkTBQueue, sourceTBQueue)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear (V2 (..), V3 (..), V4 (..))
import Linear.Affine (Point (..))
import qualified Linear.V4 as V4
import SDL (get, ($=))
import qualified SDL as SDL
import qualified System.Console.ANSI.Types as SGR
import System.Terminal.Emulator.Attrs (Cell, attrsBg, attrsFg, attrsIntensity, attrsUnderline)
import System.Terminal.Emulator.KeyboardInput.KeyPressToPty (keyPressToPty)
import System.Terminal.Emulator.Parsing (parseTermAtom)
import System.Terminal.Emulator.Parsing.Types (TermAtom)
import System.Terminal.Emulator.SDL.ImageFont (loadImageFont)
import System.Terminal.Emulator.SDL.KeyboardTranslate (translateSDLKey)
import System.Terminal.Emulator.SDL.Pty (launchPty)
import System.Terminal.Emulator.Term (activeScreen, altScreenActive, cursorPos, mkTerm, numRows, scrollBackLines, termGetKeyboardState, windowTitle)
import System.Terminal.Emulator.Term.Process (Term, TermLine, processTermAtoms)
import System.Terminal.Emulator.Term.Resize (resizeTerm)
import qualified System.Terminal.Emulator.TermLines as TL
import Prelude hiding (lines)

initialTerminalWidth, initialTerminalHeight :: CInt
initialTerminalWidth = 80
initialTerminalHeight = 30

initialWindowSize :: V2 CInt
initialWindowSize = V2 (initialTerminalWidth * cellWidth) (initialTerminalHeight * cellHeight)

main :: IO ()
main = do
  SDL.initializeAll
  bracket
    ( SDL.createWindow
        "My SDL Application"
        SDL.defaultWindow
          { SDL.windowBorder = True,
            SDL.windowInitialSize = initialWindowSize,
            SDL.windowResizable = True
          }
    )
    SDL.destroyWindow
    ( \window -> do
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
        -- We need to resize the window again, because the window manager I
        -- use (xmonad) creates it initially a bit too small
        SDL.windowSize window $= initialWindowSize
        fontSurface <- loadImageFont
        fontTexture <- SDL.createTextureFromSurface renderer fontSurface
        SDL.freeSurface fontSurface

        terminalLoop window renderer fontTexture
    )

terminalLoop :: SDL.Window -> SDL.Renderer -> SDL.Texture -> IO ()
terminalLoop window renderer fontTexture = do
  V2 windowWidth windowHeight <- get $ SDL.windowSize window
  let terminalInitialWidth = fromIntegral (windowWidth `div` cellWidth)
      terminalInitialHeight = fromIntegral (windowHeight `div` cellHeight)

  termOutputQueue <- newTBQueueIO 500
  let initialTerm = mkTerm (terminalInitialWidth, terminalInitialHeight)

  windowDirtyVar <- newTVarIO True

  scrollBackVar <- newTVarIO 0

  termVar :: TVar Term <- newTVarIO initialTerm

  termInputBuffer :: TChan ByteString <- newTChanIO

  termResizeEvents :: TChan (Int, Int) <- newTChanIO

  termOutputStream :: TBQueue TermAtom <- newTBQueueIO 500

  let streamInput :: IO ()
      streamInput =
        runConduit $
          (sourceTBQueue termOutputQueue)
            .| decodeUtf8Lenient
            .| C.iterM (\t -> putStrLn $ "[RAW] " <> show t)
            .| conduitParser parseTermAtom -- termAtom
            -- .| C.iterM (\t -> putStrLn $ "[PARSED] " <> show t)
            .| C.map snd
            .| sinkTBQueue termOutputStream

  let doTerminalResize :: (Int, Int) -> IO ()
      doTerminalResize newSize = do
        atomically $ do
          modifyTVar' termVar (flip resizeTerm newSize')
          writeTVar windowDirtyVar True
        atomically $ writeTChan termResizeEvents newSize'
        where
          (width, height) = newSize
          newSize' = (max 1 width, max 1 height)

  let readInput :: IO ()
      readInput = do
        event <- interruptibleWaitEvent
        case SDL.eventPayload event of
          SDL.KeyboardEvent SDL.KeyboardEventData {SDL.keyboardEventKeyMotion = SDL.Pressed, SDL.keyboardEventKeysym = keysym} -> do
            -- putStrLn $ "{KEYSYM} " <> show keysym
            case translateSDLKey keysym of
              Nothing -> pure ()
              Just termInput -> atomically $ do
                term <- readTVar termVar
                writeTChan termInputBuffer (keyPressToPty (termGetKeyboardState term) termInput)
            pure ()
          SDL.WindowExposedEvent _ -> atomically $ writeTVar windowDirtyVar True
          SDL.MouseWheelEvent SDL.MouseWheelEventData {SDL.mouseWheelEventPos = V2 _ scrollY} -> do
            atomically $ do
              let scrollMultiplier = 6
              modifyTVar' scrollBackVar (\scrollBack -> max 0 (scrollBack + (scrollMultiplier * (fromIntegral scrollY))))
              writeTVar windowDirtyVar True
          SDL.MouseMotionEvent
            SDL.MouseMotionEventData
              { SDL.mouseMotionEventState = [SDL.ButtonRight],
                SDL.mouseMotionEventPos = P (V2 mouseX mouseY)
              } -> do
              let newTermWidth = fromIntegral mouseX `div` fromIntegral cellWidth
                  newTermHeight = fromIntegral mouseY `div` fromIntegral cellHeight
              doTerminalResize (newTermWidth, newTermHeight)
          SDL.MouseButtonEvent
            SDL.MouseButtonEventData
              { SDL.mouseButtonEventButton = SDL.ButtonRight,
                SDL.mouseButtonEventMotion = SDL.Pressed,
                SDL.mouseButtonEventPos = P (V2 mouseX mouseY),
                SDL.mouseButtonEventClicks = numClicks
              } -> do
              putStrLn $ "mouseClick: " <> show numClicks <> show (mouseX, mouseY)
              let newTermWidth = fromIntegral mouseX `div` fromIntegral cellWidth
                  newTermHeight = fromIntegral mouseY `div` fromIntegral cellHeight
              doTerminalResize (newTermWidth, newTermHeight)
          SDL.WindowSizeChangedEvent
            SDL.WindowSizeChangedEventData
              { SDL.windowSizeChangedEventSize = V2 newWindowWidth newWindowHeight
              } ->
              do
                putStrLn $ "RESIZE: " <> show (newWindowWidth, newWindowHeight)
                let newTermWidth = fromIntegral newWindowWidth `div` fromIntegral cellWidth
                    newTermHeight = fromIntegral newWindowHeight `div` fromIntegral cellHeight
                doTerminalResize (newTermWidth, newTermHeight)
          _ -> pure ()
        readInput

  let renderLoop :: IO ()
      renderLoop = do
        mbOutputBuf <-
          atomically $
            (readTBQueueAll termOutputStream >>= pure . Just)
              `orElse` ( do
                           windowDirty <- readTVar windowDirtyVar
                           if windowDirty
                             then do
                               writeTVar windowDirtyVar False
                               pure Nothing
                             else retry
                       )
        -- putStrLn $ "{OUTPUT} " <> show outputBuf
        !(termWrite, !term') <- atomically $ do
          term <- readTVar termVar
          case mbOutputBuf of
            Just outputBuf -> do
              let (termWrite, _, term') = processTermAtoms outputBuf term
              writeTVar termVar $! term'
              pure (termWrite, term')
            Nothing -> pure (B.empty, term)
        unless (B.null termWrite) $
          atomically $ writeTChan termInputBuffer termWrite
        scrollBack <- atomically $ readTVar scrollBackVar
        renderTerm window renderer fontTexture term' scrollBack
        renderLoop

  withAsync streamInput $ \streamInputAsync -> do
    link streamInputAsync
    withAsync readInput $ \readInputAsync -> do
      link readInputAsync
      withAsync
        ( renderLoop
        )
        $ \renderLoopAsync -> do
          link renderLoopAsync
          launchPty (terminalInitialWidth, terminalInitialHeight) (readTChan termInputBuffer) (readTChan termResizeEvents) $ \contents -> do
            -- putStrLn $ "[CONTENTS] " ++ show contents
            atomically $ writeTBQueue termOutputQueue contents

-- | Reads all the values in the 'TBQueue', or retry if it is empty
readTBQueueAll :: TBQueue a -> STM [a]
readTBQueueAll c = do
  first <- readTBQueue c
  rest <- tryReadTBQueueMultiple []
  pure (first : rest)
  where
    tryReadTBQueueMultiple accum = do
      mbItem <- tryReadTBQueue c
      case mbItem of
        Nothing -> pure (reverse accum)
        Just item -> tryReadTBQueueMultiple (item : accum)

renderTerm :: SDL.Window -> SDL.Renderer -> SDL.Texture -> Term -> Int -> IO ()
renderTerm window renderer fontTexture term scrollBack = do
  SDL.windowTitle window $= term ^. windowTitle
  let visibleRows
        | term ^. altScreenActive = term ^. activeScreen
        | otherwise = (TL.take (term ^. numRows) (TL.takeLast scrollBack (term ^. scrollBackLines))) <> (TL.dropLast scrollBack (term ^. activeScreen))

  SDL.rendererDrawColor renderer $= V4 255 32 255 255
  SDL.clear renderer
  _ <- (flip TL.traverseWithIndex) visibleRows $ \line termLine -> do
    renderLine renderer fontTexture termLine line

  renderGrid window renderer

  -- Render the cursor
  let (line, col) = term ^. cursorPos
  SDL.rendererDrawColor renderer $= V4 255 255 128 255
  SDL.drawRect renderer (Just (SDL.Rectangle (P (V2 (fromIntegral col * cellWidth) (fromIntegral (line + scrollBack) * cellHeight))) charSize))

  SDL.present renderer

renderGrid :: SDL.Window -> SDL.Renderer -> IO ()
renderGrid window renderer = do
  V2 windowWidth windowHeight <- get $ SDL.windowSize window
  SDL.rendererDrawColor renderer $= V4 8 8 8 255

  -- Horizontal lines
  forM_ [0, cellHeight .. windowHeight] $ \y -> do
    SDL.drawLine renderer (P (V2 0 y)) (P (V2 windowWidth y))

  -- Vertical lines
  forM_ [0, cellWidth .. windowWidth] $ \x -> do
    SDL.drawLine renderer (P (V2 x 0)) (P (V2 x windowHeight))

renderLine :: SDL.Renderer -> SDL.Texture -> TermLine -> Int -> IO ()
renderLine renderer fontTexture termLine line = do
  VU.iforM_ termLine $ \col cell -> do
    renderChar renderer fontTexture cell line col
  pure ()

renderChar :: SDL.Renderer -> SDL.Texture -> Cell -> Int -> Int -> IO ()
renderChar renderer fontTexture (char, attrs) line col = do
  let fgColor :: (V3 Word8)
      fgColor = case attrs ^. attrsFg of
        Nothing -> V3 172 216 172
        Just (SGR.Dull, SGR.Black) -> V3 0 0 0
        Just (SGR.Dull, SGR.Red) -> V3 208 0 0
        Just (SGR.Dull, SGR.Green) -> V3 0 208 0
        Just (SGR.Dull, SGR.Yellow) -> V3 208 208 0
        Just (SGR.Dull, SGR.Blue) -> V3 0 0 208
        Just (SGR.Dull, SGR.Magenta) -> V3 208 0 208
        Just (SGR.Dull, SGR.Cyan) -> V3 0 208 208
        Just (SGR.Dull, SGR.White) -> V3 208 208 208
        Just (SGR.Vivid, SGR.Black) -> V3 128 128 128
        Just (SGR.Vivid, SGR.Red) -> V3 128 128 128
        Just (SGR.Vivid, SGR.Green) -> V3 128 128 128
        Just (SGR.Vivid, SGR.Yellow) -> V3 128 128 128
        Just (SGR.Vivid, SGR.Blue) -> V3 128 128 128
        Just (SGR.Vivid, SGR.Magenta) -> V3 128 128 128
        Just (SGR.Vivid, SGR.Cyan) -> V3 128 128 128
        Just (SGR.Vivid, SGR.White) -> V3 128 128 128

      bgColor :: (V4 Word8)
      bgColor = case attrs ^. attrsBg of
        Nothing -> V4 0 0 0 255
        Just (SGR.Dull, SGR.Black) -> V4 0 0 0 255
        Just (SGR.Dull, SGR.Red) -> V4 208 0 0 255
        Just (SGR.Dull, SGR.Green) -> V4 0 208 0 255
        Just (SGR.Dull, SGR.Yellow) -> V4 208 208 0 255
        Just (SGR.Dull, SGR.Blue) -> V4 0 0 208 255
        Just (SGR.Dull, SGR.Magenta) -> V4 208 0 208 255
        Just (SGR.Dull, SGR.Cyan) -> V4 0 208 208 255
        Just (SGR.Dull, SGR.White) -> V4 208 208 208 255
        Just (SGR.Vivid, SGR.Black) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.Red) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.Green) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.Yellow) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.Blue) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.Magenta) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.Cyan) -> V4 255 128 128 255
        Just (SGR.Vivid, SGR.White) -> V4 255 128 128 255
  SDL.rendererDrawColor renderer $= bgColor
  SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (fromIntegral col * cellWidth) (fromIntegral line * cellHeight))) charSize))

  SDL.textureColorMod fontTexture $= fgColor
  SDL.copy
    renderer
    fontTexture
    (Just (SDL.Rectangle (charPosition char) charSize))
    (Just (SDL.Rectangle (P (V2 (fromIntegral col * cellWidth) (fromIntegral line * cellHeight))) charSize))

  when (attrs ^. attrsIntensity == SGR.BoldIntensity) $ do
    SDL.copy
      renderer
      fontTexture
      (Just (SDL.Rectangle (charPosition char) charSize))
      (Just (SDL.Rectangle (P (V2 (fromIntegral col * cellWidth - 1) (fromIntegral line * cellHeight))) charSize))

  when (attrs ^. attrsUnderline == SGR.SingleUnderline) $ do
    SDL.rendererDrawColor renderer $= (V4._w .~ 255) (V4.vector fgColor)
    SDL.drawLine
      renderer
      (P (V2 (fromIntegral col * cellWidth) (fromIntegral line * cellHeight + cellHeight - 2)))
      (P (V2 (fromIntegral col * cellWidth + cellWidth) (fromIntegral line * cellHeight + cellHeight - 2)))

fontCellWidth, fontCellHeight :: CInt
(fontCellWidth, fontCellHeight) = (16, 16)

cellWidth, cellHeight :: CInt
(cellWidth, cellHeight) = (14, 16)

charPosition :: Char -> Point V2 CInt
charPosition char = case fromEnum char of
  charCode
    | charCode < 256 -> P (V2 ((fromIntegral charCode `rem` 16) * fontCellWidth) ((fromIntegral charCode `div` 16) * fontCellHeight))
    | otherwise -> invalidCharP
  where
    invalidCharP = P (V2 (15 * fontCellWidth) (7 * fontCellHeight))

charSize :: V2 CInt
charSize = V2 fontCellWidth fontCellHeight

interruptibleWaitEvent :: IO SDL.Event
interruptibleWaitEvent = do
  result <- SDL.waitEventTimeout 50
  case result of
    Just e -> pure e
    Nothing -> interruptibleWaitEvent
