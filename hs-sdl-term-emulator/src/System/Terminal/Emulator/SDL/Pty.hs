{-# LANGUAGE ScopedTypeVariables #-}

module System.Terminal.Emulator.SDL.Pty where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import System.Environment (getEnvironment)
import System.Posix.Pty
import System.Process

termEnvVarName :: String
termEnvVarName = "xterm"

launchPty :: (Int, Int) -> (STM ByteString) -> (STM (Int, Int)) -> (ByteString -> IO ()) -> IO ()
launchPty initialSize getInput getResize onOutput = do
  currentEnv <- getEnvironment

  let environ :: [(String, String)]
      environ =
        filter ((/= "TERM") . fst) currentEnv
          <> [("TERM", termEnvVarName)]

  bracket
    (spawnWithPty (Just environ) False "/bin/bash" [] initialSize)
    ( \(pty, processHandle) -> do
        closePty pty
        _ <- waitForProcess processHandle
        pure ()
    )
    ( \(pty, _) -> do
        let inputLoop :: IO ()
            inputLoop = do
              event <- atomically $ (getInput >>= pure . Left) `orElse` (getResize >>= pure . Right)
              case event of
                Left inputBuf -> writePty pty inputBuf
                Right newSize -> resizePty pty newSize
              inputLoop

        let readNext :: IO ()
            readNext = do
              result <- try $ do
                threadWaitReadPty pty
                readPty pty
              case result of
                Right output -> do
                  onOutput output
                  readNext
                Left (_ :: IOError) -> do
                  pure ()

        withAsync inputLoop $ \inputLoopAsync -> do
          link inputLoopAsync
          readNext
    )
