{-# LANGUAGE OverloadedStrings #-}

module Logger where

import qualified Data.ByteString.Char8 as BC
import Data.Time.Clock
import Data.Time.Format

getCurrentFormattedTime :: IO String
getCurrentFormattedTime = do
    currentTime <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%H:%M:%S:%q" currentTime
    return formattedTime

logMessage :: String -> IO ()
logMessage msg = do
  dt <- getCurrentFormattedTime
  BC.putStrLn $ "[" <> BC.pack dt <> "] " <> BC.pack msg

logError :: String -> IO ()
logError msg = do
  dt <- getCurrentFormattedTime
  BC.putStrLn $ "[" <> BC.pack dt <> "][ERROR] " <> BC.pack msg
