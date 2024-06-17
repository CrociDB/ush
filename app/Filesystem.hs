{-# LANGUAGE OverloadedStrings #-}

module Filesystem (openFileAsString, writeStringToFile) where

import System.Directory
import System.FilePath ((</>))

openFileAsString :: String -> String -> IO (Maybe String)
openFileAsString path fileurl = do
    let filepath = path </> fileurl
    putStrLn $ "Loading file: " <> filepath

    exists <- doesFileExist filepath
    if not exists
        then do return Nothing
        else do
            contents <- readFile filepath
            return (Just contents)

writeStringToFile :: String -> String -> String -> IO ()
writeStringToFile path fileurl contents = do
    let filepath = path </> fileurl
    putStrLn $ "Saving file: " <> filepath
    writeFile filepath contents
