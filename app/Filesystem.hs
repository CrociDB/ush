{-# LANGUAGE OverloadedStrings #-}

module Filesystem (openFileAsString, readFile) where

import System.Directory
import System.FilePath ((</>))

openFileAsString :: String -> String -> IO (Maybe String)
openFileAsString path fileurl = do
    let filepath = path </> drop 1 fileurl
    putStrLn $ "Loading file: " <> filepath

    exists <- doesFileExist filepath
    if not exists
        then do return Nothing
        else do
            contents <- readFile filepath
            return (Just contents)
