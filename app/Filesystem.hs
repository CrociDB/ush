{-# LANGUAGE OverloadedStrings #-}

module Filesystem (openFileAsString, writeStringToFile, getMimeType) where

import qualified Data.ByteString.Char8 as BC

import ContentType
import Logger

import System.Directory
import System.FilePath ((</>))

import Magic

openFileAsString :: String -> String -> IO (Maybe BC.ByteString)
openFileAsString path fileurl = do
    let filepath = path </> fileurl

    exists <- doesFileExist filepath
    if not exists
        then do 
          logError $ "File does not exist: " ++ filepath
          return Nothing
        else do
            logMessage $ "Serving file: " ++ filepath
            contents <- BC.readFile filepath
            return (Just contents)

writeStringToFile :: String -> String -> String -> IO ()
writeStringToFile path fileurl contents = do
    let filepath = path </> fileurl
    logMessage $ "Saving file: " ++ filepath
    writeFile filepath contents

getMimeType :: FilePath -> FilePath -> IO ContentType
getMimeType path fileurl = do
    let filepath = path </> fileurl

    magic <- magicOpen [MagicMimeType]
    magicLoadDefault magic

    mime <- magicFile magic filepath
    return $ stringToContentType  mime
