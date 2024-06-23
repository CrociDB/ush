{-# LANGUAGE OverloadedStrings #-}

module Filesystem (
    LoadedFileData (..),
    resolveFile,
    writeStringToFile,
    getMimeType,
    fileFullPath,
) where

import qualified Data.ByteString.Char8 as BC

import ContentType
import Logger

import System.Directory
import System.FilePath ((</>))

import Magic

data LoadedFileData = LoadedFileData BC.ByteString ContentType

fileFullPath :: String -> String -> String
fileFullPath path fileurl = path </> drop 1 fileurl

serveFile :: FilePath -> IO BC.ByteString
serveFile filepath = do
    logMessage $ "Serving file: " ++ filepath
    BC.readFile filepath

resolveFile :: String -> IO (Maybe LoadedFileData)
resolveFile filepath = do
    dir <- doesDirectoryExist filepath
    if dir
        then do
            let indexfilepath = filepath </> "index.html"
            index_exists <- doesFileExist indexfilepath
            if index_exists
                then do
                    filecontents <- serveFile indexfilepath
                    return $ Just (LoadedFileData filecontents TextHTML)
                else do
                    logError $ "File does not exist: " ++ filepath
                    return Nothing
        else do
            exists <- doesFileExist filepath
            if not exists
                then do
                    logError $ "File does not exist: " ++ filepath
                    return Nothing
                else do
                    filecontents <- serveFile filepath
                    return $ Just (LoadedFileData filecontents TextHTML)

writeStringToFile :: String -> String -> IO ()
writeStringToFile filepath contents = do
    logMessage $ "Saving file: " ++ filepath
    writeFile filepath contents

getMimeType :: FilePath -> IO ContentType
getMimeType filepath = do
    magic <- magicOpen [MagicMimeType]
    magicLoadDefault magic

    mime <- magicFile magic filepath
    return $ stringToContentType mime
