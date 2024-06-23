{-# LANGUAGE OverloadedStrings #-}

module Filesystem (
    LoadedFileData (..),
    resolveFile,
    writeStringToFile,
    getMimeType,
    fileFullPath,
) where

import qualified Data.ByteString.Char8 as BC

import AppData
import ContentType
import Logger

import System.Directory
import System.FilePath ((</>))

import qualified Data.Text as T

import Magic

data LoadedFileData = LoadedFileData BC.ByteString ContentType

fileFullPath :: String -> String -> String
fileFullPath path fileurl = path </> Prelude.drop 1 fileurl

applyItem :: String -> FilePath -> String
applyItem path file =
    T.unpack replaced
  where
    replaced = T.replace "%1" (T.pack (path </> file)) replacedTitle
    replacedTitle = T.replace "%2" (T.pack file) (T.pack indexPageItem)

getFilesDirectories :: String -> String -> IO String
getFilesDirectories dirpath fileurl = do
    names <- listDirectory dirpath
    let fullpaths = map (applyItem fileurl) names
    let lists = concat fullpaths
    return lists

getIndexPage :: String -> String -> IO BC.ByteString
getIndexPage dirpath fileurl = do
    let contents = T.replace "%1" (T.pack fileurl) $ T.replace "%3" (T.pack appHeader) (T.pack indexPage)
    filesanddirs <- getFilesDirectories dirpath fileurl
    let contentsWithFiles = T.unpack $ T.replace "%2" (T.pack filesanddirs) contents
    return $ BC.pack contentsWithFiles

serveFile :: FilePath -> IO BC.ByteString
serveFile filepath = do
    logMessage $ "Serving file: " ++ filepath
    BC.readFile filepath

resolveFile :: String -> String -> IO (Maybe LoadedFileData)
resolveFile path fileurl = do
    let filepath = fileFullPath path fileurl
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
                    filecontents <- getIndexPage filepath fileurl
                    return $ Just (LoadedFileData filecontents TextHTML)
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
