{-# LANGUAGE OverloadedStrings #-}

module ContentType (ContentType (..), contentTypeToString, stringToContentType, filenameToContentType) where

import System.FilePath (takeExtension)

data ContentType
    = TextPlain
    | ApplicationOctetStream
    | TextHTML
    | TextJavascript
    | TextCSS
    | ImageJPEG
    | ImagePNG
    | AudioMPEG
    | AudioOgg
    | Other String
    | Undefined String
    deriving (Show)

contentTypeToString :: ContentType -> String
contentTypeToString TextPlain = "text/plain"
contentTypeToString ApplicationOctetStream = "application/octet-stream"
contentTypeToString TextHTML = "text/html"
contentTypeToString TextJavascript = "text/javascript"
contentTypeToString TextCSS = "text/css"
contentTypeToString ImageJPEG = "image/jpeg"
contentTypeToString ImagePNG = "image/png"
contentTypeToString AudioMPEG = "audio/mpeg"
contentTypeToString AudioOgg = "audio/ogg"
contentTypeToString (Other str) = str
contentTypeToString (Undefined _) = "text/plain"

stringToContentType :: String -> ContentType
stringToContentType "text/plain" = TextPlain
stringToContentType "application/octet-stream" = ApplicationOctetStream
stringToContentType "text/html" = TextHTML
stringToContentType "text/javascript" = TextJavascript
stringToContentType "text/css" = TextCSS
stringToContentType "image/jpeg" = ImageJPEG
stringToContentType "image/jpg" = ImageJPEG
stringToContentType "image/png" = ImagePNG
stringToContentType "audio/mpeg" = AudioMPEG
stringToContentType "audio/ogg" = AudioOgg
stringToContentType str = Other str

extensionToContentType :: String -> ContentType
extensionToContentType "txt"  = TextPlain
extensionToContentType "bin"  = ApplicationOctetStream
extensionToContentType "html" = TextHTML
extensionToContentType "js"   = TextJavascript
extensionToContentType "css"  = TextCSS
extensionToContentType "jpeg" = ImageJPEG
extensionToContentType "jpg"  = ImageJPEG
extensionToContentType "png"  = ImagePNG
extensionToContentType "mpeg" = AudioMPEG
extensionToContentType "ogg"  = AudioOgg
extensionToContentType ext    = Undefined ext

filenameToContentType :: String -> ContentType
filenameToContentType filename = extensionToContentType (drop 1 $ takeExtension filename)


