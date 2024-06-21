{-# LANGUAGE OverloadedStrings #-}

module ContentType (ContentType (..), contentTypeToString, stringToContentType) where

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
