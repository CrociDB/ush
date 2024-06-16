{-# LANGUAGE OverloadedStrings #-}

module ContentType  where

data ContentType = TextPlain | ApplicationOctetStream deriving (Show)

contentTypeToString :: ContentType -> String
contentTypeToString TextPlain = "text/plain"
contentTypeToString ApplicationOctetStream = "application/octet-stream"

stringToContentType :: String -> ContentType
stringToContentType "text/plain" = TextPlain
stringToContentType "application/octet-stream" = ApplicationOctetStream
stringToContentType _ = TextPlain
