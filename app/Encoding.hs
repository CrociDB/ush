{-# LANGUAGE OverloadedStrings #-}

module Encoding where

data EncodingType = None | Gzip deriving (Show, Eq)

encodingToString :: EncodingType -> String
encodingToString None = ""
encodingToString Gzip = "gzip"

stringToEncoding :: String -> EncodingType
stringToEncoding "gzip" = Gzip
stringToEncoding "" = None
stringToEncoding _ = None
