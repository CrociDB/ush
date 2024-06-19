{-# LANGUAGE OverloadedStrings #-}

module Encoding where

import Data.List.Split

data EncodingType = None | Gzip deriving (Show, Eq)

encodingToString :: EncodingType -> String
encodingToString None = ""
encodingToString Gzip = "gzip"

stringToEncoding :: String -> EncodingType
stringToEncoding "gzip" = Gzip
stringToEncoding "" = None
stringToEncoding _ = None

stringsToEncoding :: String -> EncodingType
stringsToEncoding str =
    if any (\s -> stringToEncoding s == Gzip) $ splitOn ", " str
        then Gzip
        else None
