{-# LANGUAGE OverloadedStrings #-}

module Encoding where

import Data.List.Split
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Codec.Compression.GZip (compress)

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
    if Prelude.any (\s -> stringToEncoding s == Gzip) $ splitOn ", " str
        then Gzip
        else None

encodeString :: EncodingType -> BC.ByteString -> BC.ByteString
encodeString None str = str
encodeString Gzip str = LBS.toStrict $ compress $ LBS.fromStrict str
