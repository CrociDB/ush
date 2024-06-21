{-# LANGUAGE OverloadedStrings #-}

module Request (
    Request (..),
    RequestType (..),
    createRequest,
) where

import ContentType
import Encoding

import Data.List.Split
import qualified Data.Maybe as Data

data RequestType = GET | POST
    deriving (Show, Eq)

data Request = Request
    { requestType :: RequestType
    , url :: String
    , host :: String
    , userAgent :: String
    , headerAccept :: String
    , acceptEncoding :: EncodingType
    , contentType :: ContentType
    , contentSize :: Integer
    , contentText :: String
    }
    deriving (Show)

extractString :: Maybe String -> String
extractString Nothing = ""
extractString (Just str) = str

getHeaderLineValue :: String -> String -> Maybe String
getHeaderLineValue key strline = if head keyvalue == key then Just (head $ tail keyvalue) else Nothing
  where
    keyvalue = splitOn ": " strline

getHeaderValue :: String -> String -> Maybe String
getHeaderValue key str = if null filtered then Nothing else head filtered
  where
    headerlines = splitOn "\r\n" str
    values = map (getHeaderLineValue key) headerlines
    filtered = filter Data.isJust values

getRequestBody :: String -> String
getRequestBody rstr = requestBody
  where
    requestComponents = splitOn "\r\n\r\n" rstr
    requestBody = if length requestComponents > 1 then requestComponents !! 1 else ""

createRequest :: String -> Request
createRequest rstr =
    Request
        { requestType = rtype
        , url = rurl
        , host = rhost
        , userAgent = ruserAgent
        , headerAccept = raccept
        , acceptEncoding = racceptEncoding
        , contentType = rcontentType
        , contentSize = rcontentSize
        , contentText = rcontent
        }
  where
    slines = splitOn "\r\n" rstr
    strtype = head $ splitOn " " $ head slines
    rtype = case strtype of
        "GET" -> GET
        _ -> POST
    rurl = head $ tail $ splitOn " " $ head slines
    rhost = extractString $ getHeaderValue "Host" rstr
    ruserAgent = extractString $ getHeaderValue "User-Agent" rstr
    raccept = extractString $ getHeaderValue "Accept" rstr
    racceptEncoding = stringToEncoding $ extractString $ getHeaderValue "Content-Encoding" rstr
    rcontentSize = read (extractString $ getHeaderValue "Content-Size" rstr) :: Integer
    rcontentType = stringToContentType $ extractString $ getHeaderValue "Content-Type" rstr
    rcontent = getRequestBody rstr
