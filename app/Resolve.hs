{-# LANGUAGE OverloadedStrings #-}

module Resolve (Response, resolveRequest, returnBody) where

import ContentType
import Encoding
import Filesystem
import Request

import qualified Data.ByteString.Char8 as BC

import Data.List (intercalate)
import Data.List.Split

data Response = OK BC.ByteString ContentType EncodingType | NotFound | Created

httpStatus :: Response -> BC.ByteString
httpStatus NotFound = "HTTP/1.1 404 Not Found\r\n"
httpStatus OK{} = "HTTP/1.1 200 OK\r\n"
httpStatus Created = "HTTP/1.1 201 Created\r\n"

getContentHeader :: Response -> BC.ByteString
getContentHeader NotFound = ""
getContentHeader Created = ""
getContentHeader (OK body ctype encoding) =
    "Content-Type: "
        <> BC.pack (contentTypeToString ctype)
        <> encoding_line
        <> "\r\nContent-Length: "
        <> BC.pack (show (BC.length body))
        <> "\r\n\r\n"
  where
    encoding_line = BC.pack $ if encoding /= None then "\r\nContent-Encoding: " ++ encodingToString encoding else ""

returnBody :: Response -> BC.ByteString
returnBody NotFound = httpStatus NotFound <> BC.pack "\r\n"
returnBody Created = httpStatus Created <> BC.pack "\r\n"
returnBody (OK body ct encoding) =
    httpStatus (OK newbody ct encoding)
        <> getContentHeader (OK newbody ct encoding)
        <> newbody
  where
    newbody = encodeString encoding body

resolveRequest :: Request -> String -> IO Response
resolveRequest request path = do
    if requestType request == GET
        then resolveGETRequest request path
        else resolvePOSTRequest request path

resolveGETRequest :: Request -> String -> IO Response
resolveGETRequest request path = do
    let url_components = splitOn "/" $ url request
    let url_endpoint = url_components !! 1
    let url_value = if length url_components > 2 then url_components !! 2 else ""
    let encoding = acceptEncoding request

    putStrLn $ show encoding

    case url_endpoint of
        "" -> do return (OK "" TextPlain encoding)
        "echo" -> do return (OK (BC.pack url_value) TextPlain encoding)
        "user-agent" -> do return (OK (BC.pack (userAgent request)) TextPlain encoding)
        "files" -> do
            filecontents <- openFileAsString path (intercalate "/" (drop 2 url_components))
            case filecontents of
                Nothing -> do return NotFound
                (Just content) -> do return (OK (BC.pack content) ApplicationOctetStream encoding)
        _ -> do return NotFound

resolvePOSTRequest :: Request -> String -> IO Response
resolvePOSTRequest request path = do
    let url_components = splitOn "/" $ url request
    let url_endpoint = url_components !! 1
    let encoding = acceptEncoding request

    case url_endpoint of
        "" -> do return (OK "" TextPlain encoding)
        "files" -> do
            writeStringToFile path (intercalate "/" (drop 2 url_components)) (contentText request)
            return Created
        _ -> do return NotFound
