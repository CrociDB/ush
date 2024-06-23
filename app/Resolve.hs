{-# LANGUAGE OverloadedStrings #-}

module Resolve (Response, resolveRequest, returnBody) where

import ContentType
import Encoding
import Filesystem
import Request

import qualified Data.ByteString.Char8 as BC

data Response = OK BC.ByteString ContentType EncodingType | NotFound | Created

httpStatus :: Response -> BC.ByteString
httpStatus NotFound = "HTTP/1.1 404 Not Found\r\n"
httpStatus OK{} = "HTTP/1.1 200 OK\r\n"
httpStatus Created = "HTTP/1.1 201 Created\r\n"

getContentHeader :: Response -> BC.ByteString
getContentHeader NotFound = ""
getContentHeader Created = ""
getContentHeader (OK body ctype encoding) =
    content_type_line
        <> encoding_line
        <> "\r\nContent-Length: "
        <> BC.pack (show (BC.length body))
        <> "\r\n\r\n"
  where
    content_type_line = BC.pack $ "Content-Type: " ++ contentTypeToString ctype
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
    case requestType request of
        GET -> resolveGETRequest request path
        POST -> resolveGETRequest request path -- only GET request for now

resolveGETRequest :: Request -> String -> IO Response
resolveGETRequest request path = do
    let encoding = acceptEncoding request

    filedata <- resolveFile path (url request)

    case filedata of
        Nothing -> do return NotFound
        (Just (LoadedFileData content mimetype)) -> do
            return (OK content mimetype encoding)
