{-# LANGUAGE OverloadedStrings #-}

module Resolve (Response, resolveRequest, returnBody) where

import Filesystem
import Request 
import ContentType

import Data.List.Split
import Data.List (intercalate)

data Response = OK String ContentType | NotFound | Created

httpStatus :: Response -> String
httpStatus NotFound = "HTTP/1.1 404 Not Found\r\n"
httpStatus (OK _ _) = "HTTP/1.1 200 OK\r\n"
httpStatus Created = "HTTP/1.1 201 Created\r\n"

getContentHeader :: Response -> String
getContentHeader NotFound = ""
getContentHeader Created = ""
getContentHeader (OK body ctype) = "Content-Type: " ++ contentTypeToString ctype ++ "\r\nContent-Length: " ++ show (length body) ++ "\r\n\r\n"

returnBody :: Response -> String
returnBody NotFound = httpStatus NotFound ++ "\r\n"
returnBody Created = httpStatus Created ++ "\r\n"
returnBody (OK body ct) = httpStatus (OK body ct) ++ getContentHeader (OK body ct) ++ body

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

    case url_endpoint of
        "" -> do return (OK "" TextPlain)
        "echo" -> do return (OK url_value TextPlain)
        "user-agent" -> do return (OK (userAgent request) TextPlain)
        "files" -> do
            filecontents <- openFileAsString path (intercalate "/" (drop 2 url_components))
            case filecontents of
                Nothing -> do return NotFound
                (Just content) -> do return (OK content ApplicationOctetStream)
        _ -> do return NotFound

resolvePOSTRequest :: Request -> String -> IO Response
resolvePOSTRequest request path = do
    let url_components = splitOn "/" $ url request
    let url_endpoint = url_components !! 1
    let url_value = if length url_components > 2 then url_components !! 2 else ""

    putStrLn "THIS IS A POST REQUEST"

    case url_endpoint of
        "" -> do return (OK "" TextPlain)
        "files" -> do
            writeStringToFile path (intercalate "/" (drop 2 url_components)) (contentText request)
            return Created
        _ -> do return NotFound
