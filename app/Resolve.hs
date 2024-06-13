{-# LANGUAGE OverloadedStrings #-}

module Resolve (Response, resolveRequest, returnBody) where

import Filesystem
import Request

import Data.List.Split

data Response = OK String | NotFound

httpStatus :: Response -> String
httpStatus NotFound = "HTTP/1.1 404 Not Found\r\n"
httpStatus (OK _) = "HTTP/1.1 200 OK\r\n"

getContentHeader :: String -> String
getContentHeader body = "Content-Type: text/plain\r\nContent-Length: " ++ show (length body) ++ "\r\n\r\n"

returnBody :: Response -> String
returnBody NotFound = httpStatus NotFound ++ "\r\n"
returnBody (OK body) = httpStatus (OK body) ++ getContentHeader body ++ body

resolveRequest :: Request -> String -> IO Response
resolveRequest request path = do
    filecontents <- openFileAsString path (url request)

    case filecontents of
        Nothing -> do
            let url_components = splitOn "/" $ url request
            let url_endpoint = url_components !! 1
            let url_value = if length url_components > 2 then url_components !! 2 else ""

            return $ case url_endpoint of
                "echo" -> OK url_value
                "user-agent" -> OK $ userAgent request
                _ -> NotFound
        (Just content) -> do return (OK content)
