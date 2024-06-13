{-# LANGUAGE OverloadedStrings #-}

module Resolve (Response, resolveRequest, returnBody) where

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

resolveUrl :: String -> String -> Maybe String
resolveUrl strurl path = Nothing

resolveRequest :: Request -> String -> Response
resolveRequest request path =
    case file_contents of
        Nothing -> case url_endpoint of
            "echo" -> OK url_value
            "user-agent" -> OK $ userAgent request
            _ -> NotFound
        (Just contents) -> OK contents
  where
    file_contents = resolveUrl (url request) path
    url_components = splitOn "/" $ url request
    url_endpoint = url_components !! 1
    url_value = if length url_components > 2 then url_components !! 2 else ""
