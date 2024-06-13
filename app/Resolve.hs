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

resolveUrl :: String -> Response
resolveUrl strurl =
    case strurl of
        "/" -> OK ""
        _ -> case url_component of
            "echo" -> OK url_value
            _ -> NotFound
  where
    url_components = splitOn "/" strurl
    url_component = url_components !! 1
    url_value = if length url_components > 2 then url_components !! 2 else ""

resolveRequest :: Request -> Response
resolveRequest request = resolveUrl $ url request
