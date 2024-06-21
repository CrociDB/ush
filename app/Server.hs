{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.Directory
import System.IO (BufferMode (..), hSetBuffering, stdout)

import AppData
import Logger
import Request
import Resolve

serve :: FilePath -> String -> String -> IO ()
serve directory address port = do
    hSetBuffering stdout LineBuffering

    path <- canonicalizePath directory

    BC.putStrLn $ BC.pack appHeader <> "\n"
    logMessage $ "Serving directory: " ++ path
    logMessage $ "Listening on " ++ address ++ ":" ++ port

    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just address) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1
    withFdSocket serverSocket setCloseOnExecIfNeeded

    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5

    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket

        message <- recv clientSocket 4096
        let request = createRequest $ BC.unpack message

        logMessage $
            show (requestType request)
                ++ " "
                ++ show (url request)
                ++ " from: "
                ++ show clientAddr
                ++ " | "
                ++ show (userAgent request)

        response <- resolveRequest request path

        _ <- send clientSocket $ returnBody response

        close clientSocket
