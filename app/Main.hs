{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Request (createRequest)
import Resolve
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
        port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port
    --
    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

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
        let response = resolveRequest request

        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        _ <- send clientSocket $ BC.pack $ returnBody response

        BC.putStrLn $ BC.pack $ returnBody response

        close clientSocket
