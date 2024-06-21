
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Request (createRequest)
import Resolve
import System.Directory
import System.IO (BufferMode (..), hSetBuffering, stdout)

serve :: FilePath -> String -> String -> IO ()
serve directory host port = do
    hSetBuffering stdout LineBuffering

    path <- canonicalizePath directory

    BC.putStrLn $ "Serving directory: " <> BC.pack path
    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

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
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

        message <- recv clientSocket 4096
        let request = createRequest $ BC.unpack message
        response <- resolveRequest request path

        _ <- send clientSocket $ returnBody response

        BC.putStrLn "Replying: "
        BC.putStrLn $ returnBody response

        close clientSocket
