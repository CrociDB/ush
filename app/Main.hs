{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Request (createRequest)
import Resolve
import System.Environment
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- command line arguments
data Args = Args
    { aPort :: String
    , aPath :: String
    , aHost :: String
    }
    deriving (Show)

getArgValue :: [String] -> String -> String -> String
getArgValue args key dv = if null args then dv else current_value
  where
    k = head args
    v = if length args > 1 then args !! 1 else ""
    current_value = if k == key then v else getArgValue (tail args) key dv

parseArgs :: [String] -> Args
parseArgs args =
    Args
        { aPort = "4221"
        , aPath = getArgValue args "--directory" "."
        , aHost = "127.0.0.1"
        }

-- entrypoint
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    arglist <- getArgs
    let args = parseArgs arglist

    BC.putStrLn $ "Listening on " <> BC.pack (aHost args) <> ":" <> BC.pack (aPort args)

    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just (aHost args)) (Just (aPort args))

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
