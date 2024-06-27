{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import AppData
import Server

import System.Console.CmdArgs

strEmptyDefault :: String -> String -> String
strEmptyDefault "" defo = defo
strEmptyDefault str _ = str

data USH = ServerParams
    { directory :: FilePath
    , port :: String
    , host :: String
    }
    deriving (Data, Typeable, Show, Eq)

ush :: USH
ush =
    ServerParams
        { directory = def &= name "d" &= typDir &= help "Directory to serve"
        , port = def &= name "p" &= help "Port"
        , host = def &= name "a" &= help "Address"
        }
        &= summary appHeader
        &= details appDetails


main :: IO ()
main = do
  params <- cmdArgs ush
  serve (directory params) (strEmptyDefault (host params) "localhost") (strEmptyDefault (port params) "8080")

