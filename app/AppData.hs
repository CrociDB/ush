module AppData where

import qualified Data.ByteString.Char8 as BC

appHeader = "ush serves http v0.1"
appDetails = ["A simple HTTP Server to serve your files"]

indexPage = "<html><head><title>Index of %1</title></head><body><h1>Index of %1</h1><hr><ul>%2</ul><hr><p>%3</p></body></html>"

indexPageItem = "<li><a href=\"%1\">%2</a></li>"
