{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp ( run )
import Network.HTTP.Types
import Data.ByteString.Lazy.Char8 (pack)

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Starting server on port " ++ show port
    run port app

app :: Application
app _ respond = do
    let body = "<html><head><title>My Haskell Homepage</title><style>body {background-color : black; text-align: center; color : white; font-size: 60px;} h1 { margin-top: 30vh; }</style></head><body><h1>Welcome To The Gaming Site</h1></body></html>"
    respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        (pack body)
