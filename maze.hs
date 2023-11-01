{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.ByteString.Lazy.Char8 (pack)

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Starting server on port " ++ show port
    run port app

app :: Application
app _ respond = do
    let body = "<html><head></head>"
         ++ "<style>body {background-color: black;text-align: center;color: white;font-size: 60px;}h1 {margin-top: 30vh;animation: textAnimation 2s ease-in-out infinite;}"
         ++"@keyframes textAnimation {0% {transform: scale(1); opacity: 0;color: green;}50% {transform: scale(1.1);opacity: 1;color: rgb(90, 143, 212);}100% {transform: scale(1);opacity: 0;color: blue;}"
         ++ "div{margin:200px;}</style>"
         ++ "<title>My Haskell Homepage</title>"
         ++ "<body><h1>Welcome To The Gaming Site</h1><br></body></html>"
         ++ "<div class='box'>"
         ++ "<a href='#'><img src='/images/1.jpg'></a>"
         ++ "<a href='#'><img src='/images/2.jpg' alt='Image 2'></a>"
         ++ "<a href='#'><img src='/images/3.jpg' alt='Image 3'></a></div>"

    respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        (pack body)