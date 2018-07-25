{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ file "./index.html"
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["Scotty, ", beam, " me up!"]
