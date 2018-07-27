{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import Control.Concurrent.Timer -- repeatedTimer
import Control.Concurrent.Suspend.Lifted -- sDelay
import Network.HTTP
import Text.Printf
import Text.Regex
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout $ LineBuffering
  putStrLn "start"

  state <- Concurrent.newMVar []
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  printf "Client %d connected\n" clientId
  --WS.sendTextData conn (Text.pack ("Connected as " ++ (show clientId)))
  return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state -> do
  printf "Client %d disconnected\n" clientId
  return $ withoutClient clientId state

listen :: WS.Connection -> Concurrent.MVar State -> IO ()
listen conn stateRef = do
  _ <- repeatedTimer (fetch stateRef) (sDelay 10)
  WS.receiveData conn >>= broadcast stateRef

fetch :: Concurrent.MVar State -> IO ()
fetch stateRef = do
  body <- simpleHTTP (getRequest "http://whatthecommit.com/") >>= getResponseBody
  -- extract message from body
  putStrLn "fetching"
  let message = getCommitMessage body
  putStrLn ("message: " ++ message)
  broadcast stateRef (Text.pack message)

getCommitMessage :: String -> String
getCommitMessage body = do
  let regex = mkRegex "<p>(.*)"
  let matches = matchRegex regex body
  let match = head $ Maybe.fromMaybe [""] matches
  match

broadcast :: Concurrent.MVar State -> Text.Text -> IO ()
broadcast stateRef msg = do
  printf "broadcasting: %s\n" (Text.unpack msg)
  clients <- Concurrent.readMVar stateRef
  Monad.forM_ clients $ \(_, conn) ->
    WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn stateRef)
    (disconnectClient clientId stateRef)


