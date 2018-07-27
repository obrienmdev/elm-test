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
import qualified Control.Concurrent.Timer       as Timer
import qualified Control.Concurrent.Suspend.Lifted as Suspend
import qualified Network.HTTP                   as HTTP
import qualified Text.Printf                    as Printf
import qualified Text.Regex                     as Regex
import qualified System.IO                      as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout $ IO.LineBuffering
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
  Printf.printf "Client %d connected\n" clientId
  --WS.sendTextData conn (Text.pack ("Connected as " ++ (show clientId)))
  return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state -> do
  Printf.printf "Client %d disconnected\n" clientId
  return $ withoutClient clientId state

listen :: WS.Connection -> Concurrent.MVar State -> IO ()
listen conn stateRef = do
  _ <- Timer.repeatedTimer (fetch stateRef) (Suspend.sDelay 10)
  WS.receiveData conn >>= broadcast stateRef

fetch :: Concurrent.MVar State -> IO ()
fetch stateRef = do
  putStrLn "fetching"
  body <- HTTP.simpleHTTP makeRequest >>= HTTP.getResponseBody
  broadcast stateRef $ Text.pack $ getCommitMessage body

makeRequest :: HTTP.Request_String
makeRequest = HTTP.getRequest "http://whatthecommit.com/"

getCommitMessage :: String -> String
getCommitMessage body = do
  let regex = Regex.mkRegex "<p>(.*)"
  let matches = Regex.matchRegex regex body
  head $ Maybe.fromMaybe [""] matches

broadcast :: Concurrent.MVar State -> Text.Text -> IO ()
broadcast stateRef msg = do
  Printf.printf "broadcasting: %s\n" (Text.unpack msg)
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


