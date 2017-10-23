{-# LANGUAGE RecordWildCards #-}  -- Allows destructuring of record syntax

import Network
import System.IO
import System.Random
import Control.Exception (bracket_, finally)

-- Allows a function to be run on seperate thread
import Control.Concurrent
import Control.Concurrent.STM -- software transactional memory

-- No idea yet
import Control.Monad.Fix (fix)
import Control.Monad (when, forM_, forever, join)

import Data.Map (Map)
import qualified Data.Map as Map


-- Alias Types for Readability
type ClientId = Int
type ChatRoomId = Int
type ClientName = String
type ChatRoomName = String

-- Using record syntax
-- Haskell autogens lookup functions which return the associated property
-- Eg serverClients myInstantiatedServer yields the list of server clients
data Server = Server {
  serverChatRooms :: TVar (Map ChatRoomName ChatRoom),
  serverClients :: TVar (Map ClientName Client)
}

-- Initialize empty maps for serverClients and serverChatRooms
initServer :: IO Server
initServer =
  Server  <$> newTVarIO Map.empty
          <*> newTVarIO Map.empty


data Client = Client {
  clientId :: ClientId,
  clientName :: String,
  clientHandle :: Handle,
  clientSendChan :: TChan Message
}

initClient :: ClientId -> ClientName -> Handle -> IO Client
initClient id name handle =
  Client  <$> return id
          <*> return name
          <*> return handle
          <*> newTChanIO

data Message = Message {
  messageSender :: Client,
  message :: String
}

data ChatRoom = ChatRoom {
  chatRoomId :: Int,
  chatRoomName :: String,
  chatRoomChan :: Chan Message
}

initChatRoom :: Int -> String -> IO ChatRoom
initChatRoom id name =
  ChatRoom  <$> return id
            <*> return name
            <*> newChan


main :: IO ()
main = do
  server <- initServer
  sock <- listenOn $ PortNumber 3001
  putStrLn $ "Listening on port 3001"

  -- Infinite Looping construct which executes the lambda callback
  forM_ [1..] $ \id -> do
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted connection from " ++ host ++ ": " ++ show port
    forkIO $ serve server id handle `finally` hClose handle


-- Setup everything for this client before polling the socket input
serve :: Server -> ClientId -> Handle -> IO ()
serve server@Server{..} id handle = do
  putStrLn "Im the servr"
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  hPutStrLn handle "What is your name?"
  name <- hGetLine handle
  putStrLn name
  client <- initClient id name handle

--   atomically requires locking of resources
--   bracket wrapper takes over and releases locked resources in case of an exception
--   (atomically $ deleteClient server client)
  bracket_  (atomically $ insertClient server client)
            --(serverLoop server client)



insertClient :: Server -> Client -> STM ()
insertClient server@Server{..}
             client@Client{..} = do
    modifyTVar' serverClients $ Map.insert clientId client


--
-- handleClientJoin :: Server -> Client -> STM ()
-- handleClientJoin  server@Server{..}
--                   client@Client{..} = do
-- --   chatRoomMap <- readTVar serverChatRooms
--   modifyTVar' serverClients $ Map.insert clientId client


--   case Map.lookup "MyChatRoom" chatRoomMap of
--     Nothing -> do
--       chatRoom <- initChatRoom 99 "MyChatRoom"
--       modifyTVar' serverChatRooms $ Map.insert (chatRoomName chatRoom) chatRoom


  --modifyTVar' serverClients $ Map.insert clientId client

--
-- insertClientIntoChatRoom :: TVar Client -> ChatRoom -> STM ()
-- insertClientIntoChatRoom client chatRoom = do
--   modifyTVar
--
--
-- createChatRoom :: Server -> String -> STM ()
-- createChatRoom server name = do
--   chatRoom <- initChatRoom 99 "MyChatRoom"
--   modifyTVar' serverChatRooms $ Map.insert name chatRoom
--
--
--
-- deleteClient :: Server -> Client -> IO ()
-- deleteClient server client = do
--   client <- atomically (readTVar client)
--
--
--
--
--
serverLoop :: Server -> Client -> IO ()
serverLoop server client = do putStrLn "Server loop here"
--
--
--









{--

-- Repeatedly waits for incoming socket connections
-- Spawns a new thread to handle each connection it receives
acceptConnections :: Socket -> Chan Message -> Int -> IO ()
acceptConnections sock chan currentId = do
    (socket, _) <- accept sock         -- accept a connection and handle it

    handle <- socketToHandle socket ReadWriteMode
    hSetBuffering handle LineBuffering
    hSetNewlineMode handle universalNewlineMode

    forkIO (handleClient handle chan currentId)
    acceptConnections sock chan $ currentId+1




-- Note dupChan makes a copy of the channel
-- any data written to it OR written to the initial channel will be available at both channel
-- This gives us broadcast functionality between threads


-- Handler run on separate thread for each client's socket connection
handleClient :: Handle -> Chan Message -> Int -> IO ()
handleClient handle chan id  = do

  commLine <- dupChan chan

  hPutStrLn handle "Give me your name: "
  name <- hGetLine handle

  let client = Client {
    clientId = id,
    clientName = name,
    clientHandle = handle
  }


  -- Spawn a thread for monitoring the channel and sending new messages to this client
  forkIO (broadCastMessageToClient client commLine)

  -- Let this thread read form socket
  readFromSocket client chan


  hClose $ clientHandle client


-- Reads messages from socket and broadcasts it to the threads' channel
readFromSocket :: Client -> Chan Message -> IO ()
readFromSocket client channel  = do
  message <- hGetLine $ clientHandle client
  writeChan channel (Message {messageSender = client, message = message})
  readFromSocket client channel

-- Read from the shared channel and send it to the client
broadCastMessageToClient :: Client -> Chan Message  -> IO ()
broadCastMessageToClient client commLine = do
  readMessage <- readChan commLine
  hPutStrLn (clientHandle client) ((clientName $ messageSender readMessage) ++ ": " ++ message readMessage)

  broadCastMessageToClient client commLine

  --}