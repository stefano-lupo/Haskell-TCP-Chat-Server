module Main where

import Network.Socket
import System.IO
import Control.Exception

-- Allows a function to be run on seperate thread
import Control.Concurrent

-- No idea yet
import Control.Monad.Fix (fix)
import Control.Monad (when)

-- Using record syntax
-- Haskell autogens lookup functions which return the associated property
-- Eg serverClients myInstantiatedServer yields the list of server clients
data Server = Server {
  serverClients :: [Client]
} deriving (Show)

data Client = Client {
  clientId :: Int,
  clientName :: String,
  clientHandle :: Handle
} deriving (Show)

data Message = Message {
  messageSender :: Client,
  message :: String
} deriving (Show)

data ChatRoom = ChatRoom {
  chatRoomName :: String,
  chatRoomId :: Int
}

port :: PortNumber
port = 3000

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0             -- create socket
    setSocketOption sock ReuseAddr 1            -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (port) iNADDR_ANY)  -- listen on TCP port <port>
    listen sock 2                               -- set a max of 2 queued connections
    chan <- newChan                             -- Create Channel for thread communications
    print $ "Listening on port " ++ show port
    acceptConnections sock chan 0


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