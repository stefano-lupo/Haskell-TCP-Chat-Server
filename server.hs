module Main where

import Network.Socket
import System.IO
import Control.Exception

-- Allows a function to be run on seperate thread
import Control.Concurrent

-- No idea yet
import Control.Monad.Fix (fix)
import Control.Monad (when)

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


-- Semantics
type Msg = String

-- Repeatedly waits for incoming socket connections
-- Spawns a new thread to handle each connection it receives
acceptConnections :: Socket -> Chan Msg -> Int -> IO ()
acceptConnections sock chan currentId = do
    conn <- accept sock         -- accept a connection and handle it
    forkIO (handleClient conn chan currentId)
    acceptConnections sock chan $ currentId+1

-- Note dupChan makes a copy of the channel
-- any data written to it OR written to the initial channel will be available at both channel
-- This gives us broadcast functionality between threads

handleClient :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
handleClient (sock, _) chan clientId = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  commLine <- dupChan chan

  -- Spawn a thread for monitoring the channel and sending new messages to this client
  forkIO (broadCastMessageToClient handle commLine clientId)

  readFromSocket handle chan clientId


  -- read lines from the socket broadcast to channel
--   fix $ \loop -> do
--       line <- hGetLine handle
--       putStrLn line
--       broadcast line
--       loop
  hClose handle


-- Reads messages from socket and broadcasts it to the threads' channel
readFromSocket :: Handle -> Chan Msg -> Int -> IO ()
readFromSocket handle channel clientId = do
  line <- hGetLine handle
  writeChan channel (show clientId ++ ": " ++ line)
  readFromSocket handle channel clientId

-- Read from the shared channel and send it to the client
broadCastMessageToClient :: Handle -> Chan Msg -> Int -> IO ()
broadCastMessageToClient handle commLine clientId = do
  message <- readChan commLine
  hPutStrLn handle message

  broadCastMessageToClient handle commLine clientId



















--
-- main :: IO ()
-- main = withSocketsDo $ do
--   sock <- listenOn $ PortNumber $ port
--   putStrLn ("Starting server on port " ++ show port)
--   handleConnections sock
--
-- handleConnections :: Socket -> IO ()
-- handleConnections sock = do
--   (handle, host, port) <- accept sock
--   message <- hGetContents handle
--   handleMessage message
--   handleConnections sock
--
--
-- handleMessage message
--  | fst parsed == "JOIN_CHATROOM" = joinChatroom $ snd parsed
--  | otherwise = unknownCommand $ fst parsed
--  where parsed = span (/= ':') message
--
-- joinChatroom :: String -> String
-- joinChatroom rest = "Join Chatroom command received"
--
-- unknownCommand :: String -> String
-- unknownCommand rest = "Unknown command recevied"