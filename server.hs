module Main where

import Network.Socket
import System.IO
import Control.Exception

-- Allows a function to be run on seperate thread
import Control.Concurrent

-- No idea yet
import Control.Monad.Fix (fix)

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
    acceptConnections sock chan


-- Semantics
type Msg = String

-- Repeatedly waits for incoming socket connections
-- Spawns a new thread to handle each connection it receives
acceptConnections :: Socket -> Chan Msg -> IO ()
acceptConnections sock chan = do
    conn <- accept sock         -- accept a connection and handle it
    forkIO (handleClient conn chan)
    acceptConnections sock chan

-- Note dupChan makes a copy of the channel
-- any data written to it OR written to the initial channel will be available at both channel
-- This gives us broadcast functionality between threads

handleClient :: (Socket, SockAddr) -> Chan Msg -> IO ()
handleClient (sock, _) chan = do
  let broadcast msg = writeChan chan msg        -- Define a function for broadcasting
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  forkIO $ fix $ \loop -> do
      line <- readChan commLine
      putStrLn ("Read from Channel: " ++ line)
      hPutStrLn handle line
      loop

  -- read lines from the socket broadcast to channel
  fix $ \loop -> do
      line <- hGetLine handle
      putStrLn line
      broadcast line
      loop


  -- handleConnection handle
  hClose handle



















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