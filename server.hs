module Main where

import Network.Socket
import System.IO

-- Allows a function to be run on seperate thread
import Control.Concurrent

-- Need some Inter Thread Messaging
-- Going to use Controll.Concurrent.Chan (already imported ^)


-- No idea yet
import Control.Monad.Fix (fix)

-- Semantics
type Msg = String


port :: PortNumber
port = 3000


main :: IO ()
main = do
    sock <- socket AF_INET Stream 0             -- create socket
    setSocketOption sock ReuseAddr 1            -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet (port) iNADDR_ANY)  -- listen on TCP port <port>
    print $ "Listening on port " ++ show port
    listen sock 2                               -- set a max of 2 queued connections
    acceptConnections sock                               -- Pass it to mainloop along with the socket

acceptConnections :: Socket -> IO ()
acceptConnections sock = do
    conn <- accept sock         -- accept a connection and handle it
    forkIO (runConn conn)
    acceptConnections sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  handle <- socketToHandle sock ReadWriteMode
  handleConnection handle
  hClose handle


-- Run Conn runs on a seperate thread
-- But it also creates a worker thread for sending messages to client (commLine)
handleConnection :: Handle -> IO ()
handleConnection handle = do
  hPutStrLn handle "Welcome Mr Client"
  message <- hGetLine handle
  print ("From Client: " ++ message)
  handleConnection handle


















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