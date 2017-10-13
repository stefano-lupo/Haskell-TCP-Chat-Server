import Network
import System.IO

joinChatRoom = "JOIN_CHATROOM: Frog\n"
  ++ "CLIENT_IP: 0\n"
  ++ "PORT: 0\n"
  ++ "CLIENT_NAME: Stefano\n"




main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber 3000)
  message <- hGetLine handle
  putStrLn $ "From server: " ++ message

  putStrLn "Enter message: "
  toServer <- getLine
  hPutStrLn handle toServer

  hClose handle


mainLoop :: Handle -> IO ()
mainLoop handle = do
  received <- hGetLine handle
  putStrLn "From server :"

  putStrLn "Enter your message: "
  message <- getLine

  putStrLn $ "Sending to server: " ++ message
  sendMessage handle "Frank"
  mainLoop handle



sendMessage :: Handle -> String -> IO ()
sendMessage handle =  hPutStr handle


