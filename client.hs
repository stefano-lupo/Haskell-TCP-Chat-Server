import Network
import System.IO

joinChatRoom = "JOIN_CHATROOM: Frog\n"
  ++ "CLIENT_IP: 0\n"
  ++ "PORT: 0\n"
  ++ "CLIENT_NAME: Stefano\n"


main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber 3000)
  interactWithServer handle
  hClose handle


interactWithServer :: Handle -> IO ()
interactWithServer handle = do

  -- Send Message
  putStr "Enter message: "
  toServer <- getLine
  hPutStrLn handle toServer

  -- Get Echo
  message <- hGetLine handle
  putStrLn $ "From server: " ++ message

  interactWithServer handle