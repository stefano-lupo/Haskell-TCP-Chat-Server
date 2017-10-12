import Network
import System.IO

main :: IO ()
main = withSocketsDo $ do
         handle <- connectTo "localhost" (PortNumber 3001)
         hPutStr handle "JOIN_CHATROOM: ChatFrog"
         hClose handle