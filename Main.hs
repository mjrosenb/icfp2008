{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where
import Data.Char

import Data.Text.Encoding
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Message
import Parser


parseMsg = parseTel <|> parseInit
drive :: Socket -> IMsg -> IO ()
drive s =  do
  msg <- decodeLatin1 msgBS <$> recv s 4096
  TIO.putStr $ "Received: <<" <> msg <> ">>"
  case parseOnly parseTel msg of
    Right tel -> print tel
    Left err -> putStrLn
  print imsg
  drive s
    
main :: IO ()
main =
  runTCPClient "172.25.128.1" "17676" $ \socket -> do
    msgTet <- decodeLatin1 <$> recv socket 1024 
    TIO.putStr $ "Received Init: " <> msgText
    
    case  parseOnly parseInit msgText
      of Right d -> do
           print d
           drive socket d
         Left err -> TIO.putStr $ "Failed to parse initialization message: " <> err
    
-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
