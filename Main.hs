{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where
import Data.Char
import Data.Maybe
import Data.Attoparsec.Text hiding (I)
import Data.Text.Encoding
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment
import Control.Monad.State.Lazy
import Control.Lens
import Data.Monoid
import Data.List

import Message
import Parser
import State
import Point
first :: a -> First a
first x = First (Just x)

drive :: TMsg -> State DriveState DriveCommand
drive telemetry = do
  let dx = telemetry ^. vehiclePos . x
      dy = telemetry ^. vehiclePos . y
      angle = telemetry ^. vehicleDir . to floor
      radToGoal =  atan2 (negate dy) (negate dx)
      degToGoal = floor (radToGoal * 180 / pi)
      fastestTurn = minimumBy (\x y -> compare (abs x) (abs y) ) [angle - degToGoal, angle - degToGoal - 360]
      direction = if fastestTurn < 0
                  then Just LeftTurn
                  else if fastestTurn > 0
                       then Just RightTurn
                       else Nothing
  return DC {_accel =Just Accel, _turn = direction}


communicate :: Socket -> IMsg -> DriveState -> IO ()
communicate s init ds =  do
  msg <- decodeLatin1 <$> recv s 4096
  ds' <- case parseOnly parseMessage msg of
    Right (Telemetry tel) -> do print tel
                                let (cmd, ds') = runState (drive tel) ds
                                    vX = tel ^. vehiclePos . x
                                    vY = tel ^. vehiclePos . y
                                print cmd
                                print ds'
                                sendAll s (showCommandBS cmd)
                                return ds' 
    Right x -> do putStrLn $ "Non-telemetry message: " ++ show x
                  return ds
    Left err -> do putStrLn $ "Couldn't parse telemetry: " <> err
                   return ds
  communicate s init ds'
    
main :: IO ()
main = do
  args <- getArgs
  let (host, port) = case args of [] -> ("172.25.128.1", "17676")
                                  [ip] -> (ip, "17676")
                                  ip:port:_ -> (ip, port)
  runTCPClient  host port $ \socket-> do
    msgText <- decodeLatin1 <$> recv socket 1024 
    TIO.putStr $ "Received Init: " <> msgText
    
    case  parseOnly parseInit msgText
      of Right d -> do
           print d
           communicate socket d (initState (d ^. size))
         Left err -> putStr $ "Failed to parse initialization message: " <> err
    
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
