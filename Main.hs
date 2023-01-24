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
import Message
import Parser
import System.Environment
import Control.Monad.State.Lazy
import State
import Control.Lens
import Data.Monoid
driveSpiral :: TMsg -> State SpiralingSt (First DriveCommand)
driveSpiral telemetry =  do
  sc <- straightCount <+= 1
  maxC <- use maxStraight
  if sc == maxC then
    return $ first DC {_accel = Just Accel, _turn = Just LeftTurn}
    else if sc == maxC + 4 then do
    maxStraight += 3
    straightCount .= 0
    return $ first DC {_accel = Just Accel, _turn = Just RightTurn}
  else
    return $ first DC {_accel = Just Accel, _turn = Nothing}
first :: a -> First a
first x = First (Just x)
drive :: TMsg -> State DriveState DriveCommand
drive telemetry = do
  st <- use smState
  -- sp <- preuse (smState . _Spiraling) :: State DriveState (Maybe SpiralingSt)
  sp <- zoom (smState . _Spiraling) (driveSpiral telemetry) -- :: First DriveCommand
  
  case getFirst sp of
    Just command -> return command
    Nothing -> return undefined
communicate :: Socket -> IMsg -> DriveState -> IO ()
communicate s init ds =  do
  msg <- decodeLatin1 <$> recv s 4096
  TIO.putStr $ "Received: <<" <> msg <> ">>"
  ds' <- case parseOnly parseTel msg of
    Right tel -> do print tel
                    let (cmd, ds') = runState (drive tel) ds
                        vX = _vehicleX tel
                        vY = _vehicleY tel
                        vX' = tel ^. vehicleX
                        vY' = tel ^. vehicleY
                    print ((vX, vY) :: (Double, Double))
                    print ((vX', vY') :: (Double, Double))
                    sendAll s (showCommandBS cmd)
                    return ds'
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
           communicate socket d initState
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
