{-# LANGUAGE OverloadedStrings #-}
module Parser(parseInit, parseTel, parseMessage) where
import Data.Attoparsec.Text hiding (I)
import Control.Applicative
import Message
import Point
parseAccel = 
  (char 'a' >> return Accelerating) <|>
  (char 'b' >> return Braking) <|>
  (char '-' >> return Rolling)

parseTurning =
  (char 'L' >> return HardLeft) <|>
  (char 'l' >> return NormLeft) <|>
  (char '-' >> return Straight) <|>
  (char 'r' >> return NormRight) <|>
  (char 'R' >> return HardRight)

parseVehicleCtrl = do -- the vehicle control field is two characters, without spaces
  accel <- parseAccel
  turn <- parseTurning
  return $ VC accel turn


parseStatic = do
  x <-double'
  y <- double'
  r <- double'
  return $ S (P x y) r
  
parseObject = (char' 'b' >> (parseStatic >>= (return . Boulder))) <|>
              (char' 'c' >> (parseStatic >>= (return . Crater))) <|>
              (char' 'h' >> (parseStatic >>= (return . Home)))  <|>
              (do char' 'm'
                  x <- double'
                  y <- double'
                  dir <- double'
                  speed <- double' 
                  let static = S (P x y) 0.4 -- martians always have a radius of 0.4
                  return $ Martian static dir speed
              )


andSpace p = do
  ret <- p
  space
  return ret
  
char' c = andSpace (char c)
string' s = andSpace (string s)
double' = andSpace double
decimal' = andSpace decimal

-- Parse the initialization message
parseInit = do
  string' "I"
  x <- double'
  y <- double'
  tl <- decimal'
  minSen <- double'
  maxSen <- double'
  maxSp <- double'
  maxT <- double'
  maxHT <- double'
  string ";"
  return I {_size = P x y,
            _timeLimit = tl,
            _minSensor = minSen, _maxSensor = maxSen,
            _maxSpeed = maxSp, _maxTurn = maxT, _maxHardTurn = maxHT}

-- Parse the telemetry message
parseTel = do
  char' 'T'
  timeStamp <- decimal'
  vehicleCtrl <- andSpace parseVehicleCtrl
  vehicleX <- double'
  vehicleY <- double'
  vehicleDir <- double'
  vehicleSpeed <- double'
  objects <- many' parseObject
  char ';'
  return $ T  timeStamp vehicleCtrl (P vehicleX vehicleY) vehicleDir vehicleSpeed objects

parseStatus = do
  char' 'B'
  timeStamp <- decimal'
  char ';'
  return $ B timeStamp
parseCrash = do
  char' 'C'
  timeStamp <- decimal'
  char ';'
  return $ C timeStamp

parseKilled = do
  char' 'K'
  timeStamp <- decimal'
  char ';'
  return $ K timeStamp

parseSuccess = do
  char' 'S'
  timeStamp <- decimal'
  char ';'
  return $ W timeStamp

parseEnd = do
  char' 'E'
  timeStamp <- decimal'
  score <- decimal'
  char ';'
  return $ E timeStamp score
  
parseEnds = do
  choice [parseEnd, parseSuccess, parseKilled, parseCrash]

parseMessage = do
  choice [End <$> parseEnds, Status <$> parseStatus, Telemetry <$> parseTel]
