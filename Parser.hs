{-# LANGUAGE OverloadedStrings #-}
module Parser(parseInit, parseTel) where
import Data.Attoparsec.Text hiding (I)
import Control.Applicative
import Message
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
  r <- double -- don't parse the terminal space
  return $ S x y r
  
parseObject = (char 'b' >> (parseStatic >>= (return . Boulder))) <|>
              (char 'c' >> (parseStatic >>= (return . Crater))) <|>
              (char 'h' >> (parseStatic >>= (return . Home)))  <|>
              (do char 'm'
                  space
                  x <- double'
                  y <- double'
                  dir <- double'
                  speed <- double -- don't eat the terminal space
                  let static = S x y 0.4 -- martians always have a radius of 0.4
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
  return I {_dx = x, _dy = y,
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
  objects <- sepBy  parseObject (char ' ')
  space
  char ';'
  return $ T  timeStamp vehicleCtrl vehicleX vehicleY vehicleDir vehicleSpeed objects

-- TODO: Parse the ending messages
