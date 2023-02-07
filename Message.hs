{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Message  where
import Control.Lens
import Data.ByteString
import Point
-- Enums for various enum-like fields
data Accel = Accelerating | Braking | Rolling deriving (Show, Eq, Ord)
data Turning = NormLeft | HardLeft | Straight | NormRight | HardRight deriving (Show, Eq, Ord)
data VehicleControl = VC Accel Turning deriving (Show, Eq, Ord)

-- A shared data container for all of the types of objects.
data Static = S {_center :: Point Double, _r :: Double} deriving (Show, Eq, Ord)

data Object = Boulder {_static :: Static}
            | Crater {_static :: Static}
            | Home {_static :: Static}
            | Martian {_static :: Static, _dir, _speed :: Double} deriving (Show, Eq, Ord)

data IMsg = I { _size :: Point Double
              , _timeLimit :: Int
              , _minSensor, _maxSensor :: Double
              , _maxSpeed, _maxTurn, _maxHardTurn :: Double
              }
          deriving (Show, Eq, Ord)
-- A Telemetry message
data TMsg = T { _timeStamp :: Int
              , _vehicleCtrl :: VehicleControl
              , _vehiclePos :: Point Double
              , _vehicleDir :: Double
              , _vehicleSpeed :: Double
              , _objects :: [Object]
              }
          deriving (Show, Eq, Ord)

-- A status message
data SMsg = B Int
  deriving (Show, Eq, Ord)

-- An ending message
data EMsg = C Int -- Crash
          | K Int
          | W Int
          | E {_time :: Int, _score :: Int}
  deriving (Show, Eq, Ord)


data Msg = Telemetry TMsg
         | Status SMsg
         | End EMsg
          deriving (Show, Eq, Ord)

data DriveAccel = Accel | Brake deriving (Show, Eq, Ord)
data DriveTurn = LeftTurn | RightTurn deriving (Show, Eq, Ord)
data DriveCommand = DC { _accel :: Maybe DriveAccel, _turn :: Maybe DriveTurn } deriving (Show, Eq, Ord)
makeLenses ''Static
makeLenses ''Object
makeLenses ''IMsg
makeLenses ''TMsg
makeLenses ''DriveCommand

showAccelBS :: DriveAccel -> ByteString
showAccelBS Accel = "a"
showAccelBS Brake = "b"

showTurnBS :: DriveTurn -> ByteString
showTurnBS LeftTurn = "l"
showTurnBS RightTurn = "r"

showCommandBS DC {_accel = accel, _turn = turn} =
  maybe "" showAccelBS accel <>
  maybe ""  showTurnBS turn <>
  ";"
