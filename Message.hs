{-# LANGUAGE TemplateHaskell #-}
module Message  where
import Control.Lens
-- Enums for various enum-like fields
data Accel = Accelerating | Braking | Rolling deriving (Show, Eq, Ord)
data Turning = NormLeft | HardLeft | Straight | NormRight | HardRight deriving (Show, Eq, Ord)
data VehicleControl = VC Accel Turning deriving (Show, Eq, Ord)

-- A shared data container for all of the types of objects.
data Static = S {_x, _y, _r :: Double} deriving (Show, Eq, Ord)

data Object = Boulder {_static :: Static}
            | Crater {_static :: Static}
            | Home {_static :: Static}
            | Martian {_static :: Static, _dir, _speed :: Double} deriving (Show, Eq, Ord)

data IMsg = I { _dx, _dy :: Double
              , _timeLimit :: Int
              , _minSensor, _maxSensor :: Double
              , _maxSpeed, _maxTurn, _maxHardTurn :: Double
              }
          deriving (Show, Eq, Ord)
data TMsg = T { _timeStamp :: Int
              , _vehicleCtrl :: VehicleControl
              , _vehicleX :: Double
              , _vehicleY :: Double
              , _vehicleDir :: Double
              , _vehicleSpeed :: Double
              , _objects :: [Object]
              }

          deriving (Show, Eq, Ord)

makeLenses ''Static
makeLenses ''Object
makeLenses ''IMsg
makeLenses ''TMsg
