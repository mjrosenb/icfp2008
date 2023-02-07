{-# LANGUAGE TemplateHaskell #-}
module State where
import qualified Message as Msg
import qualified Data.Set as S
import Control.Lens
data SpiralingSt = SpiralingSt { _straightCount :: Int
                               , _maxStraight :: Int
                               } deriving (Show, Eq)
data EndInSightSt = EndInSightSt { _endX :: Double
                                 , _endY :: Double
                                 } deriving (Show, Eq)
data SMState = Spiraling  SpiralingSt
             | EndInSight EndInSightSt deriving (Show, Eq)



data DriveState = DriveState { _objects :: S.Set Msg.Object
                             , _smState :: SMState
                             } deriving (Show, Eq)
makeLenses ''DriveState
makeLenses ''SpiralingSt
makeLenses ''EndInSightSt
makePrisms ''SMState
initState = DriveState { _objects = S.empty
                       , _smState = Spiraling SpiralingSt { _straightCount = 0
                                                          , _maxStraight = 4
                                                          }
                       }
