{-# LANGUAGE TemplateHaskell #-}
module State where
import qualified Message as Msg
import qualified Data.Set as S
import qualified Data.Array as A
import Control.Lens
import Point

data DStarStatus =
    CLOSED
  | OPEN
  | NEW
  | RAISE
  | LOWER
  deriving (Show, Eq)

data DStarCell = Drivable { _status :: DStarStatus
                          , _nextCellInPath :: Maybe GridPoint
                          , _currentCost :: Int
                          }
               | Obstacle
  deriving (Show, Eq)               

type DStarState = A.Array GridPoint DStarCell
data DriveState = DriveState { _objects :: S.Set Msg.Object
                             , _dStarState :: DStarState
                             } deriving (Show, Eq)
makeLenses ''DriveState
makeLenses ''DStarCell
makePrisms ''DStarCell

initState P {_x=x, _y=y} =
  let hx = ceiling (x / 2)
      hy = ceiling (y / 2)
      bx = hx+1
      by = hy+1
      meat = [(P x y, Drivable NEW Nothing (x*y*2)) | x <- [-hx..hx], y <- [-hy, hy]]
      border = [(P x y, Obstacle) | (rx, ry) <- [([-bx..bx],[-by,by]), ([-bx,bx],[-by..by])]
                                  , x <- rx, y <- ry]
  in 
    DriveState { _objects = S.empty
               , _dStarState = A.array
                               (P (negate bx) (negate by), P bx by)
                               (meat ++ border)
               }
