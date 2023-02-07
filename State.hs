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

type DStarState = A.Array GridPoint (DStarStatus, Maybe GridPoint)
data DriveState = DriveState { _objects :: S.Set Msg.Object
                             , _dStarState :: DStarState
                             } deriving (Show, Eq)
makeLenses ''DriveState
initState P {_x=x, _y=y} =
  let hx = ceiling (x / 2)
      hy = ceiling (y / 2)
  in 
    DriveState { _objects = S.empty
               , _dStarState = A.listArray (P (negate hx) (negate hy), P hx hy) (repeat (NEW, Nothing))
               }
