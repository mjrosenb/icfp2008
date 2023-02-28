{-# LANGUAGE TemplateHaskell #-}
module Point where
import Control.Lens
import qualified Data.Array as A
data Point a = P { _x :: a, _y :: a} deriving (Show, Read, Ord, Eq, A.Ix)
makeLenses ''Point

type GridPoint = Point Int
instance Num a => Num (Point a) where
  p + dp = p & x +~ (dp ^. x)
             & y +~ (dp ^. y)
  negate P {_x = x, _y = y} = P {_x = -x, _y = -y}
  

P x y $* s = P (x*s) (y*s)
P x y $/ s = P (x/s) (y/s)

ceiling' P {_x=x, _y=y} = P {_x=ceiling x, _y=ceiling y}
floor' P {_x=x, _y=y} = P {_x=floor x, _y=floor y}

ds2 p1 p2 = ((p1 ^. x - p2 ^. x)^2 + (p1 ^. y - p2 ^. y)^2)

toReal :: Real a => GridPoint -> Point a
toReal P {_x = x, _y = y} = P (fromIntegral x) (fromIntegral y)
