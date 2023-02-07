{-# LANGUAGE TemplateHaskell #-}
module Point where
import Control.Lens
import qualified Data.Array as A
data Point a = P { _x :: a, _y :: a} deriving (Show, Read, Ord, Eq, A.Ix)
makeLenses ''Point

type GridPoint = Point Int

  
