{-# LANGUAGE TemplateHaskell #-}
module State where
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SP
import qualified Message as Msg
import qualified Data.Set as S
import qualified Data.Array as A
import Control.Lens
import Point
import Control.Monad.State.Lazy
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
data DriveState = DriveState { _seenObjects :: S.Set Msg.Object
                             , _nodeMap :: NodeMap Point.GridPoint
                             , _graph :: Gr Point.GridPoint Double
                             } deriving (Show, Eq)
makeLenses ''DriveState
makeLenses ''DStarCell
makePrisms ''DStarCell

deltas = [(dp, sqrt (fromIntegral (dx^2 + dy^2))) |
          dx <- [-1..1],
          dy <- [-1..1],
          let dp = P dx dy,
          dp /= P 0 0 ]

initState bounds =
  let br = ceiling' (bounds $/ 2)
      tl = negate br
      nodes = A.range (tl, br)
      (gNodes, nodeMap) = mkNodes new nodes
      gEdges = [e | n <- nodes, (dp, dl) <- deltas, Just e <- [mkEdge nodeMap  (n, n + dp, dl)]]
      gr = mkGraph gNodes gEdges
  in 
    DriveState { _seenObjects = S.empty
               , _nodeMap = nodeMap
               , _graph = gr 
               }

expandObject :: Msg.Object -> [GridPoint]
expandObject Msg.Martian {} = []
expandObject o = let p = o ^. Msg.static . Msg.center
                     rad = (o ^. Msg.static . Msg.r) + 0.5 -- TODO: don't hardcode the radius of the robot
                     rad2 = rad^2
                     diag = P rad rad
                     tl = floor' (p - diag)
                     br = ceiling' (p + diag)
                 in [gp | gp <- A.range (tl, br), ds2 (toReal gp) p < rad2]
                    
addObjects' :: [Msg.Object] -> State DriveState [Msg.Object]
addObjects' [] = return []
addObjects' (h:t) = do
  isOld <- seenObjects . contains h <<.= True
  rest <- addObjects' t
  return (if isOld
          then h:rest
          else rest)

removeGP :: GridPoint -> State DriveState ()
removeGP gp = do
  nm <- use nodeMap
  graph %= delMapNode nm gp
  
addObjects :: [Msg.Object] -> State DriveState [Msg.Object]
addObjects objs = do
  new <- addObjects' objs
  let gps = concatMap expandObject new
  mapM_ removeGP gps
  return new

sp state start end =
  let start' = fst $ mkNode_ (state ^. nodeMap) start
      end' =  fst $ mkNode_ (state ^. nodeMap) end
  in Data.Graph.Inductive.Query.SP.sp start' end' (state ^. graph)
