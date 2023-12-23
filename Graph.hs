module Main (main) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type EdgeList = Map Int [Int]

newtype Node = Node (Int, [Node])

edgeListToGraph :: EdgeList -> Maybe Node
edgeListToGraph edgeList = do
  (nodeId, _) <- listToMaybe $ Map.toList edgeList
  edgeListToGraphFrom edgeList nodeId
  where
    edgeListToGraphFrom edgeList' nodeId' = do
      neighbours <- Map.lookup nodeId' edgeList'
      return (Node (nodeId', mapMaybe (edgeListToGraphFrom edgeList') neighbours))

graphToEdgeList :: Node -> EdgeList
graphToEdgeList graph =
  bfs [graph] Set.empty Map.empty
  where
    bfs :: [Node] -> Set Int -> EdgeList -> EdgeList
    bfs [] _ edgeList = edgeList
    bfs (Node (nodeId, neighbours) : nodes) visited edgeList =
      if nodeId `Set.member` visited
        then bfs nodes visited edgeList
        else
          bfs
            (nodes ++ neighbours)
            (Set.insert nodeId visited)
            (Map.insert nodeId (map (\(Node (neighbourNodeId, _)) -> neighbourNodeId) neighbours) edgeList)

main :: IO ()
main = do
  putStrLn "Input Edge List:"
  print input

  putStrLn "One Round Trip:"
  let oneRoundTrip = roundTrip $ Just input
  print oneRoundTrip

  putStrLn "Two Round Trips:"
  let twoRoundTrips = roundTrip $ roundTrip $ Just input
  print twoRoundTrips

  putStrLn "Isomorphic:"
  print $ oneRoundTrip == twoRoundTrips
  where
    roundTrip maybeEdgeList = (maybeEdgeList >>= edgeListToGraph) <&> graphToEdgeList
    input :: EdgeList =
      Map.fromList
        [ (1, [2, 3]),
          (2, [1, 3]),
          (3, [1, 2])
        ]
