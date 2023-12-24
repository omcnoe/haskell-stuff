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

graphToEdgeListBf :: Node -> EdgeList
graphToEdgeListBf graph =
  bf [graph] Set.empty Map.empty
  where
    bf :: [Node] -> Set Int -> EdgeList -> EdgeList
    bf [] _ edgeList = edgeList
    bf (Node (nodeId, neighbours) : nodes) visited edgeList =
      if nodeId `Set.member` visited
        then bf nodes visited edgeList
        else
          bf
            (nodes ++ neighbours)
            (Set.insert nodeId visited)
            (Map.insert nodeId (map (\(Node (neighbourNodeId, _)) -> neighbourNodeId) neighbours) edgeList)

graphToEdgeListDf :: Node -> EdgeList
graphToEdgeListDf graph =
  df [graph] Set.empty Map.empty
  where
    df :: [Node] -> Set Int -> EdgeList -> EdgeList
    df [] _ edgeList = edgeList
    df (Node (nodeId, neighbours) : nodes) visited edgeList =
      if nodeId `Set.member` visited
        then df nodes visited edgeList
        else
          df
            (neighbours ++ nodes)
            (Set.insert nodeId visited)
            (Map.insert nodeId (map (\(Node (neighbourNodeId, _)) -> neighbourNodeId) neighbours) edgeList)

main :: IO ()
main = do
  putStrLn "Input Edge List:"
  print $ Just input

  putStrLn "Round Trip BF:"
  let oneRoundTripBf = roundTripBf $ Just input
  print oneRoundTripBf

  putStrLn "Round Trip DF:"
  let oneRoundTripDf = roundTripDf $ Just input
  print oneRoundTripDf

  putStrLn "Round Trips BF + DF:"
  let twoRoundTripsBfDf = roundTripBf $ roundTripDf $ Just input
  print twoRoundTripsBfDf

  putStrLn "Isomorphic:"
  print $ oneRoundTripBf == oneRoundTripDf && oneRoundTripBf == twoRoundTripsBfDf
  where
    roundTripDf maybeEdgeList = maybeEdgeList >>= edgeListToGraph <&> graphToEdgeListBf
    roundTripBf maybeEdgeList = maybeEdgeList >>= edgeListToGraph <&> graphToEdgeListDf
    input :: EdgeList =
      Map.fromList
        [ (1, [2, 3]),
          (2, [1, 3]),
          (3, [1, 2])
        ]
