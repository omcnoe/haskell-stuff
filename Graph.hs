{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.IsList (IsList (fromList, toList))

type EdgeList = Map Int [Int]

newtype Node = Node (Int, [Node])

edgeListToGraph :: EdgeList -> Maybe Node
edgeListToGraph edgeList = do
  (nodeId, _) <- listToMaybe $ toList edgeList
  edgeListToGraphFrom edgeList nodeId
  where
    edgeListToGraphFrom edgeList' nodeId' = do
      neighbours <- Map.lookup nodeId' edgeList'
      return (Node (nodeId', mapMaybe (edgeListToGraphFrom edgeList') neighbours))

graphToEdgeListBf :: Node -> EdgeList
graphToEdgeListBf graph =
  bf [graph] Set.empty Map.empty
  where
    bf :: Seq Node -> Set Int -> EdgeList -> EdgeList
    bf Seq.Empty _ edgeList = edgeList
    bf ((Node (nodeId, neighbours)) :<| remainingToVisit) visited edgeList =
      if nodeId `Set.member` visited
        then bf remainingToVisit visited edgeList
        else
          bf
            (remainingToVisit <> fromList neighbours)
            (Set.insert nodeId visited)
            (Map.insert nodeId (map (\(Node (neighbourNodeId, _)) -> neighbourNodeId) neighbours) edgeList)

graphToEdgeListDf :: Node -> EdgeList
graphToEdgeListDf graph =
  df [graph] Set.empty Map.empty
  where
    df :: Seq Node -> Set Int -> EdgeList -> EdgeList
    df Seq.Empty _ edgeList = edgeList
    df ((Node (nodeId, neighbours)) :<| remainingToVisit) visited edgeList =
      if nodeId `Set.member` visited
        then df remainingToVisit visited edgeList
        else
          df
            (fromList neighbours <> remainingToVisit)
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
    roundTripBf maybeEdgeList = maybeEdgeList >>= edgeListToGraph <&> graphToEdgeListBf
    roundTripDf maybeEdgeList = maybeEdgeList >>= edgeListToGraph <&> graphToEdgeListDf
    input :: EdgeList =
      fromList $ map createEdges [1 .. 10_000]
      where
        createEdges i = (i, [j | j <- [i .. i + 50], j /= i && j <= 10_000])
