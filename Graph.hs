{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
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
  bf [graph] Map.empty
  where
    bf :: Seq Node -> EdgeList -> EdgeList
    bf Seq.Empty edgeList = edgeList
    bf ((Node (nodeId, neighbours)) :<| remainingToVisit) edgeList =
      if nodeId `Map.member` edgeList
        then bf remainingToVisit edgeList
        else
          bf
            (remainingToVisit <> fromList neighbours)
            (Map.insert nodeId (map (\(Node (neighbourNodeId, _)) -> neighbourNodeId) neighbours) edgeList)

graphToEdgeListDf :: Node -> EdgeList
graphToEdgeListDf graph =
  df [graph] Map.empty
  where
    df :: Seq Node -> EdgeList -> EdgeList
    df Seq.Empty edgeList = edgeList
    df ((Node (nodeId, neighbours)) :<| remainingToVisit) edgeList =
      if nodeId `Map.member` edgeList
        then df remainingToVisit edgeList
        else
          df
            (fromList neighbours <> remainingToVisit)
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
