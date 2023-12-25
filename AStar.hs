{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.PSQueue (Binding ((:->)), PSQ)
import Data.PSQueue qualified as PSQ
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import GHC.IsList (IsList (fromList))

data Cell = Empty | Wall | Start | Goal | Path deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Map Coord Cell

type Path = Seq Coord

parseCell :: Char -> Cell
parseCell c =
  case c of
    ' ' -> Empty
    '#' -> Wall
    'S' -> Start
    'G' -> Goal
    '.' -> Path
    _ -> error "invalid cell"

renderCell :: Cell -> Char
renderCell c =
  case c of
    Empty -> ' '
    Wall -> '#'
    Start -> 'S'
    Goal -> 'G'
    Path -> '.'

parseGrid :: String -> (Grid, ([Coord], [Coord]))
parseGrid str =
  let grid = [((x, y), parseCell c) | (y, line) <- zip [0 ..] (lines str), (x, c) <- zip [0 ..] line]
      startXYs = [xy | (xy, cell) <- grid, cell == Start]
      goalXYs = [xy | (xy, cell) <- grid, cell == Goal]
   in (fromList grid, (startXYs, goalXYs))

renderGrid :: Grid -> String
renderGrid grid =
  let ((maxX, maxY), _) = Map.findMax grid
   in unlines [[renderCell (grid Map.! (x, y)) | x <- [0 .. maxX]] | y <- [0 .. maxY]]

renderPath :: Grid -> [Coord] -> String
renderPath grid path =
  renderGrid $ Map.unionWith (\c1 c2 -> if c2 == Start || c2 == Goal then c2 else c1) (fromList (map (,Path) path)) grid

getNeighbours :: Grid -> Coord -> [Coord]
getNeighbours grid (x, y) =
  filter
    ( \xy' -> case Map.lookup xy' grid of
        Nothing -> False
        Just c -> c /= Wall
    )
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

pathBfs :: Grid -> Coord -> Coord -> Maybe Path
pathBfs grid start goal =
  pathBfs' [start] [(start, [])]
  where
    pathBfs' :: Seq Coord -> Map Coord Path -> Maybe Path
    pathBfs' Seq.Empty _ = Nothing
    pathBfs' (curCell :<| queue) paths =
      let curPath = paths Map.! curCell
       in if curCell == goal
            then Just $ curPath Seq.|> curCell
            else
              let unvisitedNeighbours = filter (`Map.notMember` paths) (getNeighbours grid curCell)
                  unvisitedNeighboursPaths = fromList $ map (,curPath Seq.|> curCell) unvisitedNeighbours
               in pathBfs'
                    (queue <> fromList unvisitedNeighbours)
                    (Map.union paths unvisitedNeighboursPaths)

pathDfs :: Grid -> Coord -> Coord -> Maybe Path
pathDfs grid start goal =
  pathDfs' [start] [(start, [])]
  where
    pathDfs' :: Seq Coord -> Map Coord Path -> Maybe Path
    pathDfs' Seq.Empty _ = Nothing
    pathDfs' (curCell :<| queue) paths =
      let curPath = paths Map.! curCell
       in if curCell == goal
            then Just $ curPath Seq.|> curCell
            else
              let unvisitedNeighbours = filter (`Map.notMember` paths) (getNeighbours grid curCell)
                  unvisitedNeighboursPaths = fromList $ map (,curPath Seq.|> curCell) unvisitedNeighbours
               in pathDfs'
                    (fromList unvisitedNeighbours <> queue)
                    (Map.union paths unvisitedNeighboursPaths)

pathAStar :: Grid -> Coord -> Coord -> Maybe Path
pathAStar grid start goal =
  pathAStar' (PSQ.singleton start (-1)) [(start, [])]
  where
    pathAStar' :: PSQ Coord Int -> Map Coord Path -> Maybe Path
    pathAStar' queue paths =
      case PSQ.minView queue of
        Nothing -> Nothing
        Just (curCell :-> _, queueTail) ->
          let curPath = paths Map.! curCell
           in if curCell == goal
                then Just $ curPath Seq.|> curCell
                else
                  let unvisitedNeighbours = filter (`Map.notMember` paths) (getNeighbours grid curCell)
                      updatedPaths = Map.union paths $ fromList $ map (,curPath Seq.|> curCell) unvisitedNeighbours
                      updatedQueue =
                        foldr
                          -- TODO why is this so damn slow
                          -- (\unvisitedNeighbour queue' -> PSQ.insert unvisitedNeighbour (length (updatedPaths Map.! unvisitedNeighbour) + manhattanDistance unvisitedNeighbour goal) queue')
                          (\unvisitedNeighbour queue' -> PSQ.insert unvisitedNeighbour (manhattanDistance unvisitedNeighbour goal) queue')
                          queueTail
                          unvisitedNeighbours
                   in pathAStar' updatedQueue updatedPaths

    manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  putStrLn "Grid:"
  -- putStrLn $ renderGrid g

  putStrLn "BFS Path:"
  case pathBfs g start goal of
    Nothing ->
      putStrLn "No path found"
    Just path -> do
      -- putStrLn $ renderPath g path
      putStrLn $ show (length path) ++ " steps\n"

  putStrLn "DFS Path:"
  case pathDfs g start goal of
    Nothing -> putStrLn "No path found"
    Just path -> do
      -- putStrLn $ renderPath g path
      putStrLn $ show (length path) ++ " steps\n"

  putStrLn "A* Path:"
  case pathAStar g start goal of
    Nothing -> putStrLn "No path found"
    Just path -> do
      -- putStrLn $ renderPath g path
      putStrLn $ show (length path) ++ " steps\n"
  where
    g = fromList [((x :: Int, y :: Int), Empty) | x <- [0 .. 1000], y <- [0 .. 1000]]
    start = (0, 0)
    goal = (999, 999)

-- (g, _) =
--   parseGrid
--     "\
--     \S    #     \n\
--     \     #     \n\
--     \           \n\
--     \     #     \n\
--     \     #     \n\
--     \## ##### ##\n\
--     \     #     \n\
--     \     #     \n\
--     \           \n\
--     \     #     \n\
--     \     #    G\n\
--     \"
