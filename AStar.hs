{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.Array (Array)
import Data.Array qualified as Arr
import Data.Foldable (Foldable (foldl'))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.PSQueue (Binding ((:->)), PSQ)
import Data.PSQueue qualified as PSQ
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import GHC.IsList (IsList (fromList, toList))

data Cell = Empty | Wall | Start | Goal | Path deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Array Coord Cell

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

parseGrid :: String -> (Grid, (Coord, Coord))
parseGrid str =
  (Arr.array ((0, 0), (length (head (lines str)) - 1, length (lines str) - 1)) grid, (startXY, goalXY))
  where
    grid = [((x, y), parseCell c) | (y, line) <- zip [0 ..] (lines str), (x, c) <- zip [0 ..] line]
    -- no Start/Goal is an invalid Grid
    startXY = fst $ head $ filter ((==) Start . snd) grid
    goalXY = fst $ head $ filter ((==) Goal . snd) grid

renderGrid :: Grid -> String
renderGrid grid =
  unlines [[renderCell (grid Arr.! (x, y)) | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    (_, (maxX, maxY)) = Arr.bounds grid

renderPath :: Grid -> Path -> String
renderPath grid path =
  renderGrid $ grid Arr.// map (\co -> let gc = grid Arr.! co in if gc == Start || gc == Goal then (co, gc) else (co, Path)) (toList path)

getNeighbours :: Grid -> Coord -> [Coord]
getNeighbours grid (x, y) =
  filter
    (\(x', y') -> (x' >= lowerX && x' <= upperX && y' >= lowerY && y' <= upperY) && grid Arr.! (x', y') /= Wall)
    [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    ((lowerX, lowerY), (upperX, upperY)) = Arr.bounds grid

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
  pathAStar' (PSQ.singleton start (0, 0)) [(start, [])]
  where
    -- PSQ Coord (a* hueristic, (-1) * age)
    -- Need inverse of age as a tiebreaker so that newer entries with the same heuristic value come first
    pathAStar' :: PSQ Coord (Int, Int) -> Map Coord Path -> Maybe Path
    pathAStar' queue paths =
      case PSQ.minView queue of
        Nothing -> Nothing
        Just (curCell :-> (_, age), queueTail) ->
          let curPath = paths Map.! curCell
           in if curCell == goal
                then Just $ curPath Seq.|> curCell
                else
                  let unvisitedNeighbours = filter (`Map.notMember` paths) (getNeighbours grid curCell)
                      updatedPaths = Map.union paths $ fromList $ map (,curPath Seq.|> curCell) unvisitedNeighbours
                      updatedQueue =
                        foldl'
                          (\queue' unvisitedNeighbour -> PSQ.insert unvisitedNeighbour (length (updatedPaths Map.! curCell) + manhattanDistance unvisitedNeighbour goal, age - 1) queue')
                          queueTail
                          unvisitedNeighbours
                   in pathAStar' updatedQueue updatedPaths

    manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  putStrLn "Grid:"
  putStrLn $ renderGrid g

  putStrLn "\nBFS Path:"
  printPath pathBfs

  putStrLn "\nDFS Path:"
  printPath pathDfs

  putStrLn "\nA* Path:"
  printPath pathAStar

  putStrLn $ "\nA* huge grid (" ++ show hugeSize ++ "x" ++ show hugeSize ++ ") Path:"
  printHugePath pathAStar
  putStrLn "BFS and DFS would bog down here due to the size of the grid"
  putStrLn "This is near limit of A*, beyond this a smarter approach like HPA* is needed"
  where
    printPath pathFn =
      case pathFn g start goal of
        Nothing -> putStrLn "No path found"
        Just path -> do
          putStrLn $ renderPath g path
          putStrLn $ show (length path - 1) ++ " steps"

    printHugePath pathFn =
      case pathFn hugeG hugeStart hugeGoal of
        Nothing -> putStrLn "No path found"
        Just path -> do
          putStrLn $ show (length path - 1) ++ " steps"

    (g, (start, goal)) =
      parseGrid
        "\
        \S    #     \n\
        \     #     \n\
        \           \n\
        \     #     \n\
        \     #     \n\
        \## ##### ##\n\
        \     #     \n\
        \     #     \n\
        \           \n\
        \     #     \n\
        \     #    G"

    hugeSize = 1000
    hugeStart = (0, 0)
    hugeGoal = (hugeSize, hugeSize)
    hugeG = Arr.array ((0, 0), (hugeSize, hugeSize)) [((x, y), if (x, y) == hugeStart then Start else if (x, y) == hugeGoal then Goal else Empty) | x <- [0 .. hugeSize], y <- [0 .. hugeSize]]
