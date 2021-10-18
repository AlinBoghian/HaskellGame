module Terrain where

import Basics
loadGame :: FilePath -> [Behavior] -> [(Int, Int)] -> IO Game
loadGame path behaviors gatewayIndexPairs = do
    (nRows, nColumns, obstacles, hunter, targets, gateways) <- loadTerrain path
    return $ createGame (nRows, nColumns, obstacles, hunter, zip behaviors targets, fromIndexPairs gatewayIndexPairs gateways)
  where 
    fromIndexPairs :: [(Int, Int)] -> [Position] -> [(Position, Position)]
    fromIndexPairs indexPairs gateways = map (\(i, j) -> (gateways !! i, gateways !! j)) indexPairs

createGame :: (Int, Int, [Position], Position, [(Behavior, Position)], [(Position, Position)]) -> Game
createGame (nRows, nColumns, obstacles, hunter, targs, gates) = addedHunter
  where 
    emptyG = emptyGame nRows nColumns
    addedObstacles = foldl (flip addObstacle) emptyG obstacles
    addedGateways = foldl (flip addGateway) addedObstacles gates
    addedTargets = foldl (flip (uncurry addTarget)) addedGateways targs
    addedHunter = addHunter hunter addedTargets

loadTerrain :: FilePath -> IO (Int, Int, [Position], Position, [Position], [Position])
loadTerrain path = fmap processTerrain $ readFile path

processTerrain :: String -> (Int, Int, [Position], Position, [Position], [Position])
processTerrain contents = (nRows, nColumns, obstacles, hunter, targets, gateways)
  where
    rows = lines contents
    nRows = length rows
    nColumns = length $ head rows
    indexedList = concat $ zipWith (\i row -> zipWith (\j c -> ((i, j), c)) [0..] row) [0..] rows
    extract chr = map fst $ filter ((== chr) . snd) indexedList
    obstacles = extract '@'
    hunter = head $ extract '!'
    targets = extract '*'
    gateways = extract '#'
