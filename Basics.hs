{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where

import ProblemState
import Data.List
import Data.Maybe
{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)
instance Num (Int,Int) where
    (a,b) + (c,d) = (a+c,b+d)

type Behavior = Position -> Game -> Entity

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)
revDirection :: Direction -> Direction
revDirection direction = case direction of  West -> East
                                            East -> West
                                            North -> South
                                            South -> North

data Terrain = Obstacle | Gateway{linkedPos::Position} | Empty deriving (Ord)
data Entity = Hunter | Target {position::Position,behavior::Behavior} | None 

instance Eq Terrain where
    (==) (Obstacle) (Obstacle) = True
    (==) (Gateway _) (Gateway _) =True
    (==) (Empty) (Empty) = True
    (==) _ _ = False 
instance Eq Entity where
    (==) (Hunter) (Hunter) = True
    (==) (Target _ _)  (Target _ _) = True
    (==) (None) (None) = True
    (==) _ _ = False

instance Ord Entity where
    (<=) _ _ = True

type Cell = (Terrain,Entity)
data Game = Game {linesGame::[[Cell]],hunterPos::Position} deriving (Eq, Ord)



gameAsString :: Game -> String
gameAsString game = intercalate "\n" [ concatMap cellAsString list | list <- (linesGame game) ]

isObstacle :: Terrain -> Bool
isObstacle Obstacle = True
isObstacle _ = False

isTarget :: Entity -> Bool
isTarget (Target _ _) = True
isTarget _ = False

isGateWay :: Terrain -> Bool
isGateWay (Gateway _) = True
isGateWay _ = False

cellAsString :: Cell -> String
cellAsString (terrain,None) = case terrain of   Obstacle -> "@"
                                                Empty -> " "
                                                (Gateway _) -> "#"

cellAsString (_ , entity) = case entity of  Hunter -> "!"
                                            (Target _ _ ) -> "*"


instance Show Game where
    show game= gameAsString game 

emptyGame :: Int -> Int -> Game
emptyGame rows columns = addHunter (1,1) newgame
    where
        newgame = Game [line i | i <-[1..rows] ] (-1,-1) 
            where
                line x
                    |   x==1 || x==rows = take columns (repeat (Obstacle,None))
                    | otherwise = (Obstacle,None) : (take (columns-2) (repeat  (Empty,None))) ++ ((Obstacle,None) : [])

inBounds :: Position -> Game -> Bool
inBounds (row,column) game = column >= 0 && row >= 0 && row <= (length (linesGame game)-1)  && column <= (length (head $ linesGame game)-1)


addTerrain :: Position -> Game -> Terrain -> Game
addTerrain pos game terrain = addCell pos game (terrain,getEntity pos game)
addEntity :: Position -> Game -> Entity -> Game
addEntity pos game entity = addCell pos game (getTerrain pos game,entity)
getEntity :: Position -> Game -> Entity
getEntity pos game  = snd $ getCell pos game
getTerrain :: Position -> Game -> Terrain
getTerrain pos game = fst $ getCell pos game
addCell :: Position -> Game -> Cell -> Game
addCell pos game cell = Game newlinesGame $ hunterPos game
        where
            newlinesGame = [insertCell (fst pair) (snd pair) | pair <- (zip [0..length (linesGame game) - 1] (linesGame game)) ]
                where
                    row   = fst pos
                    column = snd pos
                    insertCell currentRow line
                        | currentRow == row = take column line ++ (cell : (drop (column+1) line))
                        | otherwise = line
getCell :: Position -> Game -> Cell
getCell pos game = (linesGame game) !! row !! column
    where row    = fst pos
          column = snd pos

addHunter :: Position -> Game -> Game
addHunter pos game
    | (not $ inBounds pos game) || terrain == Obstacle = game
    | otherwise = Game (linesGame updatedGame) newPos
        where   terrain = getTerrain pos game
                newPos
                    | isGateWay terrain = linkedPos terrain
                    | otherwise = pos
                updatedGame = addEntity newPos (removeEntity (hunterPos game) game) Hunter

addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior pos game
    | (not $ inBounds pos game) || terrain == Obstacle = game
    | otherwise = addCell pos game (terrain,(Target pos behavior))
        where   terrain = getTerrain pos game
placeTarget :: Entity -> Game -> Game
placeTarget target game = addTarget (behavior target) (position target)  game

addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1,pos2) game = addTerrain pos1 (addTerrain pos2 game (Gateway pos1)) (Gateway pos2)

addObstacle :: Position -> Game -> Game
addObstacle pos game = addTerrain pos game Obstacle

removeEntity :: Position -> Game -> Game
removeEntity pos game = addCell pos game (terrain,None) where   
    terrain = getTerrain pos game

attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    |  or [not $ inBounds pos game ,  isObstacle terrain ] = Nothing
    | isGateWay terrain = Just $ linkedPos terrain
    | otherwise = Just pos
        where
            terrain = getTerrain pos game

directionToOffset :: Direction -> Position
directionToOffset direction = case direction of West  -> (0,-1)
                                                East  -> (0,1)
                                                North -> (-1,0)
                                                South -> (1,0)

goDirection :: Direction -> Behavior
goDirection direction = newBehaviour
    where 
        newBehaviour pos game = target
            where
                newPos = pos + directionToOffset direction
                move   = attemptMove newPos game
                target
                    | isJust move = Target (fromJust move) (goDirection direction)
                    | otherwise = Target pos (goDirection direction) 

goDirectionWithSpare :: Direction -> Behavior ->Behavior
goDirectionWithSpare direction spareBehaviour = newBehaviour
    where 
        newBehaviour pos game = target
            where
                newPos = pos + (directionToOffset direction)
                move   = attemptMove newPos game
                target
                    | isJust move = Target (fromJust move) (goDirectionWithSpare direction spareBehaviour)
                    | otherwise = spareBehaviour pos game



goEast :: Behavior
goEast = goDirection East

goWest :: Behavior
goWest = goDirection West

goNorth :: Behavior
goNorth = goDirection North

goSouth :: Behavior
goSouth = goDirection South


bounce :: Int -> Behavior
bounce 1 = goDirectionWithSpare South (bounce (-1))
bounce (-1) = goDirectionWithSpare North (bounce 1)

moveTargets :: Game -> Game
moveTargets game = foldr placeTarget filteredGame updated
            where
                collected =collectTargets game
                updated = map (updateTarget game) collected
                filteredGame = foldr (\target game -> removeEntity (position target) game) game collected

collectTargets :: Game -> [Entity]
collectTargets game = filter isTarget (map (\(terrain,entity) -> entity)  (concat $ linesGame game))

updateTarget :: Game -> Entity -> Entity
updateTarget game target = (behavior target) (position target) game

adjacentPositions :: Game -> Position -> [Position]
adjacentPositions game (x,y) = [ pos| pos <- [(x-1,y),(x+1,y),(x,y+1),(x,y-1)], inBounds pos game ]

adjacentPositionsUnsafe :: Position -> [Position]
adjacentPositionsUnsafe (x,y) = [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]

isTargetKilled :: Position -> Entity -> Bool
isTargetKilled pos entity = elem pos adjacent
            where
                    adjacent= adjacentPositionsUnsafe (position entity)

killTarget :: Position -> Game -> Game
killTarget pos game
    | isTarget $ snd (getCell pos game) = removeEntity pos game
    | otherwise = game

eliminateTargets :: Game -> Game
eliminateTargets game = foldr killTarget game adjacent 
    where
                adjacent= adjacentPositions  game $ hunterPos game


advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction bool game 
    | bool =  eliminateTargets $ moveTargets $ eliminateTargets gameMovedHunter
    | otherwise =gameMovedHunter
            where
                gameMovedHunter = addHunter (directionToOffset direction + (hunterPos game)) game

areTargetsLeft :: Game -> Bool
areTargetsLeft game = collectTargets game == []


instance ProblemState Game Direction where
   
    successors game = map advance [West,East,North,South]
            where advance = \direction -> (direction,advanceGameState direction False game)
   
    isGoal game = not ((filter isTarget (map (\pos -> getEntity pos game) $ adjacentPositions game (hunterPos game) )) == [])

    h game = foldr min 0 distances 
        where
            distances=map (hEuclidean (hunterPos game)) (map position (collectTargets game)) 

hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

