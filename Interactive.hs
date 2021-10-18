
module Interactive where

import Basics
import Terrain
import ProblemState
import Search

interactive :: IO Game -> IO ()
interactive loader = loader >>= loop
  where
    loop :: Game -> IO ()
    loop game = do
        print game
        putStr "Choose hunter's move (w/a/s/d): "
        answer <- getLine
        let direction = case answer of
                            "w" -> Just North
                            "s" -> Just South
                            "d" -> Just East
                            "a" -> Just West
                            _   -> Nothing
        let newGame = case direction of
                          Just dir -> advanceGameState dir True game
                          _        -> game
        loop newGame

hunt :: Bool -> IO Game -> IO ()
hunt stepwise loader = loader >>= loop 0
  where
    loop :: Int -> Game -> IO ()
    loop step game = do
        putStrLn $ "\nStep " ++ show step ++ ": "
        print game
        if areTargetsLeft game
            then do
                if stepwise then putStr "Press ENTER" >> getLine >> return () else return ()
                let initialNode = createStateSpace game
                let goalNode = astar initialNode
                let (direction, _) : _ = extractPath goalNode
                loop (step + 1) $ advanceGameState direction True game
            else
                putStrLn "\nNo targets left"

bonusHunt :: Bool -> IO Game -> IO ()
bonusHunt stepwise loader = loader >>= loop 0
  where
    loop :: Int -> Game -> IO ()
    loop step game = do
        putStrLn $ "\nStep " ++ show step ++ ": "
        print game
        if areTargetsLeft game
            then do
                if stepwise then putStr "Press ENTER" >> getLine >> return () else return ()
                let initialNode = createStateSpace $ BonusGame game
                let goalNode = astar initialNode
                let (direction, _) : _ = extractPath goalNode
                loop (step + 1) $ advanceGameState direction True game
            else
                putStrLn "\nNo targets left"
