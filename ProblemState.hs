{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ProblemState where

class ProblemState s a | s -> a where
    successors :: s -> [(a, s)]
    isGoal :: s -> Bool
    h :: s -> Float