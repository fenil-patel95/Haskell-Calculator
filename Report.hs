{-# LANGUAGE DeriveGeneric #-}
module Report where

import Grade
import qualified Data.Map as M
import Control.Monad
import Data.Ratio
import Text.Printf

printResults :: (Outcome a, [Score]) -> IO ()
printResults (outcome, scores) = do
  printOutcome outcome
  printTotal outcome scores
  printExercises scores

printOutcome :: Outcome a -> IO ()
printOutcome = putStrLn . describeOutcome

printTotal :: Outcome a -> [Score] -> IO ()
printTotal (Right _) scores = do
  printf "Total: %s / %s\n"
    (showPoints $ earned scores) (showPoints $ possible scores)
printTotal (Left _) scores = do
  putStrLn "Total: **ERROR**"

printExercises :: [Score] -> IO ()
printExercises scores = forM_ scores (printExercise widest)
  where widest = maximum [ length name | (_, name, _, _) <- scores ]

printExercise :: Int -> Score -> IO ()
printExercise width (req, name, earned, total) = do
  let sigil = case req of
        Regular -> ">"
        Bonus   -> "+"
  let pad = replicate (width - length name) ' '
  printf "%s %s:%s %s / %s\n"
    sigil name pad (showPoints earned) (showPoints total)

showPoints :: Rational -> String
showPoints points
  | whole points = show (numerator points)
  | otherwise    = printf "%.2f" (fromRational points :: Double)

whole :: Rational -> Bool
whole r = denominator r == 1

