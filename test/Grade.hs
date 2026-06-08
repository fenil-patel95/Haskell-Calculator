module Grade where

import qualified Data.Map as M
import Control.Monad.RWS
import Control.Monad.Except

data Checked = Pass | Fail
  deriving (Eq, Show, Bounded, Enum)

type Trail = [String]

type Answers = M.Map Trail Checked

type Context = (Trail, Answers)

data Requirement = Regular | Bonus
  deriving (Eq, Show, Bounded, Enum)

type Score = (Requirement, String, Rational, Rational)

type Grader = ExceptT Trail (RWS Context [Score] ())

type Outcome a = Either Trail a

grade :: Grader a -> Answers -> (Outcome a, [Score])
grade m ans = (val, scores)
  where (val, (), scores) = runRWS (runExceptT m) ([], ans) ()

describeOutcome :: Outcome a -> String
describeOutcome (Right _) = "All exercises checked successfully."
describeOutcome (Left trail) =
  unlines $ "Failed to find the result of the following exercise:"
          : zipWith (++) indents trail
  where indents = iterate ("  " ++) "  "

earned, possible, regular, extra, credit :: [Score] -> Rational

earned   scores = sum [ i | (_,       _, i, _) <- scores ]
possible scores = sum [ n | (Regular, _, _, n) <- scores ]
regular  scores = sum [ i | (Regular, _, i, _) <- scores ]
extra    scores = sum [ n | (Bonus,   _, _, n) <- scores ]
credit   scores = sum [ i | (Bonus,   _, i, _) <- scores ]



within :: String -> Context -> Context
within s (trail, ans) = (trail ++ [s], ans)

outOf :: Rational -> Rational -> Score
i `outOf` n = (Regular, "", i, n)

passed :: Score
passed = 1 `outOf` 1

failed :: Score
failed = 0 `outOf` 1

section :: String -> Grader a -> Grader a
section name m = local (within name) m

exercise :: String -> Grader a -> Grader a
exercise name m = censor summation $ local (within name) m
  where summation scores = required scores ++ bonus scores
        required scores = [(Regular, name, regular scores, possible scores)]
        bonus scores
          | c > 0      = [(Bonus, name, c, extra scores)]
          | otherwise  = []
          where c = credit scores

bonus :: Grader a -> Grader a
bonus m = censor optional m
  where optional scores = [ (Bonus, ex, i, n) | (_, ex, i, n) <- scores, i /= 0 ]

part :: String -> Grader a -> Grader a
part name m = section name m

check :: Rational -> Grader a -> Grader a
check n m = pass $ do
  (a, scores) <- listen m
  if and [ got == max | (Regular, _, got, max) <- scores ]
    then return (a, const [n `outOf` n])
    else return (a, const [0 `outOf` n])

split :: Rational -> Grader a -> Grader a
split total m = pass $ do
  (a, scores) <- listen m
  let count = length scores
  let scale = total / toRational count
  let mult (req, name, i, n) = (req, name, scale*i, scale*n)
  return (a, fmap mult)

must :: String -> Grader ()
must desc = local (within desc) $ do
  (trail, ans) <- ask
  case M.lookup trail ans of
    Just Pass -> tell [passed]
    Just Fail -> tell [failed]
    Nothing   -> throwError trail
