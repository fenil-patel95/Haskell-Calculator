import Spec
import Assignment
import Check
import Grade
import Report

main :: IO ()
main = do
  (ans, _) <- hspecCheck test
  let r = grade assignment ans
  putStrLn ""
  printResults r
