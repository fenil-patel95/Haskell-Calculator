module Check where

import Grade
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Api.Formatters.V2
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M

hspecCheck :: Spec -> IO (Answers, Summary)
hspecCheck test = do
  ref <- newIORef M.empty
  let config = useFormatter ("checking", checking ref) defaultConfig
  summary <- hspecWithResult config test
  answers <- readIORef ref
  return (answers, summary)

checking :: IORef Answers -> Formatter
checking ref = checkingOn ref checks

checkingOn :: IORef Answers -> Formatter -> Formatter
checkingOn ref form = form {
  formatterItemDone = \path@(cxt, thing) item -> do
      let trail = cxt ++ [thing]
      let check = checkResult $ itemResult item
      liftIO $ modifyIORef ref $ M.insert trail check
      formatterItemDone form path item
  }

checkResult :: Result -> Checked
checkResult Success = Pass
checkResult _       = Fail
