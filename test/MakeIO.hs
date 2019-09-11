module MakeIO where

import Control.Concurrent.MVar
import Data.Text (Text)
import Data.Text.IO

import Syntax (Binding)

writeVars :: Handle -> [(Text, Binding, Text)] -> IO ()
writeVars h vars = mapM_ writeMakeBinding vars where
  writeMakeBinding (x, v) = mapM_ (hPutStr h) [x, " ", binding, " ", v]

makeProcess :: CreateProcess
makeProcess = proc "make" ["-f", "-"]

runMake :: MVar -> [(Text, Binding, Text)] -> IO ExitCode
runMake mvar vars = withCreateProcess makeProcess $ \stdin stdout stderr ph -> do
  writeVars stdin vars
  waitForProcess ph
