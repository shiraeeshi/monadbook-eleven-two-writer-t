module Main where

import Lib
import Control.Monad (forM)

main :: IO ()
main = do
  let state = AppState "10" "2"
      resultWriterT = divParsedXY
      (result, logs) = runReader (runWriterT resultWriterT) state
  forM logs putStrLn
  putStrLn $ "result: " ++ (show result)
