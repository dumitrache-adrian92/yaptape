module Main (main) where

import Yaptape (runWithPaths)
import Paths_yaptape (getDataFileName)
import System.FilePath ((</>))

main :: IO ()
main = do
  staticDir <- getDataFileName ("static" </> "")
  migrationsDir <- getDataFileName ("db" </> "migrations" </> "")
  runWithPaths staticDir migrationsDir
