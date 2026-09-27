module Main (main) where

import Yaptape (runWithStatic)
import Paths_yaptape (getDataFileName)
import System.FilePath ((</>))

main :: IO ()
main = getDataFileName ("static" </> "") >>= runWithStatic
