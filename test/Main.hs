module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

includeDirs :: [String]
includeDirs = []

doctestWithIncludeDirs :: [String] -> IO ()
doctestWithIncludeDirs fs = doctest (map ((++) "-I") includeDirs ++ fs)

main :: IO ()
main = do
  glob "src/**/*.hs" >>= doctestWithIncludeDirs

