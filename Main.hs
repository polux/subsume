-- Copyright 2015 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License")--
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE OverloadedStrings #-}

import Datatypes
import Algo
import Parser
import System.Environment (getArgs)
import Criterion.Main
import System.Directory (getDirectoryContents, doesFileExist)
import System.Exit (exitFailure)
import Data.Either (partitionEithers)
import System.IO (hPutStrLn, stderr)
import Control.DeepSeq (deepseq)
import Control.Monad (filterM)

collect :: [Either a b] -> Either [a] [b]
collect es = select (partitionEithers es)
  where select (xs, ys) | null xs = Right ys
                        | otherwise = Left xs

getModules :: IO [(FilePath, Module)]
getModules = do
  examples <- getDirectoryContents "."
  files <- filterM doesFileExist examples
  filesContent <- mapM readFile files
  case collect (zipWith parseModule files filesContent) of
    Left errors -> do
      mapM_ (hPutStrLn stderr . show) errors
      exitFailure
    Right modules ->
      return (zip files modules)

makeBenchmarks :: [(FilePath, Module)] -> [Benchmark]
makeBenchmarks namedModules = map makeGroup namedModules
  where makeGroup (name, Module sig trs) = bgroup name
          [ bench "maranget" $ whnf (otrsToTrs Maranget sig) trs
          , bench "paper" $ whnf (otrsToTrs Paper sig) trs ]

main = do
  modules <- getModules
  modules `deepseq` defaultMain (makeBenchmarks modules)
