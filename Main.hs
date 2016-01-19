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

main = do
  [filename] <- getArgs
  s <- readFile filename
  case parseModule filename s of
    Left err -> putStrLn (show err)
    Right (Module sig trs) -> mapM_ print (otrsToTrs sig trs)
