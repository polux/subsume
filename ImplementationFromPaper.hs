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

module ImplementationFromPaper (subsumes) where

import Data.List (intercalate)
import Datatypes
    ( Signature, Pattern(..), FunName, arity, functionsOfSameRange )
import qualified Data.Set as S ( fromList )

-- R is for "Rich" because it's patterns enriched with T and F
data RPattern = RCons FunName [RPattern]
              | RVar
              | RPlus [RPattern]
              | RMatch RPattern RPattern
              | RTrue
              | RFalse
  deriving (Eq, Ord)

instance Show RPattern where
  show (RCons f ps) = show f ++ "(" ++ intercalate ", " (map show ps) ++ ")"
  show RVar = "_"
  show (RPlus ps) = "(" ++ intercalate " (+) " (map show ps) ++ ")"
  show (RMatch p1 p2) = "(" ++ show p1 ++ " <<? " ++ show p2 ++ ")"
  show RTrue = "T"
  show RFalse = "F"

isRTrue :: RPattern -> Bool
isRTrue RTrue = True
isRTrue _ = False

isRFalse :: RPattern -> Bool
isRFalse RFalse = True
isRFalse _ = False

rCons :: FunName -> [RPattern] -> RPattern
rCons f ps | all isRTrue ps = RTrue
           | any isRFalse ps = RFalse
           | otherwise = RCons f ps

rPlus :: [RPattern] -> RPattern
rPlus ps = undefined

(<<?) :: RPattern -> RPattern -> RPattern
(RCons f ps) <<? (RCons g qs)
  | f == g = if null ps then RTrue else rCons f (zipWith (<<?) ps qs)
  | otherwise = RFalse
RVar <<? _ = RTrue



subsumes :: Signature -> [Pattern] -> Pattern -> Bool
subsumes = undefined
