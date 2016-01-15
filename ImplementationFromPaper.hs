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

{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, OverloadedStrings #-}

module ImplementationFromPaper (subsumes) where

import Data.List ( intercalate, tails, inits )
import Datatypes
    ( Signature,
      Pattern(..),
      FunName(..),
      arity,
      functionsOfSameRange )
import Data.Data ( Data, Typeable )
import Examples ( test_sig, fork, tip )

deriving instance Data FunName

-- R is for "Rich" because it's patterns enriched with constructors from the algo
data RPattern = RCons FunName [RPattern]
              | RVar
              | RPlus [RPattern]
              | RBottom
              | Stuck
  deriving (Eq, Ord)

instance Show RPattern where
  show (RCons f ps) = show f ++ "(" ++ intercalate ", " (map show ps) ++ ")"
  show RVar = "_"
  show (RPlus ps) = "(" ++ intercalate " + " (map show ps) ++ ")"
  show RBottom = "⊥"
  show Stuck = "<stuck>"

isRBottom :: RPattern -> Bool
isRBottom RBottom = True
isRBottom _ = False

-- interleave abc ABC = Abc, aBc, abC
interleave :: [a] -> [a] -> [[a]]
interleave [] [] = []
interleave xs ys = zipWith3 glue (inits xs) ys (tails (tail xs))
  where glue xs x ys = (xs++x:ys)

trs :: Signature -> RPattern -> [RPattern] -> RPattern
trs sig p ps = foldl (\\) p ps
  where
    rCons f ps | any isRBottom ps = RBottom
               | otherwise = RCons f ps

    rPlus ps = if null ps' then RBottom else RPlus ps'
      where ps' = filter (not . isRBottom) ps

    u \\ RVar = RBottom
    u \\ RBottom = u
    RVar \\ p@(RCons g ps) = rPlus [pattern f \\ p | f <- functionsOfSameRange sig g]
    RBottom \\ RCons _ _ = RBottom
    RCons f ps \\ RCons g qs
        | f /= g || someUnchanged = rCons f ps
        | otherwise = rPlus [rCons f ps' | ps' <- interleave ps pqs]
      where pqs = zipWith combine ps qs
            combine p q = (p \\ q)
            someUnchanged = or (zipWith (==) ps pqs)
    RPlus qs \\ RCons f ps = rPlus [q \\ rCons f ps | q <- qs]
    x \\ y = Stuck

    pattern f = RCons f (replicate (arity sig f) RVar)

convert :: Pattern -> RPattern
convert (PCons f ps) = RCons f (map convert ps)
convert PVar = RVar

subsumes :: Signature -> [Pattern] -> Pattern -> Bool
subsumes sig [] p = False
subsumes sig ps p = trs sig (convert p) (map convert ps) == RBottom

