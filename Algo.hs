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

module Algo (otrsToTrs) where

import Debug.Trace
import Data.List ( intercalate, tails, inits )
import Datatypes
import Signature

isBottom :: Term -> Bool
isBottom Bottom = True
isBottom _ = False

-- interleave abc ABC = Abc, aBc, abC
interleave :: [a] -> [a] -> [[a]]
interleave [] [] = []
interleave xs ys = zipWith3 glue (inits xs) ys (tails (tail xs))
  where glue xs x ys = xs ++ x : ys

difference :: Signature -> Term -> [Term] -> Term
difference sig p ps = foldl (\\) p ps
  where
    appl f ps | any isBottom ps = Bottom
              | otherwise = Appl f ps

    plus Bottom u = u
    plus t Bottom = t
    plus t u = Plus t u

    plus' = foldr plus Bottom

    alias x Bottom = Bottom
    alias x t = Alias x t

    u \\ (Var _) = Bottom
    u \\ Bottom = u
    (Var x) \\ p@(Appl g ps) = alias x (plus' [pattern f \\ p | f <- fs])
      where fs = functionsOfSameRange sig g
            pattern f = Appl f (replicate (arity sig f) (Var "_"))
    Bottom \\ Appl _ _ = Bottom
    Appl f ps \\ Appl g qs
        | f /= g || someUnchanged = appl f ps
        | otherwise = plus' [appl f ps' | ps' <- interleave ps pqs]
      where pqs = zipWith (\\) ps qs
            someUnchanged = or (zipWith (==) ps pqs)
    Plus q1 q2 \\ p@(Appl _ _) = plus (q1 \\ p) (q2 \\ p)
    Alias x p1 \\ p2 = alias x (p1 \\ p2)
    p1 \\ Alias x p2 = p1 \\ p2

subsumes :: Signature -> [Term] -> Term -> Bool
subsumes sig [] p = False
subsumes sig ps p = difference sig p ps == Bottom

removePlusses :: Term -> [Term]
removePlusses = undefined

minimize :: Signature -> [Term] -> [Term]
minimize = undefined

removeAliases :: Rule -> Rule
removeAliases = undefined

otrsToAdditiveTrs :: Signature -> [Rule] -> [Rule]
otrsToAdditiveTrs = undefined

aliasedTrsToTrs :: [Rule] -> [Rule]
aliasedTrsToTrs = map removeAliases

additiveTrsToAliasedTrs :: Signature -> [Rule] -> [Rule]
additiveTrsToAliasedTrs sig rules = concatMap transform rules
  where transform (Rule lhs rhs) = map (Rule lhs) (minimize sig (removePlusses rhs))

otrsToTrs :: Signature -> [Rule] -> [Rule]
otrsToTrs sig = aliasedTrsToTrs . additiveTrsToAliasedTrs sig . otrsToAdditiveTrs sig


