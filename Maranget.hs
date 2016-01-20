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

module Maranget (subsumes) where

import Debug.Trace
import Datatypes
import Signature
import qualified Data.Set as S ( fromList )

type Vector = [Term]
type Matrix = [Vector]

{- Helper Functions -}

sameSet :: Ord a => [a] -> [a] -> Bool
sameSet xs ys = S.fromList xs == S.fromList ys

startsWithVar :: Vector -> Bool
startsWithVar (Var _ : _) = True
startsWithVar _ = False

startsWithCons :: Vector -> Bool
startsWithCons (Appl _ _ : _) = True
startsWithCons _ = False

headConstructor :: Vector -> FunName
headConstructor (Appl c _ : _) = c

headConstructors :: Matrix -> [FunName]
headConstructors m = map headConstructor (filter startsWithCons m)

dropAliases (Appl f ts) = Appl f (map dropAliases ts)
dropAliases v@(Var x) = v
dropAliases (Alias x t) = dropAliases t

{- Algo -}

subsumes :: Signature -> [Term] -> Term -> Bool
subsumes sig ps p = useless [[dropAliases q] | q <- ps] [dropAliases p]

  where

    --useless m v | trace (show m ++ " " ++ show v) False = undefined
    useless [] _ = False
    useless _ [] = True
    useless m v@(Appl c _ : _) = useless (specializeM c m) (specializeV c v)
    useless m v@(Var _ : ps)
      | complete = and [useless (specializeM c m) (specializeV c v) | c <- possibleCtors]
      | otherwise = useless (defaultM m) ps
      where
        complete = not (null headCtors) && sameSet headCtors possibleCtors
        headCtors = headConstructors m
        possibleCtors = ctorsOfSameRange sig (head headCtors)

    specializeM c m = map (specializeV c) (filter keep m)
       where keep v = startsWithVar v || (headConstructor v == c)

    specializeV c (Appl _ ps : qs) = ps ++ qs
    specializeV c (Var _ : qs) = replicate (arity sig c) (Var "_") ++ qs

    defaultM m = map tail (filter startsWithVar m)

