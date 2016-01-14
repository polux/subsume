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

import Debug.Trace
import Data.List (intercalate, tails, inits)
import Data.Maybe (catMaybes)
import Datatypes
    ( Signature, Pattern(..), FunName(..), arity, functionsOfSameRange )
import qualified Data.Set as S ( fromList )
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Typeable
import Control.Monad (guard)
import Examples
import qualified Data.Set as S ( fromList )

deriving instance Data FunName

-- R is for "Rich" because it's patterns enriched with constructors from the algo
data RPattern = RCons FunName [RPattern]
              | RVar
              | RPattern :\: RPattern
              | RPlus [RPattern]
              | RVarAt RPattern
              | RBottom
  deriving (Eq, Ord, Data, Typeable)

instance Show RPattern where
  show (RCons f ps) = show f ++ "(" ++ intercalate ", " (map show ps) ++ ")"
  show RVar = "_"
  show (p1 :\: p2) = "(" ++ show p1 ++ " \\ " ++ show p2 ++ ")"
  show (RPlus ps) = "(" ++ intercalate " + " (map show ps) ++ ")"
  show (RVarAt p) = "_@" ++ show p
  show RBottom = "⊥"

isRPlus :: RPattern -> Bool
isRPlus (RPlus _) = True
isRPlus _ = False

isRBottom :: RPattern -> Bool
isRBottom RBottom = True
isRBottom _ = False

isRVar :: RPattern -> Bool
isRVar RVar = True
isRVar _ = False

flattenRPlusses = concatMap flatten
  where flatten (RPlus ps) = ps
        flatten p = [p]

-- interleave [a,b,c] [A,B,C] = [[A,b,c], [a,B,c], [a,b,C]]
interleave :: [a] -> [a] -> [[a]]
interleave [] [] = []
interleave xs ys = zipWith3 glue (inits xs) ys (tails (tail xs))
  where glue xs x ys = (xs++x:ys)

trs :: Signature -> RPattern -> Maybe RPattern
trs sig = trs'
  where
--    trs' x | traceShow x False = undefined
    trs' (RPlus ps) | any isRPlus ps = Just (RPlus (flattenRPlusses ps))    -- flattening of +

    trs' (RPlus []) = Just RBottom
    trs' (RPlus ps) | any isRBottom ps = Just (RPlus (filter (not . isRBottom) ps))
    trs' (RCons f ps) | any isRBottom ps = Just RBottom
    trs' (RVarAt RBottom) = Just RBottom
    trs' (u :\: RVar) = Just RBottom
    trs' (u :\: RBottom) = Just u
    trs' (u :\: RPlus vs) = Just (foldl (:\:) u vs)
    trs' (RVar :\: RCons g ps) = Just (RVarAt (RPlus ts) :\: RCons g ps)
      where ts = [RCons f (replicate (arity sig f) RVar) | f <- functionsOfSameRange sig g]
    trs' (RBottom :\: RCons _ _) = Just RBottom
    trs' (RPlus qs :\: RCons f ps) = Just (RPlus [q :\: RCons f ps | q <- qs])
    trs' (RCons f ps :\: RCons g qs)
        | f /= g || someUnchanged = Just (RCons f ps)
        | otherwise = Just (RPlus [RCons f ps' | ps' <- interleave ps pqs])
      where pqs = zipWith combine ps qs
            combine p q = normalize sig (p :\: q)
            someUnchanged = or (zipWith (==) ps pqs)
    trs' (RVarAt u :\: v) = Just (RVarAt (u :\: v))
    trs' (u :\: RVarAt v) = Just (u :\: v)
    trs' _ = Nothing

normalize sig = rewrite (trs sig)

convert :: Pattern -> RPattern
convert (PCons f ps) = RCons f (map convert ps)
convert PVar = RVar

subsumes :: Signature -> [Pattern] -> Pattern -> Bool
subsumes sig [] p = False
subsumes sig ps p = normalize sig (p' :\: RPlus ps') == RBottom
  where p' = convert p
        ps' = map convert ps

main = --print (interleave "abcd" "ABCD" :: [String])
  print (subsumes test_sig [fork (fork PVar PVar) PVar] (fork tip tip))
