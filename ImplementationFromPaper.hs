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
              | RPattern :<<: RPattern
              | RPlus [RPattern]
              | RUnion [RPattern]
              | RTrue
              | RFalse
  deriving (Eq, Ord, Data, Typeable)

instance Show RPattern where
  show (RCons f ps) = show f ++ "(" ++ intercalate ", " (map show ps) ++ ")"
  show RVar = "_"
  show (p1 :<<: p2) = "(" ++ show p1 ++ " <<? " ++ show p2 ++ ")"
  show (RPlus ps) = "(" ++ intercalate " (+) " (map show ps) ++ ")"
  show (RUnion ps) = "(" ++ intercalate " U " (map show ps) ++ ")"
  show RTrue = "T"
  show RFalse = "F"

isRPlus :: RPattern -> Bool
isRPlus (RPlus _) = True
isRPlus _ = False

isRUnion :: RPattern -> Bool
isRUnion (RUnion _) = True
isRUnion _ = False

isRTrue :: RPattern -> Bool
isRTrue RTrue = True
isRTrue _ = False

isRFalse :: RPattern -> Bool
isRFalse RFalse = True
isRFalse _ = False

isRVar :: RPattern -> Bool
isRVar RVar = True
isRVar _ = False

split :: (a -> Bool) -> [a] -> ([a], [a])
split f xs = (filter f xs, filter (not . f) xs)

flattenRPlusses = concatMap flatten
  where flatten (RPlus ps) = ps
        flatten p = [p]

flattenRUnions = concatMap flatten
  where flatten (RUnion ps) = ps
        flatten p = [p]

cursor :: [a] -> [(a, [a])]
cursor xs = catMaybes (zipWith extract (inits xs) (tails xs))
  where extract xs (y:ys) = Just (y, (xs++ys))
        extract _ _ = Nothing

match :: Eq a => [a] -> [a] -> Maybe ([a], a, a, [a])
match xs ys = match' [] xs ys
  where match' p (x:xs) (y:ys)
          | x == y = match' (p++[x]) xs ys
          | otherwise = if xs == ys then Just (p, x, y, xs) else Nothing
        match' p [] [] = Nothing

matchTwoFuns :: ([RPattern]-> RPattern) -> [RPattern] -> [[RPattern]]
matchTwoFuns cons xs = do
  (RCons f ps, xs') <- cursor xs
  (RCons g qs, xs'') <- cursor xs'
  guard (f == g)
  case match ps qs of
    Just (p, x, y, s) -> return (RCons f (p ++ [cons [x, y]] ++ s) : xs'')
    Nothing -> []

matchTwoTermVars :: [RPattern] -> [[RPattern]]
matchTwoTermVars xs = do
  (t :<<: RVar, xs') <- cursor xs
  (u :<<: RVar, xs'') <- cursor xs'
  guard (not (isRVar t) && not (isRVar u))
  return ((RUnion [t, u] :<<: RVar) : xs'')

constructorsOfShallowPatterns :: [RPattern] -> [FunName]
constructorsOfShallowPatterns ps = [f | RCons f xs <- ps, all isRVar xs]

complete sig [] = False
complete sig (f:fs) = S.fromList (functionsOfSameRange sig f) == S.fromList (f:fs)

isCompleteShallowPatterns :: Signature -> [RPattern] -> Bool
isCompleteShallowPatterns sig ps = complete sig (constructorsOfShallowPatterns ps)

trs :: Signature -> RPattern -> Maybe RPattern
trs sig = trs'
  where
--    trs' x | traceShow x False = undefined
    trs' (RPlus ps) | any isRPlus ps = Just (RPlus (flattenRPlusses ps))
    trs' (RUnion ps) | any isRUnion ps = Just (RUnion (flattenRUnions ps))

    trs' (RCons f ps :<<: RCons g qs)
      | f == g = Just (if null ps then RTrue else RCons f (zipWith (:<<:) ps qs))
      | otherwise = Just RFalse
    trs' (RVar :<<: _) = Just RTrue
    trs' (RCons f ps) | not (null ps) && all isRTrue ps = Just RTrue
    trs' (RCons f ps) | any isRFalse ps = Just RFalse
    trs' (RPlus ps) | any isRTrue ps = Just RTrue
    trs' (RPlus ps) | any isRFalse ps = Just (RPlus (filter (not . isRFalse) ps))
    trs' (RPlus ps) | (ps':_) <- matchTwoTermVars ps = Just (RPlus ps')
    trs' (RPlus ps) | (ps':_) <- matchTwoFuns RPlus ps = Just (RPlus ps')
    trs' (RUnion ps) | any isRVar ps = Just RVar
    trs' (RUnion ps) | (ps':_) <- matchTwoFuns RUnion ps = Just (RUnion ps')
    trs' (RUnion ps) | isCompleteShallowPatterns sig ps = Just RVar
    trs' _ = Nothing

normalize sig = rewrite (trs sig)

convert :: Pattern -> RPattern
convert (PCons f ps) = RCons f (map convert ps)
convert PVar = RVar

subsumes :: Signature -> [Pattern] -> Pattern -> Bool
subsumes sig [] p = False
subsumes sig ps p = normalize sig (RPlus [convert q :<<: p' | q <- ps]) == RTrue
  where p' = convert p
