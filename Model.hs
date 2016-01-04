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

module Model (subsumes) where

import Debug.Trace
import Data.MemoTrie ( mup, memo3 )
import GHC.Generics ()
import qualified Data.Set as S
    ( Set, unions, isSubsetOf, fromList )
import Datatypes
    ( TypeName, Signature(..), Pattern(..), Decl(Decl), range )

depth PVar = 0
depth (PCons f []) = 1
depth (PCons f ts) = 1 + maximum (map depth ts)

matches :: Pattern -> Pattern -> Bool
matches (PCons f ts) (PCons g us) = f == g && and (zipWith matches ts us)
matches PVar _ = True
matches _ _ = False

hasRange ty (Decl _ _ ty') = ty == ty'

closedTerms :: Signature -> TypeName -> Int -> [Pattern]
closedTerms = memo3 closedTerms'
  where closedTerms' (Signature sig) ty 0 =
          [PCons f [] | Decl f [] _ <- filter (hasRange ty) sig]
        closedTerms' (Signature sig) ty n = do
          Decl f tys _ <- filter (hasRange ty) sig
          ts <- sequence (map (\ty -> closedTerms (Signature sig) ty (n-1)) tys)
          return (PCons f ts)

isVar :: Pattern -> Bool
isVar PVar = True
isVar _       = False

isPCons :: Pattern -> Bool
isPCons (PCons _ _) = True
isPCons _ = False

semantics :: Signature -> Int -> TypeName -> Pattern -> S.Set Pattern
semantics = (mup memo3) semantics'
  where semantics' sig d ty p = S.fromList $ filter (matches p) (closedTerms sig ty d)

semanticss :: Signature -> Int -> TypeName -> [Pattern] -> S.Set Pattern
semanticss = (mup memo3) semanticss'
  where semanticss' sig d ty ps = S.unions (map (semantics sig d ty) ps)

myHead s [] = error s
myHead _ (x:xs) = x

subsumes :: Signature -> [Pattern] -> Pattern -> Bool
--subsumesModel' _ ps p | trace ("subsumesModel " ++ show ps ++ " " ++ show p) False = undefined
subsumes _ [] p = False
subsumes sig ps p | any isVar ps = True
                        | otherwise = not (null sem) && sem `S.isSubsetOf` sems
  where sem = semantics sig dept ty p
        sems = semanticss sig dept ty ps
        ty = range sig (funName (myHead "subsumesModel" (filter isPCons ps)))
        dept = maximum (map depth (p:ps))
        funName (PCons f _) = f
