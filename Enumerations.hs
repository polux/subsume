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

module Enumerations (myShrink, myPairShrink) where

import qualified Test.Feat as F ( Enumerable(..), consts, uniform )
import qualified Test.Feat.Enumerate as F ( noOptim )
import qualified Test.LazySmallCheck as LSC
    ( Serial(..), cons, (\/), (><) )
import qualified Test.QuickCheck as QC
    ( Arbitrary(..), sized, shrinkList, genericShrink )
import Datatypes
    ( Signature(Signature),
      Decl(Decl),
      Pattern(..),
      FunName(..),
      range )
import Examples ( fork, tip )

instance LSC.Serial Pattern where
  series = (LSC.cons fork LSC.>< LSC.series LSC.>< LSC.series)
    LSC.\/ (LSC.cons tip)
    LSC.\/ (LSC.cons PVar)

instance F.Enumerable Pattern where
  enumerate = F.consts [ fork <$> enumerateTree <*> enumerateTree ]
    where enumerateTree = F.consts
            [ fork <$> F.noOptim enumerateTree <*> F.noOptim enumerateTree
            , pure tip
            , pure PVar]

instance QC.Arbitrary Pattern where
  arbitrary = QC.sized F.uniform
  shrink = QC.genericShrink

instance QC.Arbitrary FunName where
  arbitrary = FunName <$> QC.sized F.uniform
  shrink x = []

myShrink sig PVar = []
myShrink sig (PCons f []) = []
myShrink sig@(Signature decls) (PCons f ts) = PVar : constants ++ subterms ++ recurse
  where rg = range sig f
        constants = [PCons c [] | Decl c [] _ <- decls]
        subterms = filter hasRg (concatMap subterms' ts)
        subterms' PVar = []
        subterms' t@(PCons _ ts) = t : (concatMap subterms' ts)
        hasRg (PCons f _ ) = range sig f == rg
        recurse = map (PCons f) (shrinkOne ts)
        shrinkOne []     = []
        shrinkOne (x:xs) = [ x':xs | x'  <- myShrink sig x ]
                           ++ [ x:xs' | xs' <- shrinkOne xs ]

myPairShrink sig (ps, p) = [(ps', p) | ps' <- QC.shrinkList (myShrink sig) ps]
                        ++ [(ps, p') | p' <- myShrink sig p]


