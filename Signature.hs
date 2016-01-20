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

module Signature (
  domain,
  range,
  arity,
  ctorsOfRange,
  ctorsOfSameRange
) where

import Datatypes (FunName, TypeName, Decl(..), Signature(..))
import Data.List ( find )
import Data.Maybe ( fromJust )

_funName (Decl f _ _) = f
_domain (Decl _ d _) = d
_range (Decl _ _ r) = r

decl :: Signature -> FunName -> Decl
decl (Signature ctors funs) f = unpack (find hasF (ctors ++ funs))
  where hasF (Decl g _ _) = f == g
        unpack (Just d) = d
        unpack Nothing = error (show f ++ " is not declared")

domain :: Signature -> FunName -> [TypeName]
domain sig f = _domain (decl sig f)

range :: Signature -> FunName -> TypeName
range sig f = _range (decl sig f)

arity :: Signature -> FunName -> Int
arity sig f = length (domain sig f)

ctorsOfRange :: Signature -> TypeName -> [FunName]
ctorsOfRange (Signature ctors _) ty = map _funName (filter hasRangeTy ctors)
  where hasRangeTy (Decl _ _ ty') = ty == ty'

ctorsOfSameRange :: Signature -> FunName -> [FunName]
ctorsOfSameRange sig f = ctorsOfRange sig (range sig f)

