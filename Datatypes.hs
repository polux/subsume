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

{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Datatypes (
  FunName(..),
  TypeName(..),
  Decl(..),
  Signature(..),
  Pattern(..),
  domain,
  range,
  arity,
  functionsOfRange,
  functionsOfSameRange
) where

import Data.List ( intercalate, find )
import Data.Maybe ( fromJust )
import Data.MemoTrie
    ( Reg, HasTrie(..), untrieGeneric, trieGeneric, enumerateGeneric )
import Data.String ( IsString(..) )
import GHC.Generics ( Generic )

{- Datatypes -}

newtype FunName = FunName String
  deriving (Eq, Ord, Generic)

data Pattern = PCons FunName [Pattern] | PVar
  deriving (Eq, Ord, Generic)

newtype TypeName = TypeName String
  deriving (Eq, Ord, Generic)

data Decl = Decl FunName [TypeName] TypeName
  deriving (Eq, Generic)

data Signature = Signature [Decl]
  deriving (Eq, Generic)

{- Pretty Printing -}

instance Show FunName where
  show (FunName x) = x

instance Show TypeName where
  show (TypeName ty) = ty

parSep :: [String] -> String
parSep ss = "(" ++ intercalate ", " ss ++ ")"

instance Show Pattern where
  show (PCons f ps) = show f ++ parSep (map show ps)
  show PVar = "_"

instance Show Decl where
  show (Decl f tys ty) = show f ++ ": " ++ parSep (map show tys) ++ " -> " ++ show ty

instance Show Signature where
  show (Signature decls) = show decls

{- HasTrie Instances -}

instance HasTrie Decl where
  newtype (Decl :->: b) = DeclTrie { unDeclTrie :: Reg Decl :->: b }
  trie = trieGeneric DeclTrie
  untrie = untrieGeneric unDeclTrie
  enumerate = enumerateGeneric unDeclTrie

instance HasTrie TypeName where
  newtype (TypeName :->: b) = TypeNameTrie { unTypeNameTrie :: Reg TypeName :->: b }
  trie = trieGeneric TypeNameTrie
  untrie = untrieGeneric unTypeNameTrie
  enumerate = enumerateGeneric unTypeNameTrie

instance HasTrie FunName where
  newtype (FunName :->: b) = FunNameTrie { unFunNameTrie :: Reg FunName :->: b }
  trie = trieGeneric FunNameTrie
  untrie = untrieGeneric unFunNameTrie
  enumerate = enumerateGeneric unFunNameTrie

instance HasTrie Signature where
  newtype (Signature :->: b) = SignatureTrie { unSignatureTrie :: Reg Signature :->: b }
  trie = trieGeneric SignatureTrie
  untrie = untrieGeneric unSignatureTrie
  enumerate = enumerateGeneric unSignatureTrie

instance HasTrie Pattern where
  newtype (Pattern :->: b) = PatternTrie { unPatternTrie :: Reg Pattern :->: b }
  trie = trieGeneric PatternTrie
  untrie = untrieGeneric unPatternTrie
  enumerate = enumerateGeneric unPatternTrie

{- IsString instances -}

instance IsString FunName where
  fromString = FunName

instance IsString TypeName where
  fromString = TypeName

{- Signature Helper Functions -}

_funName (Decl f _ _) = f
_domain (Decl _ d _) = d
_range (Decl _ _ r) = r

decl :: Signature -> FunName -> Decl
decl (Signature sig) f = fromJust (find hasF sig)
  where hasF (Decl g _ _) = f == g

domain :: Signature -> FunName -> [TypeName]
domain sig f = _domain (decl sig f)

range :: Signature -> FunName -> TypeName
range sig f = _range (decl sig f)

arity :: Signature -> FunName -> Int
arity sig f = length (domain sig f)

functionsOfRange :: Signature -> TypeName -> [FunName]
functionsOfRange (Signature sig) ty = map _funName (filter hasRangeTy sig)
  where hasRangeTy (Decl _ _ ty') = ty == ty'

functionsOfSameRange :: Signature -> FunName -> [FunName]
functionsOfSameRange sig f = functionsOfRange sig (range sig f)

