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

{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies, TemplateHaskell #-}

module Datatypes (
  FunName(..),
  TypeName(..),
  VarName(..),
  Decl(..),
  Signature(..),
  Term(..),
  Rule(..),
  Module(..)
) where

import Data.List ( intercalate )
import Data.Maybe ( fromJust )
import Data.String ( IsString(..) )
import Data.MemoTrie
    ( Reg, HasTrie(..), untrieGeneric, trieGeneric, enumerateGeneric )
import GHC.Generics ( Generic )
import Data.Function.Memoize


{- Datatypes -}

newtype VarName = VarName String
  deriving (Eq, Ord, Generic)

newtype FunName = FunName String
  deriving (Eq, Ord, Generic)

newtype TypeName = TypeName String
  deriving (Eq, Ord, Generic)

data Decl = Decl FunName [TypeName] TypeName
  deriving (Eq, Ord, Generic)

data Signature = Signature [Decl] [Decl]
  deriving (Eq, Ord, Generic)

data Term = Appl FunName [Term]
          | Var VarName
          | Plus Term Term
          | Alias VarName Term
          | Bottom
  deriving (Eq, Ord, Generic)

data Rule = Rule Term Term
  deriving (Eq, Ord)

data Module = Module Signature [Rule]
  deriving (Eq, Ord, Show)

{- Pretty Printing -}

instance Show VarName where
  show (VarName x) = x

instance Show FunName where
  show (FunName x) = x

instance Show TypeName where
  show (TypeName ty) = ty

parSep :: [String] -> String
parSep ss = "(" ++ intercalate ", " ss ++ ")"

instance Show Decl where
  show (Decl f tys ty) = show f ++ ": " ++ intercalate " * " (map show tys) ++ " -> " ++ show ty

instance Show Signature where
  show (Signature ctors funs) = show (ctors, funs)

instance Show Term where
  show (Appl f ps) = show f ++ parSep (map show ps)
  show (Var x) = show x
  show (Plus p1 p2) = "(" ++ show p1 ++ " + " ++ show p2 ++ ")"
  show (Alias x p) = show x ++ "@" ++ show p
  show Bottom = "⊥"

instance Show Rule where
  show (Rule lhs rhs) = show lhs ++ " -> " ++ show rhs

{- IsString instances -}

instance IsString VarName where
  fromString = VarName

instance IsString FunName where
  fromString = FunName

instance IsString TypeName where
  fromString = TypeName

{- HasTrie Instances -}

instance HasTrie VarName where
  newtype (VarName :->: b) = VarNameTrie { unVarNameTrie :: Reg VarName :->: b }
  trie = trieGeneric VarNameTrie
  untrie = untrieGeneric unVarNameTrie
  enumerate = enumerateGeneric unVarNameTrie

instance HasTrie FunName where
  newtype (FunName :->: b) = FunNameTrie { unFunNameTrie :: Reg FunName :->: b }
  trie = trieGeneric FunNameTrie
  untrie = untrieGeneric unFunNameTrie
  enumerate = enumerateGeneric unFunNameTrie

instance HasTrie TypeName where
  newtype (TypeName :->: b) = TypeNameTrie { unTypeNameTrie :: Reg TypeName :->: b }
  trie = trieGeneric TypeNameTrie
  untrie = untrieGeneric unTypeNameTrie
  enumerate = enumerateGeneric unTypeNameTrie

instance HasTrie Decl where
  newtype (Decl :->: b) = DeclTrie { unDeclTrie :: Reg Decl :->: b }
  trie = trieGeneric DeclTrie
  untrie = untrieGeneric unDeclTrie
  enumerate = enumerateGeneric unDeclTrie

instance HasTrie Signature where
  newtype (Signature :->: b) = SignatureTrie { unSignatureTrie :: Reg Signature :->: b }
  trie = trieGeneric SignatureTrie
  untrie = untrieGeneric unSignatureTrie
  enumerate = enumerateGeneric unSignatureTrie

instance HasTrie Term where
  newtype (Term :->: b) = TermTrie { unTermTrie :: Reg Term :->: b }
  trie = trieGeneric TermTrie
  untrie = untrieGeneric unTermTrie
  enumerate = enumerateGeneric unTermTrie

{- Memoizable instances -}

deriveMemoizable ''VarName
deriveMemoizable ''FunName
deriveMemoizable ''TypeName
deriveMemoizable ''Decl
deriveMemoizable ''Signature
deriveMemoizable ''Term
