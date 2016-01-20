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

module Terms (
  Substitution,
  substitute,
  renameUnderscores,
  matches
) where

import Datatypes
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict (evalState, get, put)

type Substitution = M.Map VarName Term

substitute :: Substitution -> Term -> Term
substitute subst t = replace t
  where replace (Appl f ts) = Appl f (map (substitute subst) ts)
        replace (Var x) = M.findWithDefault (Var x) x subst
        replace (Alias x t) = Alias x (replace t)
        replace (Plus t1 t2) = Plus (replace t1) (replace t2)
        replace Bottom = Bottom

renameUnderscores :: Term -> Term
renameUnderscores t = evalState (rename t) 0
  where rename (Appl f ps) = Appl f <$> mapM rename ps
        rename (Alias x p) = Alias x <$> rename p
        rename (Plus t1 t2) = Plus <$> rename t1 <*> rename t2
        rename Bottom = return Bottom
        rename (Var v) = Var <$> renameVarName v

        renameVarName (VarName "_") = VarName <$> freshName
        renameVarName (VarName x) = return (VarName x)

        freshName = do
          n <- get
          put (n+1)
          return ("_" ++ show n)

matches :: Term -> Term -> Bool
matches (Appl f ts) (Appl g us) = f == g && and (zipWith matches ts us)
matches (Alias _ t) u = matches t u
matches t (Alias _ u) = matches t u
matches (Var _) t = True
matches _ _ = False
