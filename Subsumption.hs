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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, DeriveGeneric, TypeOperators, TypeFamilies #-}

--import Test.SmallCheck.Series
import qualified Test.Feat as F
import qualified Test.Feat.Enumerate as F
import qualified Test.LazySmallCheck as LSC
import qualified Test.QuickCheck as QC
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Function
import Data.Typeable (Typeable)
import Data.MemoTrie
import GHC.Generics
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M

type FunctionSymbol = String
type VariableSymbol = String
type Type = String
type Signature = [Decl]
data Decl = Decl { symbol :: FunctionSymbol, range :: Type, domain :: [Type] }
type Constraints = M.Map VariableSymbol (S.Set Term)

data Term = Appl { funName :: FunctionSymbol, children :: [Term] }
          | Var { varName :: VariableSymbol }
 deriving (Eq, Ord, Typeable, Generic)

depth (Var _) = 0
depth (Appl f []) = 1
depth (Appl f ts) = 1 + maximum (map depth ts)

matches :: Term -> Term -> Bool
matches (Appl f ts) (Appl g us) = f == g && and (zipWith matches ts us)
matches (Var _) _ = True
matches _ _ = False

hasRange ty (Decl _ ty' _) = ty == ty'

instance HasTrie Decl where
  newtype (Decl :->: b) = DeclTrie ((FunctionSymbol, Type, [Type]) :->: b)
  trie f = DeclTrie (trie (f . decl))
  untrie (DeclTrie t) = untrie t . undecl
  enumerate (DeclTrie t) = [(decl x, y) | (x, y) <- enumerate t]

decl (x, y, z) = Decl x y z
undecl (Decl x y z) = (x, y, z)

closedTerms :: Signature -> Type -> Int -> [Term]
closedTerms = memo3 closedTerms'
  where closedTerms' sig ty 0 = [Appl f [] | Decl f _ [] <- filter (hasRange ty) sig]
        closedTerms' sig ty n = do
          Decl f _ tys <- filter (hasRange ty) sig
          ts <- sequence (map (\ty -> closedTerms sig ty (n-1)) tys)
          return (Appl f ts)

covers :: Signature -> [Term] -> Bool
--covers _ ps | trace ("covers " ++ show ps) False = undefined
covers sig ps | any isVar ps = True
              | {- optim -} not (S.fromList [f | Appl f _ <- ps] == S.fromList (functionsOfType sig typeOfTs)) = False
              | otherwise = all matchesOnePattern closedTermOfMaxDepth
  where typeOfTs = typeOf sig (funName (myHead "covers" (filter isAppl ps)))
        maxDepth = maximum (map depth ps)
        closedTermOfMaxDepth = closedTerms sig typeOfTs maxDepth
        matchesOnePattern closedTerm = or [matches p closedTerm | p <- ps]

instance Show Term where
  show (Appl f ts) = f ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (Var x) = x

sameSet :: [FunctionSymbol] -> [FunctionSymbol] -> Bool
sameSet fs1 fs2 = S.fromList fs1 == S.fromList fs2

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

isAppl :: Term -> Bool
isAppl (Appl _ _) = True
isAppl _ = False

functionsOfType :: Signature -> Type -> [FunctionSymbol]
functionsOfType sig ty = map symbol (filter ((== ty) . range) sig)

typeOf :: Signature -> FunctionSymbol -> Type
typeOf sig funName = range (myHead ("typeOf " ++ funName) (filter ((== funName) . symbol) sig))

-- groupChildren [f(t1, t2, t3), f(u1, u2, u3), ...] = [[t1, u1, ...], [t2, u2, ...], [t3, u3, ...]]
groupChildren :: [Term] -> [[Term]]
groupChildren ts = transpose (map children ts)

groupByKey :: Ord k => (a -> k) -> [a] -> [[a]]
groupByKey key xs = M.elems (M.fromListWith (++) (map makeEntry xs))
  where makeEntry x = (key x, [x])

match :: Term -> Term -> Maybe Constraints
match (Appl f ts) (Appl g us) | f == g    = combine (zipWith match ts us)
                              | otherwise = Nothing
  where combine mcs = fmap (M.unionsWith S.union) (sequence mcs)
match (Var x) t = Just M.empty
match t (Var x) = Just (M.singleton x (S.singleton t))

solvable :: Signature -> Constraints -> Bool
solvable sig cs = all (covers sig . S.toList) (M.elems cs)

traceShowId x = traceShow x x

subsumes :: Signature -> [Term] -> Term -> Bool
--subsumes _ ps p | trace ("subsumes " ++ show ps ++ " " ++ show p) False = undefined
subsumes sig [] p = False
subsumes sig ps p = subsumes' (catMaybes [match q p | q <- ps])
  where subsumes' [] = False
        subsumes' css | any M.null css = True
                      | otherwise = solvable sig (M.unionsWith S.union css)

minimize sig ps = minimize' ps []
  where minimize' [] kernel = kernel
        minimize' (p:ps) kernel =
           if subsumesModel sig (ps++kernel) p
              then shortest (minimize' ps (p:kernel)) (minimize' ps kernel)
              else minimize' ps (p:kernel)

        shortest xs ys = if length xs <= length ys then xs else ys

semantics :: Signature -> Int -> Type -> Term -> S.Set Term
semantics sig d ty p = S.fromList $ filter (matches p) (closedTerms sig ty d)

myHead s [] = error s
myHead _ (x:xs) = x

subsumesModel :: Signature -> [Term] -> Term -> Bool
--subsumesModel _ ps p | trace ("subsumesModel " ++ show ps ++ " " ++ show p) False = undefined
subsumesModel sig ps p | any isVar ps = True
                       | otherwise = sem p `S.isSubsetOf` (S.unions (map sem ps))
  where sem = semantics sig dept ty
        ty = typeOf sig (funName (myHead "subsumesModel" (filter isAppl ps)))
        dept = maximum (map depth (p:ps))

linear p = S.size (S.fromList vars) == length vars
  where vars = vars' p
        vars' (Appl f ts) = concat (map vars' ts)
        vars' (Var x) = [x]

example_patterns = [
  interp(s(s(s(s(s(s(s(Var "z_33_1"))))))), Var "y"),
  interp(s(s(s(s(s(s(z())))))), cons(nv(Var "n1"), cons(nv(Var "n2"), nil()))),
  interp(s(s(s(s(s(s(Var "z_69_1")))))), cons(bv(Var "z_38_1"), Var "z_35_2")),
  interp(s(s(s(s(s(s(Var "z_69_1")))))), cons(Var "z_35_1", cons(bv(Var "z_43_1"), Var "z_40_2"))),
  interp(s(s(s(s(s(z()))))), cons(bv(Var "b1"), cons(bv(Var "b2"), nil()))),
  interp(s(s(s(s(s(z()))))), cons(nv(Var "z_51_1"), Var "z_47_2")),
  interp(s(s(s(s(s(z()))))), cons(Var "z_47_1", cons(nv(Var "z_56_1"), Var "z_52_2"))),
  interp(s(s(s(s(s(Var "z_99_1"))))), cons(Var "z_35_1", nil())),
  interp(s(s(s(s(z())))), cons(bv(Var "b"), nil())),
  interp(s(s(s(s(z())))), cons(nv(Var "z_51_1"), Var "z_47_2")),
  interp(s(s(s(s(z())))), cons(Var "z_106_1", cons(Var "z_111_1", Var "z_111_2"))),
  interp(s(s(s(s(Var "z_124_1")))), nil()),
  interp(s(s(s(z()))), cons(Var "z_126_1", Var "z_126_2")),
  interp(s(s(s(z()))), nil()),
  interp(s(s(z())), cons(bv(Var "z_38_1"), Var "z_35_2")),
  interp(s(s(z())), cons(nv(Var "m"), cons(nv(Var "n"), nil()))),
  interp(s(s(z())), cons(Var "z_35_1", cons(bv(Var "z_43_1"), Var "z_40_2"))),
  interp(s(s(z())), cons(Var "z_35_1", nil())),
  interp(s(s(z())), nil()),
  interp(s(s(Var "z_493_1")), cons(nv(Var "z_73_1"), nil())),
  interp(s(z()), cons(bv(Var "z_38_1"), Var "z_35_2")),
  interp(s(z()), cons(nv(Var "n"), nil())),
  interp(s(z()), cons(Var "z_555_1", cons(Var "z_560_1", Var "z_560_2"))),
  interp(s(z()), nil()),
  interp(z(), cons(Var "z_126_1", Var "z_126_2")),
  interp(z(), nil()),
  interp(Var "x", cons(bv(Var "z_38_1"), cons(nv(Var "z_83_1"), Var "z_79_2"))),
  interp(Var "x", cons(nv(Var "z_73_1"), cons(bv(Var "z_43_1"), Var "z_40_2"))),
  interp(Var "x", cons(undef(), Var "z_35_2")),
  interp(Var "x", cons(Var "z_35_1", cons(undef(), Var "z_40_2"))),
  interp(Var "x", cons(Var "z_35_1", cons(Var "z_40_1", cons(Var "z_45_1", Var "z_45_2"))))]

interp(x, y) = Appl "interp" [x, y]
cons(x, y) = Appl "cons" [x, y]
bv(x) = Appl "bv" [x]
nv(x) = Appl "nv" [x]
undef() = Appl "undef" []
s(x) = Appl "s" [x]
z() = Appl "z" []
nil() = Appl "nil" []

example_sig =
  [Decl "cons" "List" ["Val", "List"],
   Decl "nil" "List" [],
   Decl "bv" "Val" ["Bool"],
   Decl "nv" "Val" ["Nat"],
   Decl "undef" "Val" [],
   Decl "s" "Nat" ["Nat"],
   Decl "z" "Nat" [],
   Decl "true" "Bool" [],
   Decl "false" "Bool" [],
   Decl "interp" "Result" ["Nat", "List"]]

test_sig =
  [ Decl "fork" "Tree" ["Tree", "Tree"]
  , Decl "tip" "Tree" []
  ]

-- counter example [fork(tip(), tip()), fork(tip(), x10), fork(fork(x000, x100), x10)] fork(x00, x10)

fork l r = Appl "fork" [l, r]
tip = Appl "tip" []

instance LSC.Serial Term where
  series = (LSC.cons (curry cons) LSC.>< valSeries LSC.>< LSC.series) LSC.\/ LSC.cons0 (nil()) LSC.\/ varSeries
    where valSeries = (LSC.cons nv LSC.>< natSeries) LSC.\/ LSC.cons0 (undef()) LSC.\/ varSeries
          natSeries = (LSC.cons s LSC.>< natSeries) LSC.\/ LSC.cons0 (z()) LSC.\/ varSeries
          varSeries = LSC.cons0 (Var "x1") LSC.\/ LSC.cons0 (Var "x2") LSC.\/ LSC.cons0 (Var "x3")

instance F.Enumerable Term where
  enumerate = linearize <$> enumerateTree --enumerateInterp
    where enumerateVar = pure (Var "_")
          enumerateInterp = F.consts [curry interp <$> enumerateNat <*> enumerateList, enumerateVar]
          enumerateList = F.consts [curry cons <$> enumerateVal <*> F.noOptim enumerateList, pure (nil()), enumerateVar]
          enumerateVal = F.consts [nv <$> enumerateNat, bv <$> enumerateVar, pure (undef()), enumerateVar]
          enumerateNat = F.consts [s <$> F.noOptim enumerateNat, pure (z()), enumerateVar]

          enumerateTree = F.consts [ fork <$> enumerateTree' <*> enumerateTree' ]

          enumerateTree' = F.consts [ fork <$> F.noOptim enumerateTree' <*> F.noOptim enumerateTree'
                                    , pure tip
                                    , enumerateVar]

          linearize t = linearize' [0] t
            where linearize' path (Var _) = Var ("x" ++ concat (map show path))
                  linearize' path (Appl f ts) = Appl f (zipWith go [0..] ts)
                    where go n t = linearize' (n:path) t

instance QC.Arbitrary Term where
  arbitrary = QC.sized F.uniform
  shrink = QC.genericShrink

erase (Appl f ts) = (Appl f (map erase ts))
erase (Var x) = Var "_"

hasVars (Appl _ ts) = any hasVars ts
hasVars (Var _) = True

data Input = Input [Term] Term
  deriving (Typeable, Show)

instance F.Enumerable Input where
  enumerate = Input <$> ts <*> F.enumerate
    where ts = (\x y z  -> (x:y:z:[])) <$> F.enumerate <*> F.enumerate <*> F.enumerate -- <*> F.enumerate

property (Input ps p) = subsumesModel test_sig ps p == subsumes test_sig ps p

--property1 ps p = subsumes example_sig ps p && all linear ps && linear p ==> forAll $ \qs -> subsumes example_sig (ps++qs) p

property2 ps p = (LSC.lift (linear p) LSC.*&* parAll (map (LSC.lift . linear) ps) LSC.*&* LSC.lift (hasVars p) LSC.*&* LSC.lift (not (null ps)) LSC.*&* LSC.lift (any ((>= depth p) . depth) ps)) LSC.*=>* LSC.lift (subsumesModel example_sig ps p == subsumes example_sig ps p)
  where parAll = foldr (LSC.*&*) (LSC.lift True)

{-
property3 ps = (length ps > 1 && all linear ps && all isAppl ps) ==> all sameAsResult (map (minimize example_sig) (permutations ps))
  where result = minimize example_sig ps
        sameAsResult r = S.fromList (map erase r) == S.fromList (map erase result)
-}

property4 ps p qs = subsumes example_sig ps p `implies` subsumes example_sig (ps++qs) p
  where implies a b = not a || b

main0 = F.featCheck 18 property

main01 = F.featCheck 15 (F.funcurry (F.funcurry property4))

main = do
  print (subsumes test_sig ps p)
  print (subsumesModel test_sig ps p)
  where ps = [fork tip tip, fork tip x10, fork (fork x000 x100) x10]
        p = fork x00 x10
        x00 = Var "x00"
        x10 = Var "x10"
        x000 = Var "x000"
        x100 = Var "x100"

main1 = LSC.depthCheck 4 property2
main2 = do
  print (minimize example_sig [nil(), cons(undef(), Var "x1")])
  print (minimize example_sig [cons(undef(), Var "x1"), nil()])


main3 = print (subsumes example_sig example_patterns t)
  where t = interp(s(s(z())), cons(bv(Var "z_38_1"), Var "z_35_2"))


main4 = do
  print (length example_patterns)
  let res = (minimize example_sig example_patterns)
  print (length res)
  putStrLn (unlines (map show res))


main5 = print (subsumes example_sig (example_patterns \\ [t]) t)
  where t = interp(s(s(z())), cons(Var "z_35_1", cons(bv(Var "z_43_1"), Var "z_40_2")))

main6 = putStrLn $ unlines $ map show $ (closedTerms example_sig "List" 2)

