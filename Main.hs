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

import Datatypes ( Pattern(PVar) )
import Examples
import Implementation ( subsumes )
import qualified ImplementationFromPaper as Paper
import Enumerations ( myPairShrink )
import qualified Model as Model ( subsumes )
import qualified Test.Feat as F
    ( Enumerable(..), funcurry, featCheck )
import qualified Test.LazySmallCheck as LSC ( depthCheck )
import Data.Typeable ( Typeable )
import System.Environment ( getArgs )
import Data.List ( (\\) )
import qualified Test.QuickCheck as QC ( quickCheck, forAllShrink )
import Minimize ( minimize )

data Input = Input [Pattern] Pattern
  deriving (Typeable, Show)

instance F.Enumerable Input where
  enumerate = Input <$> F.enumerate <*> F.enumerate
--    where ts = (\x y -> [x, y]) <$> F.enumerate <*> F.enumerate

property (Input ps p) = Paper.subsumes test_sig ps p == subsumes test_sig ps p

property2 ps p = Paper.subsumes test_sig ps p == subsumes test_sig ps p

property4 ps p qs = subsumes test_sig ps p `implies` subsumes test_sig (ps++qs) p
  where implies a b = not a || b


bug_term = interp(s(s(z())), cons(_x, cons(bv(_x), _x)))

bug_patterns =
  [ interp(s(s(z())), cons(bv(_x), _x))
  , interp(_x, cons(nv(_x), cons(bv(_x), _x)))
  , interp(_x, cons(undef(), _x))
  , interp(_x, cons(_x, cons(_x, cons(_x, _x))))]

min_bug_patterns = [
  interp(s(_x), cons(bv(_x), _x)),
  interp(_x, cons(nv(_x), _x)),
  interp(_x, cons(undef(), _x))]

min_bug_term = interp(s(z()), cons(_x, _x))

main = do
  [x] <- getArgs
  main' (read x :: Int)

main' 0 = F.featCheck 22 property

main' 1 = F.featCheck 15 (F.funcurry (F.funcurry property4))

main' 2 = do
  print (subsumes test_sig ps p)
  print (Paper.subsumes test_sig ps p)
  where ps = [fork (fork x x) (fork x x), fork x tip, fork tip x]
        p = fork x x
        x = PVar

main' 3 = LSC.depthCheck 4 property2

main' 4 = do
  print (length example_patterns)
  let res = (minimize Paper.subsumes example_sig example_patterns)
  print (length res)
  putStrLn (unlines (map show res))


main' 5 = print (subsumes example_sig (example_patterns \\ [t]) t)
  where --t = interp(s(s(z())), cons(_x, cons(bv(_x), _x)))
        t = interp(_x, cons(_x, cons(_x, cons(_x, _x))))

main' 6 = do
  print (subsumes example_sig bug_patterns bug_term)
  print (Model.subsumes example_sig bug_patterns bug_term)

main' 7 = QC.quickCheck (QC.forAllShrink gen (myPairShrink example_sig) prop)
  where gen = pure (bug_patterns, bug_term)
        prop (ps, p) = subsumes example_sig ps p == Model.subsumes example_sig ps p

main' 8 = do
  print (subsumes example_sig min_bug_patterns min_bug_term)
  print (Model.subsumes example_sig min_bug_patterns min_bug_term)
