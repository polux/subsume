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

{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Datatypes
    ( Signature(Signature), Decl(Decl), Pattern(PCons, PVar) )

_x = PVar

{- Signature for smallcheck, quickcheck and feat -}

test_sig = Signature
  [ decl "fork" "Tree" ["Tree", "Tree"]
  , decl "tip" "Tree" []
  ] where decl f ty tys = Decl f tys ty

fork l r = PCons "fork" [l, r]
tip = PCons "tip" []

{- Pem's signature and patterns -}

example_sig = Signature
  [ decl "cons" "List" ["Val", "List"]
  , decl "nil" "List" []
  , decl "bv" "Val" ["Bool"]
  , decl "nv" "Val" ["Nat"]
  , decl "undef" "Val" []
  , decl "s" "Nat" ["Nat"]
  , decl "z" "Nat" []
  , decl "true" "Bool" []
  , decl "false" "Bool" []
  , decl "interp" "Result" ["Nat", "List"]
  ] where decl f ty tys = Decl f tys ty

interp(x, y) = PCons "interp" [x, y]
cons(x, y) = PCons "cons" [x, y]
bv(x) = PCons "bv" [x]
nv(x) = PCons "nv" [x]
undef() = PCons "undef" []
s(x) = PCons "s" [x]
z() = PCons "z" []
nil() = PCons "nil" []

example_patterns = [
  interp(s(s(s(s(s(s(s(_x))))))), _x),
  interp(s(s(s(s(s(s(z())))))), cons(nv(_x), cons(nv(_x), nil()))),
  interp(s(s(s(s(s(s(_x)))))), cons(bv(_x), _x)),
  interp(s(s(s(s(s(s(_x)))))), cons(_x, cons(bv(_x), _x))),
  interp(s(s(s(s(s(z()))))), cons(bv(_x), cons(bv(_x), nil()))),
  interp(s(s(s(s(s(z()))))), cons(nv(_x), _x)),
  interp(s(s(s(s(s(z()))))), cons(_x, cons(nv(_x), _x))),
  interp(s(s(s(s(s(_x))))), cons(_x, nil())),
  interp(s(s(s(s(z())))), cons(bv(_x), nil())),
  interp(s(s(s(s(z())))), cons(nv(_x), _x)),
  interp(s(s(s(s(z())))), cons(_x, cons(_x, _x))),
  interp(s(s(s(s(_x)))), nil()),
  interp(s(s(s(z()))), cons(_x, _x)),
  interp(s(s(s(z()))), nil()),
  interp(s(s(z())), cons(bv(_x), _x)),
  interp(s(s(z())), cons(nv(_x), cons(nv(_x), nil()))),
  interp(s(s(z())), cons(_x, cons(bv(_x), _x))),
  interp(s(s(z())), cons(_x, nil())),
  interp(s(s(z())), nil()),
  interp(s(s(_x)), cons(nv(_x), nil())),
  interp(s(z()), cons(bv(_x), _x)),
  interp(s(z()), cons(nv(_x), nil())),
  interp(s(z()), cons(_x, cons(_x, _x))),
  interp(s(z()), nil()),
  interp(z(), cons(_x, _x)),
  interp(z(), nil()),
  interp(_x, cons(bv(_x), cons(nv(_x), _x))),
  interp(_x, cons(nv(_x), cons(bv(_x), _x))),
  interp(_x, cons(undef(), _x)),
  interp(_x, cons(_x, cons(undef(), _x))),
  interp(_x, cons(_x, cons(_x, cons(_x, _x))))]

