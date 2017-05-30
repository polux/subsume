-- Copyright 2015 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
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

module Examples (interp, balance, numadd, cars, extendedSyntax) where

interp = "\
\CONSTRUCTORS\n\
\\n\
\True : Bool\n\
\False : Bool\n\
\Z : Nat\n\
\S : Nat -> Nat\n\
\\n\
\Nil : List\n\
\Cons : Val * List -> List\n\
\\n\
\Nv : Nat -> Val\n\
\Bv : Bool -> Val\n\
\Undef : Val\n\
\\n\
\FUNCTIONS\n\
\\n\
\not : Bool -> Bool\n\
\or : Bool * Bool -> Bool\n\
\inf : Nat * Nat -> Bool\n\
\plus : Nat * Nat -> Nat\n\
\interp : Nat * List -> Val\n\
\\n\
\RULES\n\
\\n\
\interp(Z(), Nil())                                             -> Nv(Z())\n\
\interp(S(Z()), Cons(Nv(n),Nil()))                              -> Nv(S(n))\n\
\interp(S(S(Z())), Cons(Nv(m),Cons(Nv(n),Nil())))               -> Nv(plus(m,n))\n\
\interp(S(S(S(Z()))), Nil())                                    -> Bv(True())\n\
\interp(S(S(S(S(Z())))), Cons(Bv(b),Nil()))                     -> Bv(not(b))\n\
\interp(S(S(S(S(S(Z()))))), Cons(Bv(b1),Cons(Bv(b2),Nil())))    -> Bv(or(b1,b2))\n\
\interp(S(S(S(S(S(S(Z())))))), Cons(Nv(n1),Cons(Nv(n2),Nil()))) -> Bv(inf(n1,n2))\n\
\interp(x,y)                                                    -> Undef()\n\
\\n\
\plus(x,Z())    -> x\n\
\plus(x,S(y))   -> S(plus(x,y))\n\
\\n\
\inf(Z(),S(x))  -> True()\n\
\inf(S(x),S(y)) -> inf(x,y)\n\
\inf(x,y)       -> False()\n\
\\n\
\not(True())    -> False()\n\
\not(False())   -> True()\n\
\\n\
\or(True(),x)   -> True\n\
\or(x,True())   -> True\n\
\or(False(),x)  -> x\n\
\or(x,False())  -> x"

balance = "\
\CONSTRUCTORS\n\
\\n\
\E : Tree\n\
\T : Color * Tree * Nat * Tree -> Tree\n\
\\n\
\R : Color\n\
\B : Color\n\
\\n\
\Z : Nat\n\
\S : Nat -> Nat\n\
\\n\
\FUNCTIONS\n\
\\n\
\balance : Tree -> Tree\n\
\\n\
\RULES\n\
\\n\
\balance(T(B(),T(R(),T(R(),a,x,b),y,c),z,d)) -> T(R(),T(B(),a,x,b),y,T(B(),c,z,d))\n\
\balance(T(B(),T(R(),a,x,T(R(),b,y,c)),z,d)) -> T(R(),T(B(),a,x,b),y,T(B(),c,z,d))\n\
\balance(T(B(),a,x,T(R(),T(R(),b,y,c),z,d))) -> T(R(),T(B(),a,x,b),y,T(B(),c,z,d))\n\
\balance(T(B(),a,x,T(R(),b,y,T(R(),c,z,d)))) -> T(R(),T(B(),a,x,b),y,T(B(),c,z,d))\n\
\balance(t) -> t"

numadd = "\n\
\CONSTRUCTORS\n\
\\n\
\C : Nat -> T\n\
\Bound : Nat -> T\n\
\Neg : T -> T\n\
\Add : T * T -> T\n\
\Sub : T * T -> T\n\
\Mul : Nat * T -> T\n\
\\n\
\Z : Nat\n\
\S : Nat -> Nat\n\
\\n\
\FUNCTIONS\n\
\\n\
\numadd : T * T -> T\n\
\\n\
\RULES\n\
\\n\
\numadd(Add(Mul(c1,Bound(n1)),r1), Add(Mul(c2,Bound(n2)),r2)) -> C(Z())\n\
\numadd(Add(Mul(c1,Bound(n1)),r1), t) -> C(Z())\n\
\numadd(t, Add(Mul(c2,Bound(n2)),r2)) -> C(Z())\n\
\numadd(C(b1), C(b2)) -> C(Z())\n\
\numadd(a, b) -> C(Z())"

cars = "\
\CONSTRUCTORS\n\
\\n\
\car : engine * type -> car\n\
\\n\
\electric : engine\n\
\diesel : engine\n\
\hybrid : engine\n\
\gas : engine\n\
\\n\
\family : type\n\
\van : type\n\
\suv : type\n\
\\n\
\red : color\n\
\blue : color\n\
\white : color\n\
\\n\
\FUNCTIONS\n\
\\n\
\paint : car -> color\n\
\\n\
\RULES\n\
\\n\
\paint(car(electric(), !suv())) -> blue()\n\
\paint(car(!diesel(), !suv())) -> white()\n\
\paint(x) -> red()"

extendedSyntax = "\
\CONSTRUCTORS\n\
\\n\
\A : T\n\
\B : T -> T\n\
\C : T -> T\n\
\D : T -> T\n\
\\n\
\FUNCTIONS\n\
\\n\
\f : T -> T\n\
\\n\
\RULES\n\
\\n\
\g( (B(x) + C(y)) \\ (B(C(z)) + C(B(w))) ) -> A()"
