(*
Copyright 2015 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License")*
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

Require Import Bool.
Require Import List.
Require Import Arith.Compare_dec.

Variable A : Set.
Variable A_eq_dec : forall (x y : A), {x = y}+{x <> y}.
Variable P : A -> list A -> Prop.
Variable P_dec : forall x xs, {P x xs}+{~P x xs}.

Hypothesis P_subset: forall x xs xs', incl xs xs' -> P x xs -> P x xs'.
Hypothesis P_trans: forall x y xs, P x (y::xs) /\ P y xs -> P x xs.

Definition smallest (xs : list A) (ys : list A):= if le_gt_dec (length xs) (length ys) then xs else ys.

Fixpoint Algo (xs: list A) (kernel: list A) : list A :=
  match xs with
  | nil => kernel
  | (x::xs) => if P_dec x (xs ++ kernel) 
                 then smallest (Algo xs (x::kernel)) (Algo xs kernel) 
                 else Algo xs (x::kernel)
  end.

Lemma incl_permut : forall (xs ys : list A) (x : A), incl (xs ++ x :: ys) (x :: xs ++ ys).
induction xs; intuition.
Qed.

Lemma incl_permut' : forall (xs ys : list A) (x : A), incl (x :: xs ++ ys) (xs ++ x :: ys).
induction xs; intuition.
Qed.

Lemma kernel_preserved : forall xs k1 k2, incl k1 k2 -> incl k1 (Algo xs k2).
induction xs.
simpl; intuition.
intros.
simpl.
destruct (P_dec a (xs++k2)).
unfold smallest.
destruct (le_gt_dec (length (Algo xs (a :: k2))) (length (Algo xs k2))).
apply IHxs.
unfold incl; intuition.
apply IHxs.
apply H.
apply IHxs.
unfold incl; intuition.
Qed.

Lemma kernel_preserved' : forall xs kernel, incl kernel (Algo xs kernel).
intros; apply kernel_preserved.
apply incl_refl.
Qed.

Lemma kernel_cons : forall x xs kernel, In x (Algo xs (x::kernel)).
intros.
assert (incl (x::kernel) (Algo xs (x::kernel))).
apply kernel_preserved'.
unfold incl in H; intuition.
Qed.

Theorem result_incl : forall xs kernel, incl (Algo xs kernel) (xs++kernel).
induction xs; simpl; intuition.
destruct (P_dec a (xs ++ kernel)).
  (* P a (xs++kernel) *)
  unfold smallest.
  destruct (le_gt_dec (length (Algo xs (a :: kernel))) (length (Algo xs kernel))).
     (* length (Algo xs (a :: kernel)) <= length (Algo xs kernel) *)
     apply incl_tran with (m := (xs ++ a :: kernel)).
     intuition.
     apply incl_permut.         
     (* length (Algo xs (a :: kernel)) > length (Algo xs kernel) *)
     apply incl_tran with (m := (xs ++ kernel)).
     intuition.
     unfold incl; intuition.
  (* ~ P a (xs++kernel) *)
  apply incl_tran with (m := (xs ++ a :: kernel)).
  intuition.
  apply incl_permut.         
Qed.

Lemma P_algo : forall xs kernel, forall x, P x (xs++kernel) -> P x (Algo xs kernel).
induction xs.

(* xs = nil *)
simpl; intuition.

(* xs = a :: xs *)
intros.
simpl.
destruct (P_dec a (xs++kernel)).
  (* P a (xs ++ kernel) *)
  unfold smallest.
  destruct (le_gt_dec (length (Algo xs (a :: kernel))) (length (Algo xs kernel))).
    (* length (Algo xs (a :: kernel)) <= length (Algo xs kernel) *)
    apply IHxs.
    apply P_subset with (xs := ((a :: xs) ++ kernel)).
    apply incl_permut'.
    apply H.
    (* length (Algo xs (a :: kernel)) > length (Algo xs kernel) *)
    apply IHxs.
    apply P_trans with (y := a).
    rewrite <- app_comm_cons in H.
    intuition.
 (* ~ P a (xs ++ kernel) *)
 apply IHxs.
 apply P_subset with (xs := ((a :: xs) ++ kernel)).
 apply incl_permut'.
 apply H.
Qed.

Theorem pre_correctness : forall xs kernel, forall x, 
  In x xs -> In x (Algo xs kernel) \/ P x (Algo xs kernel).
induction xs.

(* xs = nil *)
simpl; intuition.

(* xs = a :: xs *)
intros.
simpl.
destruct (P_dec a (xs++kernel)).
  (* P a (xs ++ kernel) *)
  unfold smallest.
  destruct (le_gt_dec (length (Algo xs (a :: kernel))) (length (Algo xs kernel))).
    (* length (Algo xs (a :: kernel)) <= length (Algo xs kernel) *)
    simpl in H.
    destruct H.    
      (* a = x *)
      subst.
      left.
      apply kernel_cons.
      (* a != x *)
      intuition.
    (* length (Algo xs (a :: kernel)) > length (Algo xs kernel) *)
    simpl in H.
    destruct H.
      (* a = x *)
      subst.
      right.
      apply P_algo.
      apply p.
      (* a != x *)
      intuition.
  (* ~ P a (xs ++ kernel) *)
  simpl in H.
  destruct H.    
    (* a = x *)
    subst.
    left.
    apply kernel_cons.
    (* a != x *)
    intuition.
Qed.

Theorem correctness  : forall xs, 
  let sol := (Algo xs nil)
  in incl sol xs 
     /\ (forall x, In x xs -> In x sol \/ P x sol).
intuition.
split.
assert (incl (Algo xs nil) (xs++nil)).
apply result_incl.
rewrite app_nil_r in H; trivial.
apply pre_correctness.
Qed.
