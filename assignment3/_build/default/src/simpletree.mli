(* 
  This file specifies the interface for your code and must not be edited (it will not be included in your zip submission). 
  Your actual implementation should go in the file `simpletree.ml` which satisfies this interface appropriately.

  Mutation operations of OCaml are not allowed or required.
*)

type 'a t =
  | Leaf
  | Branch of
      { item  : 'a
      ; left  : 'a t
      ; right : 'a t } [@@deriving show]

val size : 'a t -> int
(** [size t] is the number of branches in the tree [t]. Leaves do not count towards size. *)

val height : 'a t -> int
(** [height t] is the length of the longest root-to-leaf path in [t]. *)

val is_balanced : 'a t -> bool
(** [is_balanced t] is true if for every branch in [t], the heights of the left and right subtrees do not differ by more than 1. *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(** [map t f] is a tree where [f] has been applied to every value in [t], where the shape
    of the tree is the same as [t]. *)

val fold_left : 'a t -> acc:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
(** [fold_left t acc f] is left-to-right in-order folding of the branches in [t], where [acc] is the base case
    for folding function [f]. 
   
    E.g. fold_left t ~acc:0 ~f:(+) where t is

          3
        /  \
       1    4
      / \    \
     0   2    5

     computes (((((0 + 0) + 1) + 2) + 3) + 4) + 5
  *)

val is_ordered : 'a t -> compare:('a -> 'a -> int) -> bool
(** [is_ordered t ~compare] is true if for every branch in [t], all left subtree items are strictly less than the branch item, and all 
    right subtree items are strictly greater.
    Note that this requirement guarantees (by induction) that the tree has no duplicate items. *)
