(*
  FPSE Assignment 3

  Name                  : 
  List of Collaborators :

  Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.

  See file simpletree.mli for the specification of this assignment. Recall from lecture that .mli files are module signatures (aka module types) and in this file you will need to provide implementations of all the functions listed there.

  Note that .ml files need to include all `type` declarations in .mli files. Hence we have copied over `'a t`.

  You must leave all `[@@deriving show]` annotations. Otherwise, your autograder won't work. We use this to pretty-print your results.
*)

open Core

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

let unimplemented () = failwith "unimplemented"

type 'a t =
  | Leaf
  | Branch of
      { item  : 'a
      ; left  : 'a t
      ; right : 'a t } [@@deriving show]

let rec size (tree : 'a t) : int =
  match tree with
  | Branch a -> 1 + size a.left + size a.right
  | Leaf -> 0;;

let rec height (tree : 'a t) : int =
  match tree with
  | Branch a -> 1 + max (height a.left) (height a.right)
  | Leaf -> 0;;

let rec is_balanced (tree : 'a t) : bool =
  match tree with
  | Branch a -> 
    if abs begin height a.left - height a.right end > 1 then false
    else is_balanced a.left && is_balanced a.right
  | Leaf -> true;;

let rec map (tree : 'a t) ~(f : 'a -> 'b) : 'b t =
  match tree with 
    | Leaf -> Leaf 
  | Branch a -> Branch {item = f a.item ; left = map a.left ~f; right = map a.right ~f};;

let rec fold_left (tree : 'a t) ~(acc : 'acc) ~(f : 'acc -> 'a -> 'acc) : 'acc =
  match tree with 
  | Leaf -> acc
  | Branch a -> 
    let left_acc = fold_left a.left ~acc:acc ~f in
    (* le left_acc = f new_acc  *)
    let res_acc = f left_acc a.item in
    fold_left a.right ~acc:res_acc ~f;;

let is_ordered (tree : 'a t) ~(compare : 'a -> 'a -> int) : bool =
  unimplemented ()
