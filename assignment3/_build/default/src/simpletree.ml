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

let size (tree : 'a t) : int =
  unimplemented ()

let height (tree : 'a t) : int =
  unimplemented ()

let is_balanced (tree : 'a t) : bool =
  unimplemented ()

let map (tree : 'a t) ~(f : 'a -> 'b) : 'b t =
  unimplemented ()

let fold_left (tree : 'a t) ~(acc : 'acc) ~(f : 'acc -> 'a -> 'acc) : 'acc =
  unimplemented ()

let is_ordered (tree : 'a t) ~(compare : 'a -> 'a -> int) : bool =
  unimplemented ()
