(*
  FPSE Assignment 4

  Name                  :
  List of Collaborators :

  See `histogram.mli` for the requirements. Your answers go in this file.

  To get this project to build, you should first fill in some dummy headers for each function
  declared by the .mli file, like we do for you with `increment`. Once you have something for
  each declared value, and they have the correct types, then the project should build, and you
  can begin implementing the correct behavior.
*)

open Core

(* This is to silence the warning for unused variables until everything here is implemented. *)
[@@@ocaml.warning "-27"]

let unimplemented () =
  failwith "unimplemented"

(*
  Here we use a functor. `Keyword_map` is a module for mapping keywords to values. You will make a functor
  in the next assignment. Here, you can just take for granted that this works.

  Check the module signatures for both `Map` and `Keyword_map`. Some useful functions will be inside
  `Keyword_map`, and some will be inside `Map`.
*)
module Keyword_map = Map.Make (Keyword)

type t = int Keyword_map.t [@@deriving compare]

let empty : t = Keyword_map.empty

let increment (hist : t) (key : Keyword.t) : t =
  unimplemented ()

(* ... fill in the rest of the module below to satisfy the mli, except sexp_of_t ... *)

(*
  We implement this for you to help you avoid simple autograder failures. However, the inverse,
  `t_of_sexp` is still up to you.

  This relies on your implementation of `to_ordered_alist`.

  If we did not care about ordering in the string representation, we would use `Keyword_map.sexp_of_t`.
  However, the autograder will compare strings and needs an ordered serialized histogram.
*)
let sexp_of_t (hist : t) : Sexp.t =
  hist 
  |> to_ordered_alist
  |> List.sexp_of_t (Tuple2.sexp_of_t Keyword.sexp_of_t Int.sexp_of_t)
