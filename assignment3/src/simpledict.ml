(*
  FPSE Assignment 3

  Name                  : Tobi Ijiyemi
  List of Collaborators : Vrinda Seghal

  Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.

  See file simpledict.mli for the specification of this assignment. Recall from lecture that .mli files are module signatures (aka module types) and in this file you will need to provide implementations of all the functions listed there.

  Note that .ml files need to include all `type` declarations in .mli files.

  You must leave all `[@@deriving show]` annotations, or your autograder won't work. We use this to pretty-print your results.
*)

open Core

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

let unimplemented () =
  failwith "unimplemented"

type 'a t = 'a Dict_item.t Simpletree.t [@@deriving show]

let empty : 'a t = Simpletree.Leaf

(* 
  We provide `size` for you to demonstrate that the Simpletree module functions work on the dict
  since the dict is a Simpletree.t.

  Utilize this when implementing the rest of the functions. For example, `map` should be easy to
  implement on dictionaries becacuse it's already done on trees.
*)
let size (dict : 'a t) : int =
  Simpletree.size dict

let map (dict : 'a t) ~(f : string -> 'a -> 'b) : 'b t =
  unimplemented ()

  (* grab value of dict_item at current node, compare dict_item key with
  current key... if smaller, equal, or greater then recursively call with
    appropriate 
    *)
let lookup (dict : 'a t) ~(key : string) : 'a option =
  unimplemented ()

let insert (dict : 'a t) ~(key : string) ~(value : 'a) : 'a t =
  unimplemented ()

let keys (dict : 'a t) : string list =
  unimplemented ()

let update (dict : 'a t) ~(key : string) ~(f : 'a option -> 'a) : 'a t =
  unimplemented ()

let merge (a : 'a t) (b : 'a t) : 'a t =
  unimplemented ()

let combine (a : 'a t) (b : 'b t) ~(f : string -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c) : 'c t =
  unimplemented ()
