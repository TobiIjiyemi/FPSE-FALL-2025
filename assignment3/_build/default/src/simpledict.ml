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

let rec map (dict : 'a t) ~(f : string -> 'a -> 'b) : 'b t =
  match dict with
  | Leaf -> Leaf 
  | Branch a -> Branch {item = {key = a.item.key; value = f a.item.key a.item.value}
  ; left = map a.left ~f; right = map a.right ~f};;

  (* grab value of dict_item at current node, compare dict_item key with
  current key... if smaller, equal, or greater then recursively call with
    appropriate 
    *)
let rec lookup (dict : 'a t) ~(key : string) : 'a option =
  match dict with
  | Leaf -> None
  | Branch a -> 
    let curr_key = a.item.key in
    if String.(key = curr_key) then
      Some a.item.value
    else
      if String.(key > curr_key) then
        lookup a.right ~key
      else
        lookup a.left ~key;;

let rec insert (dict : 'a t) ~(key : string) ~(value : 'a) : 'a t =
  match dict with
  | Leaf -> Branch {item = {key = key; value = value}; left = Leaf; right = Leaf}
  | Branch a ->
    let d_key = a.item.key in
    let d_val = a.item.value in
    if String.(key = d_key) then
      Branch {item = {key = d_key; value = value}; left = a.left; right = a.right}
    else
      if String.(key > d_key) then
        Branch {item = {key = d_key; value = d_val}; left = a.left; right = 
        begin insert a.right ~key ~value end}
      else
        Branch {item = {key = d_key; value = d_val}; left = begin
        insert a.left ~key ~value 
        end; right = a.right};;

let keys (dict : 'a t) : string list =
  let rec traverse (dict : 'a t) (res : string list) : string list =
    match dict with
    | Leaf -> res
    | Branch a ->
      let dkey = a.item.key in
      let new_res = dkey :: traverse a.left res in
      traverse a.right new_res
    in traverse dict [];;
      (* traverse a.left res :: res;; *)

let rec update (dict : 'a t) ~(key : string) ~(f : 'a option -> 'a) : 'a t =
  match dict with
  | Leaf -> Branch {item = {key = key; value = f None}; left = Leaf; right = Leaf}
  | Branch a ->
    let d_key = a.item.key in
    let d_val = a.item.value in
    if String.(key = d_key) then
      Branch {item = {key = d_key; value = f (Some d_val) }; left = a.left; right = a.right}
    else
      if String.(key > d_key) then
        Branch {item = {key = d_key; value = d_val }; left = a.left; right = (update a.right ~key ~f)}
      else
        Branch {item = {key = d_key; value = d_val}; left = (update a.left ~key ~f); right = a.right};;

let merge (a : 'a t) (b : 'a t) : 'a t =
  unimplemented ()

let combine (a : 'a t) (b : 'b t) ~(f : string -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c) : 'c t =
  unimplemented ()
