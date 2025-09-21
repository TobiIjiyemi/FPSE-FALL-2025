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

(* let unimplemented () =
  failwith "unimplemented" *)

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
  List.rev begin Simpletree.fold_left dict ~acc:[] ~f:(fun x y -> y.key :: x) end ;;

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

let in_dict (dict : 'a t) (key : string): bool =
  match lookup dict ~key:key with
  | Some a -> true
  | None -> false;;

let merge (a : 'a t) (b : 'a t) : 'a t =
  (* keys of a, plus the keys of b... insert the keys. loop through a's keys, if
    it's in b then use b's value and if itn's not use b's value...
    and then loop through the b's keys, and add all the keys
      *)
  let a_keys = keys a in
  let b_keys = keys b in
  let rec loop1 first_d second_d first_key res =
    match first_key with
    | [] -> res
    | hd :: tl -> 
      if in_dict second_d hd then
      loop1 first_d second_d tl begin insert res ~key:hd ~value: 
      begin Option.value_exn (lookup second_d ~key:hd) end
      end
        (*build the branch and keep going*)
      else
        loop1 first_d second_d tl begin insert res ~key:hd ~value:(
          Option.value_exn begin lookup first_d ~key:hd end
          ) end
      in 
      let curr_dict = loop1 a b a_keys empty in
    let rec loop2 first_d second_d second_key res = 
      match second_key with
    | [] -> res
    | hd :: tl -> 
      loop2 first_d second_d tl begin insert res ~key:hd ~value:(
      Option.value_exn begin 
      lookup second_d ~key:hd
      end
      ) end
    in loop2 a b b_keys curr_dict;;

let combine (a : 'a t) (b : 'b t) ~(f : string -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c) : 'c t =
let all_keys = List.sort ~compare:String.compare (keys a @ keys b) in
  let rec loop key_list acc =
    match key_list with
    | [] -> acc
    | key :: tl ->
      let val_a = lookup a ~key in
      let val_b = lookup b ~key in
      let next_acc =
        match val_a, val_b with
        | Some v_a, Some v_b -> insert acc ~key ~value:(f key (`Both (v_a, v_b)))
        | Some v_a, None -> insert acc ~key ~value:(f key (`Left v_a))
        | None, Some v_b -> insert acc ~key ~value:(f key (`Right v_b))
        | None, None -> acc
      in
      loop tl next_acc
  in
  loop all_keys empty
;;
