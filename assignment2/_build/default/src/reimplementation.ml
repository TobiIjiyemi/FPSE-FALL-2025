(*
  FPSE Assignment 2

  Name                  :
  List of Collaborators :

  Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  CAs/Prof need not be listed!

	------------
	INSTRUCTIONS
	------------

  For selected functions in Assignment 1, provide a reimplementation of your previous code by refactoring the definition to use combinators provided by the List module.

  Care should be taken to use a concise, elegant combination of these provided functions to best express the task.

  These new implementations should be not explicitly recursive. Note that the autograder is not aware if you cheated and used `rec`; we will manually inspect your code and give you negative points if you used recursion.

	Define any auxiliary functions you like, but you must not use the `rec` keyword anywhere.

  You must not use any mutation operations of OCaml in this assignment.
*)
open Core

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* let unimplemented () =
  failwith "unimplemented" *)

(*
	Refer to Assignment 1 for the expected behavior of these functions.	 
*)

(** [factors n] is all positive factors of [n]. *)
let factors (n : int) : int list =
  let nums = List.init n ~f:(fun i -> i + 1) in
  List.filter ~f:(fun x-> n mod x = 0) nums;;

(*
  Given integers `n`, `d`, and non-negative `k`, produce the arithmetic progression [n; n + d; n + 2d; ...; n + (k-1)d].
*)
let arithmetic_progression (n : int) (d : int) (k : int) : int list =
  List.init k ~f:(fun i -> n + i*d);;

let remove_max (ls : string list) : string * string list =
  
  let find_max = 
    match List.max_elt ls ~compare:String.compare with 
    | Some max -> max
    | None -> "" in
  let reversed_list = List.rev ls in
  match reversed_list with 
  | [] -> "", [""]
  | hd :: tl ->
    if String.(=) hd find_max then (find_max, List.rev tl) 
    else
    let state_tuple = List.fold tl ~init:(false,[]) 
    ~f:(fun (found, lsts) curr -> 
      if not found && String.(=) curr find_max then (true, lsts)
      else (found, curr :: lsts)
    ) in
  let res_list = snd state_tuple in
  let reverse_back = List.rev res_list in
  let total_list = hd :: reverse_back in
  (find_max, List.rev total_list);;

  (* let mx = match ls with
  | [] -> ""
  | hd :: tl -> List.fold tl ~init:(hd, false, []) ~f:(
    fun (max_rn, rem, lst) curr -> 
      if curr > max_rn then (curr, false, max_rn :: lst)
      else (max_rn, false, lst)
  );; *)

let flatten (lss : 'a list list) : 'a list =
  (*List.fold_right ~f:(@) ~init:[] lss;;*)
  List.fold ~f:(@) ~init:[] lss;;