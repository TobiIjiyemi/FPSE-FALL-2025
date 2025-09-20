(*
  FPSE Assignment 2

  Name                  :
  List of Collaborators :

  Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  CAs/Prof need not be listed!

  ------------
  INSTRUCTIONS
  ------------

  In this part of the assignment, you will implement a few functions based around the divisors of an integer. Provide implementations that meet the descriptions.

  You must not use any mutation operations of OCaml in this assignment. You may define any auxiliary functions you like, and you may add `rec` to any function.
*)

open Core

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* let unimplemented () =
  failwith "unimplemented" *)

(*
  Check if a positive number is perfect: a perfect number is equal to the sum of its proper divisors.

  A positive divisor of `n` is called "proper" if it is different from `n`.

  For example, 28 is a perfect number:
    1 + 2 + 4 + 7 + 14 = 28
*)
let summate ls = 
  match ls with
  | [] -> 0
  | hd :: tl -> List.fold tl ~init:hd ~f:(+);;

let is_perfect (n : int) : bool =
  let factors (n : int) : int list =
  let nums = List.init n ~f:(fun i -> i + 1) in
  List.filter ~f:(fun x-> n mod x = 0) nums in
  if summate (factors n) - n = n then true
  else false;;

(* 
  Check if positive integer `n` is prime. This should be able to quickly handle numbers up to 2^32.

  Performance hints:
    - You only need to rule out factors up to sqrt(n).
    - Use recursion instead of a list to save memory.
*)
let is_prime (n : int) : bool =
let sqrtn = sqrt (float_of_int n) in
 let rec loop acc n prime =
  if acc > int_of_float sqrtn then prime
  else
    if n mod acc = 0 then false
    else loop (acc+1) n prime
    in loop 2 n true;;
(*
  Positive integers `n` and `m` are buddies if the sum of their prime divisors, with multiplicity, are equal.

  `are_buddies` is true if and only if `n` and `m` are buddy numbers, under this definition.

  For example, 1971 and 1988 are buddies:
    1971 = 3^3 * 73
    1988 = 2^2 * 7 * 71

    3 + 3 + 3 + 73 = 82 = 2 + 2 + 7 + 71
*)

let primes n = 
  let rec loop curr div res =
    if curr <= 1 then res
    else
      if not (is_prime div) then loop curr (div+1) res
      else
        if curr % div = 0 then loop (curr/div) div (div::res)
        else loop curr (div+1) res 
      in loop n 2 [];;  

  let are_buddies (n : int) (m : int) : bool =
  if summate (primes n) = summate (primes m) then true
  else false
