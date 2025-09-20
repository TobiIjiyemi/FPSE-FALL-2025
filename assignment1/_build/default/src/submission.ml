(*
  FPSE Assignment 1

  Name                  :
  List of Collaborators :

  Please make a good faith effort at listing people you discussed any problems with here, as per the course academic integrity policy.  CAs/Prof need not be listed!

  Note that it is strictly illegal to look for direct answers to these questions using search or AI tools.  For example asking ChatGPT "how do I implement a least common multiple function in OCaml" is illegal.

  Fill in the function definitions below by replacing the 

    unimplemented ()

  with your code. You may add `rec` to any function to make it recursive. You may define any auxillary functions you'd like.

  You must not use any mutation operations of OCaml in this assignment (which we have not taught yet in any case): no arrays, for- or while-loops, references, etc. Also, you may not use the `List` module functions in this assignment, but you may use other standard libraries. In the next assignment, we will start using `List`.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(*
  You are required to use core in this class. Don't remove the following line.  If the editor is not recognizing Core (red squiggle under it for example), run a "dune build" from the shell -- the first time you build it will create some .merlin files which tells the editor where the libraries are.
*)
open Core

(*
  Here is a simple function which gets passed unit, (), as argument and raises an exception. It is the initial implementation of all functions below.
*)
(* let unimplemented () =
	failwith "unimplemented" *)

(*
  Given a list of integers, return a new list with consecutive dupliates removed.
*)
let compress (ls : int list) : int list =
  let rec loop ls last cnt = 
    match ls with 
    | [] -> []
    | hd :: tl -> 
      if cnt = 0 then hd :: loop tl hd (cnt+1)
      else
        if hd = last then loop tl last (cnt+1)
        else hd :: loop tl hd (cnt+1)
    in loop ls 0 0;;

(*
	All functions must be total for the specified domain;	overflow is excluded from this restriction but should be avoided.
*)

(*
  Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion (don't use the closed-form solution, do the actual addition).
*)
let rec summate (n : int) : int =
  match n with
    | 0 -> 0
    | n -> n + summate(n-1);;

(*
  Given non-negative integers `n` and `m`, compute their least common multiple.
*)

(* let rec is_prime n prime divisor =
  if n < (divisor * divisor) || not prime then true
  else 
    if n % divisor = 0 then false
    else is_prime n prime (divisor+1) *)

(* let primes n = 
  let rec step ceil cnt = 
  if cnt > ceil then []
  else
    if is_prime cnt true 2 then cnt :: step ceil (cnt+1)
    else step ceil (cnt + 1)
  in step n 2;; *)

(* let rec prime_factors ls n res= 
  match ls with
  | [] -> []
  | hd :: tl -> 
    if n < 2 then res
    else
      if n % hd = 0 then hd :: prime_factors ls (n/hd) res
      else prime_factors tl n res;; *)

(* let rec union ls1 ls2 = 
  match ls1 with 
  | [] -> ls2
  | hd :: tl -> 
    match ls2 with 
    | [] -> ls1
    | x :: y -> if hd = x then hd :: union tl y
      else hd :: union tl ls2;; *)

let rec gcf n m = 
if m = 0 then n else gcf m (n mod m);;

let lcm (n : int) (m : int) : int =
  n * m / gcf n m;;
  (* let rec product ls p =
    match ls with 
    | [] -> p
    | hd :: tl -> product tl (p*hd)
  in product (union (prime_factors (primes n) n []) (prime_factors (primes m) m []) ) 1;; *)

(*
  Given a non-negative integer `n`, compute the n-th fibonacci number.	Give an implementation that does not take exponential time; the naive version from lecture is exponential	since it has two recursive calls for each call.
*)
let fibonacci (n : int) : int =
  if n = 0 then 0
  else
  let rec help n k sum = 
    if n = 1 then sum
    else help (n-1) sum (sum+k)
    in help n 0 1;;

(*
  Given non-negative integers `a` and `b`, where `a` is not greater than `b`, produce a list [a; a+1; ...; b-1].
*)
let rec range (a : int) (b : int) : int list =
  if a > b-1 then []
  else a :: range (a+1) b;;
(*
  Given integers `n`, `d`, and non-negative `k`, produce the arithmetic progression [n; n + d; n + 2d; ...; n + (k-1)d].
*)
let arithmetic_progression (n : int) (d : int) (k : int) : int list =
  let rec loop x k = 
    if x > k-1 then [] else n+(x*d) :: loop (x+1) k
  in loop 0 k;;

(*
  Given non-negative integer `n`, produce the list of integers in the range (0, n] which it is divisible by, in ascending order.
*)
let factors (n : int) : int list =
  let rec loop x n =
    if x > n then [] 
    else if n%x=0 then x :: loop (x+1) n
    else loop (x+1) n
  in loop 1 n;;

  (* 
  Reverse a list. Your solution must be in O(n) time. Note: the solution in lecture is O(n^2).
*)
let reverse (ls : 'a list) : 'a list =
  let rec loop ils res = 
    match ils with 
    | [] -> res
    | hd :: tl -> loop tl (hd :: res)
  in loop ls [];;
      
(*
  Given a list of lists, flatten it into a single list.

  Example:
    `flatten [[1; 2]; [3]; [4; 5]]` is `[1; 2; 3; 4; 5]`
*)
let rec flatten (lss : 'a list list) : 'a list =
  match lss with 
  | [] -> []
  | hd :: tl -> hd @ flatten tl;;


(*
  Given a list of strings, check to see if it is ordered, i.e. whether earlier elements are less than or equal to later elements.
*)

let rec check_order lst comp =
  match lst with
  | [] -> true
  | head :: tail -> 
    if String.compare comp head < 0 then check_order tail comp
    else false;;

let rec is_ordered (ls : string list) : bool =
  match ls with 
  | [] -> true
  | hd :: tl -> 
    if check_order tl hd then is_ordered tl
    else false;;

(*
  Given a non-empty list of strings, return the lexicographically greatest string from the list, as well as the list without that greatest string.

  If there are duplicates of the greatest string, remove the instance that occurs last in the list.
*)

let break_up tup addend =
  match tup with 
  | (a,b) -> (a, addend :: b);;

let reverse_back lst =
  let x,y = lst in 
  x,reverse y;;
  
let remove_max (ls : string list) : string * string list =
  let ls = reverse ls in match ls with
  | [] -> ("", [""])
  | head :: tail -> 
    let rec check_max lst comp =
    match lst with
    | [] -> (comp, lst)
    | hd :: tl -> 
      if String.compare hd comp >= 0 then 
        break_up (check_max tl hd) comp
      else break_up (check_max tl comp) hd
    in reverse_back (check_max tail head) ;;

(*
  Sort a string list using selection sort. Your solution must have time complexity O(n^2). Note that time complexity will depend on your implementation of `remove_max`.

  The resulting list must be sorted from smallest to largest string lexicographically.
*)
let selection_sort (ls : string list) : string list =
  let rec loop lst res =
  match lst with 
  | [] -> res
  | hd :: tl -> 
    let x,y = remove_max lst in loop y (x :: res)
  in loop ls [];;


type 'a bin_tree =
  | Leaf of {data: 'a}
  | Node of {left: 'a bin_tree; right: 'a bin_tree};;

let simple = Leaf {data = 5};;
let simple = Node {left = simple; right = simple};;

(* let summate bt = 
match bt with 
| {Leaf} *)