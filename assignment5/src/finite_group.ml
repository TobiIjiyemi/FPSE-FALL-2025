(*
  FPSE Assignment 5 

  Name                  :
  List of collaborators :

  See `finite_group.mli` for a lengthy explanation of this assignment. Your answers go in this file.

  We provide a little bit of code here to help you get over the syntax hurdles.

  The total amount of code here can be very short. The difficulty is in understanding modules and functors.
*)

open Core

(*
  Copy all module types from `finite_group.mli` and put them here.   
*)

(* ... module types here ... *)

(*
  Now write your functors below. There will be errors for unbound module types until you put the module types above.
*)

module Make : MAKE = functor (Operable : OPERABLE) -> struct

  (* ... your implementation here ... *)

end

module Make_precomputed : MAKE = functor (Operable : OPERABLE) -> struct
  
  (* ... your implementation here ... *)

end
