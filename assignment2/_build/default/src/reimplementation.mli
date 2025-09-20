
val factors : int -> int list
(** [factors n] is all positive factors of [n]. *)

val arithmetic_progression : int -> int -> int -> int list
(** [arithmetic_progression n d k] is the list [n; n + d; n + 2d; ...; n + (k-1)d], where [k] is non-negative. *)

val remove_max : string list -> string * string list
(** [remove_max ls] is the max element of [ls] and [ls] without that last instance of the max element. Assumes [ls] is non-empty. *)

val flatten : 'a list list -> 'a list
(** [flatten lss] flattens a list of lists into a single list. *)
