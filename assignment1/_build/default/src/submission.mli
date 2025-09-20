
val summate : int -> int
(** [summate n] is 0+1+2+...+n where [n] is non-negative. *)

val lcm : int -> int -> int
(** [lcm n m] is the least common multiple of [n] and [m]. *)

val fibonacci : int -> int
(** [fibonacci n] is the [n]-th fibonacci number. *)

val range : int -> int -> int list
(** [range a b] is the list [a..b-1] *)

val arithmetic_progression : int -> int -> int -> int list
(** [arithmetic_progression n d k] is the list [n; n + d; n + 2d; ...; n + (k-1)d], where [k] is non-negative. *)

val factors : int -> int list
(** [factors n] is all positive factors of non-negative [n]. *)

val reverse : 'a list -> 'a list
(** [reverse ls] is the list [ls] in reverse. *)

val flatten : 'a list list -> 'a list
(** [flatten lss] flattens a list of lists into a single list. *)

val compress : int list -> int list
(** [compress ls] is [ls] without consecutive duplicates. *)

val is_ordered : string list -> bool
(** [is_ordered ls] is true if and only if [ls] is in sorted order. *)

val remove_max : string list -> string * string list
(** [remove_max ls] is the max element of [ls] and [ls] without that last instance of the max element. Assumes [ls] is non-empty. *)

val selection_sort : string list -> string list
(** [selection_sort ls] is the list [ls] in sorted order using selection sort. *)
