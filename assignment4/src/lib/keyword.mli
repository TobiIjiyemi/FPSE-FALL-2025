
type t [@@deriving compare, sexp]
(** [t] is a comparable, serializable type to represent an OCaml keyword.
    Comparison is alphabetical on the string respresentation of the keyword. *)

val of_string : string -> t option
(** [of_string candidate] is some keyword if [candidate] is a valid OCaml keyword,
    or [None] if it is not. *)
