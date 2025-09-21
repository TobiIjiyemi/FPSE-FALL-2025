(*
  The Dict_item module describes a type so that a Tree can represent a dictionary. 
  The type holds a key and a value. The key is always a string, and the type of the
  value is the type parameter 'a.
*)


type 'a t = { key : string ; value : 'a } [@@deriving show]
(** For simplicity, we restrict keys to be strings only; the values are type 'a. *)

val compare : 'a t -> 'a t -> int
(** [compare a b] is [String.compare a.key b.key]. Values are not compared. *)
