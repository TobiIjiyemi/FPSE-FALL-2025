
open Core

type 'a t = { key : string ; value : 'a } [@@deriving show]

(* This is implemented for you *)
let compare (x : 'a t) (y : 'a t) : int =
  String.compare x.key y.key
