(* 
  This file specifies the interface for your code and must not be edited (it will not be included in your zip submission). 
  Your actual implementation should go in the file `simpledict.ml` which satisfies this interface appropriately.

  Mutation operations of OCaml are not allowed or required.
*)

type 'a t = 'a Dict_item.t Simpletree.t 
(* [@@deriving show] *)
(** The dict type is a tree of ['a Dict_item.t]. Note that because this type is a tree, all [Simpletree] module functions will work on it.
    Like function applications, type parameters bind tightly, so this type is the same as [('a Dict_item.t) Simpletree.t].
  
    You may ignore the `[@@deriving show]`. You will not need to work with it, and it is to pretty-print your autograder results.
    Just don't remove it in your .ml file. *)

val empty : 'a t
(** [empty] is an empty dictionary. *)

(*
  We will now define several natural operations on these dictionaries.

  IMPORTANT:

    We will implicitly require all dicts **provided to and created by** the functions below to obey the
    `Simpletree.is_ordered` requirement so that the keys are ordered.
  
  You do not need to check that the dicts are ordered when they are provided to a function.

  The autograder for this assignment will check that all dicts resulting from these functions are ordered.
*)

val size : 'a t -> int
(** [size t] is the number of key, value pairs in [t]. *)

val map : 'a t -> f:(string -> 'a -> 'b) -> 'b t
(** [map t ~f] is a dictionary where every value in [t] has been mapped by [f]. The keys stay constant.
    The mapping function [f] can use the key in its calculation, so we pass it as an argument. *)

val lookup : 'a t -> key:string -> 'a option
(** [lookup t ~key] is the associated value to [key] in [t], if any.
    This must be O(log n) time complexity if [t] is balanced for n nodes in [t]. *)

val insert : 'a t -> key:string -> value:'a -> 'a t
(** [insert t ~key ~value] is a dictionary with [key] mapping to [value] in [t], overwriting any existing
    value attached to the key in [t] if present.
    This should also be O(log n) time complexity if [t] is balanced, as explained in lecture. *)

val keys : 'a t -> string list
(** [keys t] are the keys in [t], in sorted order. This is O(n) time complexity; recall that each [::] is O(1)
    and each [@] is O(n). *)

val update : 'a t -> key:string -> f:('a option -> 'a) -> 'a t
(** [update t ~key ~f] applies [f] to the value in [t] associated with [key], if it exists. If [key] is
    unbound in [t], then [f None] is inserted as the value for [key]. *)

val merge : 'a t -> 'a t -> 'a t
(** [merge a b] is a dictionary containing all key, value pairs in [a] and [b]. If both contain a value associated
    to a key, then [b]'s value is retained, and [a]'s value is overwritten. *)

val combine : 'a t -> 'b t -> f:(string -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c) -> 'c t
(** [combine a b] is a dictionary containing all keys from [a] and [b] whose elements have been merged using [f]. 
    The merging function uses polymorphic variants to handle keys that are only in the left dictionary [a], the
    right dictionary [b], or are in both dictionaries. *)