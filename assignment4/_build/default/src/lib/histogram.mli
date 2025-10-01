
(*
  To assist in writing the keywordcount executable, we would like to use a histogram module to help count the keywords and display the results nicely.

  The histogram will map keywords to the counts. This will use a functional map; in the last assignment, you made a map, and in this assignment, you'll use Core's map.
  
  This file describes the module. Implement it in `histogram.ml`. We have done some setup there for you.
*)

open Core

type t [@@deriving compare]

val empty : t
(** [empty] is a histogram with nothing in it. *)

val increment : t -> Keyword.t -> t
(** [increment t key] has the count associated with [key] in [t] increased by 1. *)

val increment_many : t -> Keyword.t list -> t
(** [increment t keys] has the count in [t] increased by 1 for each key for each
    occurance of the key in [keys]. *)

val to_ordered_alist : t -> (Keyword.t * int) list
(** [to_ordered_alist t] is an association list of key,value pairs in [t], ordered
    high-to-low by count and then low-to-high by keyword when counts are tied. *)

val of_alist : (Keyword.t * int) list -> t
(** [of_alist ls] is a histogram of the key,value pairs in the association list [ls],
    where no key shows up more than once in [ls]. *)

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t t] is the s-expression for [to_ordered_alist t]. *)

val t_of_sexp : Sexp.t -> t
(** [sexp_of_t] is the inverse of [t_of_sexp]. *)

val to_string : t -> string
(** [to_string t] is [sexp_of_t t] as a string. *)

