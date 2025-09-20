
val is_perfect : int -> bool
(** [is_perfect n] is true if and only if the sum of the proper divisors of [n] is equal to [n].
    Assumes [n] is positive. *)

val is_prime : int -> bool
(** [is_prime n] is true if and only if [n] is a prime number. Assumes [n] is positive.*)

val are_buddies : int -> int -> bool
(** [are_buddies n m] is true if the sum of the prime divisors of [n] and [m], with multiplicity,
    are equal, where [n] and [m] are positive. *)
