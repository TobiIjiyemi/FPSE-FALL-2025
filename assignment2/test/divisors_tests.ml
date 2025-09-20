
open OUnit2
open Divisors

let test_is_prime _ =
  assert_equal (is_prime 2) true;
  assert_equal (is_prime 15) false;
  assert_equal (is_prime (Core.Int.pow 2 31 - 1)) true

let test_is_perfect _ =
  assert_equal (is_perfect 28) true;
  assert_equal (is_perfect 100) false;
  assert_equal (is_perfect 8128) true

let test_buddies _ =
  assert_equal (are_buddies 1971 1988) true;
  assert_equal (are_buddies 131072 236196) true;
  assert_equal (are_buddies 1971 1989) false

let series =
  "Divisors tests" >:::
  [ "Is Prime" >:: test_is_prime
  ; "Is Perfect" >:: test_is_perfect
  ; "Are buddies" >:: test_buddies ] 
