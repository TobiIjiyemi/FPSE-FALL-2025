
open OUnit2
open Reimplementation

(*
  These tests are simply copied from Assignment 1.
*)

let test_factors _ =
  assert_equal (factors 10) [1; 2; 5; 10];
  assert_equal (factors 12) [1; 2; 3; 4; 6; 12];
  assert_equal (factors 0) []

let test_arithmetic_progression _ =
  assert_equal (arithmetic_progression 1 3 4) [1; 4; 7; 10];
  assert_equal (arithmetic_progression (-10) (-10) 2) [-10; -20];
  assert_equal (arithmetic_progression 0 0 0) []

let test_remove_max _ =
  assert_equal (remove_max ["apple"; "zebra"; "banana"]) ("zebra", ["apple"; "banana"]);
  assert_equal (remove_max ["cat"; "dog"; "cat"; "dog"; "cat"]) ("dog", ["cat"; "dog"; "cat"; "cat"])

let test_flatten _ =
  assert_equal (flatten [[1; 2]; [3]; [4; 5]]) [1; 2; 3; 4; 5];
  assert_equal (flatten [[]]) [];
  assert_equal (flatten [["hello"]; ["world"; "test"]]) ["hello"; "world"; "test"]

let series =
  "Reimplementation tests" >:::
  [ "Factors"       >:: test_factors
  ; "Arith. prog."  >:: test_arithmetic_progression
  ; "Remove max"    >:: test_remove_max
  ; "Flatten"       >:: test_flatten
  ]