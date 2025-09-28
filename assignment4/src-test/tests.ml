
(* open Core *)
open OUnit2

module Student_tests = struct
  (* Write your own non-redundant tests here for any part of the assignment. *)

  let series =
    "Student tests" >::: []
end

let series =
  "Assignment 4 tests" >:::
  [ Student_tests.series
  ; Hist_tests.series
  ; Exec_tests.series ]

let () = run_test_tt_main series