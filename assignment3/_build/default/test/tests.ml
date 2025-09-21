
open OUnit2

module Student_tests = struct
  (* Disables "unused open" from dune. Delete this after you have written your tests. *)
  [@@@ocaml.warning "-33"]

  (*
    Replace these tests with your own. You must have at least ten `assert_equal`
    statements that are not redundant with the provided tests.

    Notice how in the provided tests, we only assert equality on atomic, primitive values.
    We never assert equality on values with any sort of structure. This is because by default,
    OUnit2 uses polymorphic comparison: values are compared by their structure. Two dictionaries
    may have the same mappings as each other, but if one is a rotated version of the other,
    then OUnit2 will not tell them apart.

    Make sure that your tests here only assert equality on values where the structure is simple,
    or pass in an equality function to the `?cmp` optional argument.
  *)

  let dummy_test1 _ =
    assert_equal 0 0

  let dummy_test2 _ =
    assert_equal 0 0

  let series =
    "Student tests" >:::
    [ "Dummy test 1" >:: dummy_test1
    ; "Dummy test 2" >:: dummy_test2 ]
end

let series =
  "Assignment2 Tests" >:::
  [ Student_tests.series
  ; Tree_tests.series
  ; Dict_tests.series ]

(* The following line runs all the tests put together into `series` above *)

let () = run_test_tt_main series