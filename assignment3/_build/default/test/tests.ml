open Core
open OUnit2

module Student_tests = struct
  (* Disables "unused open" from dune. Delete this after you have written your tests. *)
  (* [@@@ocaml.warning "-33"] *)

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
module T = Simpletree
module D = Simpledict
module I = Dict_item

let dict_singleton k v =
  T.Branch { item = { I.key = k ; value = v } ; left = T.Leaf ; right = T.Leaf }

let d1 = T.Branch
  { item = { I.key = "b" ; value = 1 }
  ; left = dict_singleton "a" 2
  ; right = dict_singleton "c" 3 }

let test_tree_fold_on_empty _ =
  let result = T.fold_left T.Leaf ~acc:100 ~f:(fun acc v -> acc + v) in
  assert_equal 100 result

let test_dict_map_to_different_type _ =
  let mapped_dict = D.map d1 ~f:(fun _key value ->
      Float.of_int value *. 3.14
    )
  in
  assert_equal (Some 6.28) @@ D.lookup mapped_dict ~key:"a"

let test_dict_insert_maintains_order _ =
  let keys_to_insert = ["z"; "n"; "a"; "k"; "d"; "s"; "x"] in
  let final_dict = List.fold keys_to_insert ~init:D.empty ~f:(fun acc key ->
      D.insert acc ~key ~value:(String.length key)
    )
  in
  let compare_items i1 i2 = String.compare i1.I.key i2.I.key in
  assert_equal true @@ T.is_ordered final_dict ~compare:compare_items;
  assert_equal ["a"; "d"; "k"; "n"; "s"; "x"; "z"] @@ D.keys final_dict

let test_dict_lookup_misses _ =
  assert_equal None @@ D.lookup d1 ~key:"A";
  assert_equal None @@ D.lookup d1 ~key:"z";
  assert_equal None @@ D.lookup d1 ~key:"bb"

  let series =
    "Student tests" >:::
    [ "fold on empty" >:: test_tree_fold_on_empty
    ; "dict map to different type" >:: test_dict_map_to_different_type
    ; "dict insert keeps order" >:: test_dict_insert_maintains_order
    ; "dict lookup misses" >:: test_dict_lookup_misses
    ]
end

let series =
  "Assignment2 Tests" >:::
  [ Student_tests.series
  ; Tree_tests.series
  ; Dict_tests.series ]

(* The following line runs all the tests put together into `series` above *)

let () = run_test_tt_main series