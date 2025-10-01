open Core
open OUnit2
(* Helper to create a Keyword.t from a string. *)
let kw s = Option.value_exn (Keyword.of_string s)

module Student_tests = struct
  let test_utils_filter _ =
    (*empty string *)
    assert_equal "" (Utils.filter_code "");

    (*basic comment *)
    assert_equal "let   x" (Utils.filter_code "let (* comment *) x");

    (* nested comment *)
    assert_equal "let   x" (Utils.filter_code "let (* a (* nested *) comment *) x");

    (* string literal *)
    assert_equal "let s =   " (Utils.filter_code "let s = \"keyword let\" ");

    (* comment inside of a string, string should take precedence*)
    assert_equal "let s =   " (Utils.filter_code "let s = \"(* not a comment *)\" ");

    (* string inside of a comment, inverted from above *)
    assert_equal "  let x" (Utils.filter_code "(* \"not a string\" *) let x");

    (* unclosed comment, nothing after unclosed comment should be caught in keywords*)
    assert_equal "let " (Utils.filter_code "let (* unclosed \n if then if then x let ")

  (* Histogram tests *)
  let test_histogram _ =
    (* empty histogram produces empty s-expression list. *)
    assert_equal "()" (Histogram.empty |> Histogram.to_string);

    (* sorting based on count, descending*)
    let hist1 =
      Histogram.increment_many Histogram.empty [ kw "let"; kw "in"; kw "let"; kw "rec"; kw "let" ]
    in
    assert_equal "(((Keyword let)3)((Keyword in)1)((Keyword rec)1))" (Histogram.to_string hist1);

    (* edge case when counts are tied, should default to alphabetical *)
    let hist2 =
      Histogram.increment_many Histogram.empty [ kw "with"; kw "rec"; kw "let" ]
    in
    assert_equal "(((Keyword let)1)((Keyword rec)1)((Keyword with)1))" (Histogram.to_string hist2);

    (* of_alist works correctly and produces a sorted output via to_string. *)
    let alist = [ (kw "if", 1); (kw "then", 5) ] in
    let hist3 = Histogram.of_alist alist in
    assert_equal "(((Keyword then)5)((Keyword if)1))" (Histogram.to_string hist3)

  let series =
    "Student tests"
    >::: [
           "Utils filter_code" >:: test_utils_filter;
           "Histogram logic" >:: test_histogram;
         ]
end

let series =
  "Assignment 4 tests"
  >::: [ Student_tests.series; Hist_tests.series; Exec_tests.series ]

let () = run_test_tt_main series