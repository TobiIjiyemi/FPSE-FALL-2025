
open Core
open OUnit2

(*
  We only provide one test on histograms.
*)

let test _ =
  let keywords = 
    List.map [ "let" ; "in" ; "let" ; "with" ; "with" ] ~f:(fun s ->
      Option.value_exn (Keyword.of_string s)
    )
  in
  assert_equal
    "(((Keyword let)2)((Keyword with)2)((Keyword in)1))"
    (Histogram.to_string
    @@ Histogram.increment_many Histogram.empty keywords)

let series =
  "Hist tests" >::: [ "Hist test" >:: test ]