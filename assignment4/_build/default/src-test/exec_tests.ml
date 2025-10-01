
open Core
open OUnit2

(*
  ------------------
  EXECUTABLE TESTING   
  ------------------

  To test the executable we capture its output and compare the result to the string from the expected histogram.
*)

(* OUnit2.assert_command provides an infinite sequence that "ends" when it throws an EOF exception *)
(* Note the OUnit2 discards the last character (newline or not), so we'll require that output is terminated in a newline in order for it to be read properly. *)
(* This has a non-exhaustive pattern match. Turn off warning with "-8" *)
let[@warning "-8"] string_of_seq (s : char Seq.t) : string =
  (* the pattern-matched argument in the following function is a "thawed" sequence *)
  let rec char_lst_of_seq (Seq.Cons (h, t)) lst =
    try
      char_lst_of_seq (t ()) (h :: lst)
    with _ ->
      List.rev lst
  in
  String.of_char_list
  @@ char_lst_of_seq (s ()) []
  
let assert_string_output (message: string) (expected : string) (cseq : char Seq.t) : unit =
  let actual = String.filter ~f:(Char.(<>) '\n') @@ string_of_seq cseq |> String.strip in (* get string from char Seq.t *)
  assert_bool
    (Printf.sprintf
      "Failed on: %s\n- expected: %s\n- actual: %s\n"
      message expected actual)
    (String.equal expected actual)

let exec_dir = "../src/bin/" (* note that cwd is _build/default/src-test *)
let test_dir = "../test/"
  
(* We may get timing errors when using dune exec -- src/filename.exe <args>, so we locate the executable and run it directly. *)
let test_exec (args : string) (expected : string) (ctxt : test_ctxt) : unit =
  assert_command
    ~foutput:(assert_string_output ("keywordcount.exe " ^ args) expected) (* this is a function from char Seq.t to unit that throws an exception if the output is not as expected *)
    ~ctxt
    (exec_dir ^ "keywordcount.exe")
    (String.split ~on:(' ') args) (* Arguments to exec *)

let test_nested_counts =
  test_exec
    (test_dir ^ "nested_folder_test/")
    "(((Keyword else)1)((Keyword if)1)((Keyword let)1)((Keyword then)1))"

let test_comments_counts =
  test_exec
    (test_dir ^ "comments_test/")
    "(((Keyword let)8)((Keyword in)2)((Keyword match)1)((Keyword rec)1)((Keyword with)1))"

let test_strings_counts =
  test_exec
    (test_dir ^ "strings_test/")
    "(((Keyword let)2)((Keyword in)1))"

let series =
  "Executable tests" >:::
  [ "Nested"   >:: test_nested_counts
  ; "Comments" >:: test_comments_counts
  ; "Strings"  >:: test_strings_counts ]