(*
  See utils.mli. This file contains implementations for your utils.mli functions.
*)

open Core

(*manage the type of data we're inside
in_comment int manages how many levels deep in the comments we're in
*)
type state =
    | Code
    | In_string
    | In_comment of int

let get_ocaml_files (dir : string) : string list =
  let rec find_files_recursive path acc =
    (* Use a match statement to handle the result robustly *)
    match Sys_unix.is_directory path with
    | `Yes ->
        (* if a directory then recursively search it*)
        (*take entries from path, run in recursive function, when yes then rerun
        if no then add to accumulator if ocaml file
        *)
        let entries = Sys_unix.readdir path |> Array.to_list in
        List.fold entries ~init:acc ~f:(fun collected_files entry ->
            let full_path = Filename.concat path entry in
            find_files_recursive full_path collected_files)
    | `No | `Unknown ->
        (* check if it's an OCaml file *)
        if Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli"
        then path :: acc
        else acc
  in
  find_files_recursive dir []

let filter_code (content : string) : string =
  let rec process_chars (chars : char list) (current_state : state) (acc : char list) : char list =
    match current_state, chars with
    (* if no more characters then quit and return accumulator
      if we're in the code and the next character is a string so put the state in In_string,
        if the next character is a parenthesees follow by "*" then put the state in comment with
          the first depth of the comment as 1. other wise if the next character is still code then
        continue to process those characters
      
      *)
    | _, [] -> acc
    | Code, '"' :: rest -> process_chars rest In_string acc 
    | Code, '(' :: '*' :: rest -> process_chars rest (In_comment 1) acc
    | Code, c :: rest -> process_chars rest Code (c :: acc)
    | In_string, '"' :: rest -> process_chars rest Code (' ' :: acc)
    | In_string, _ :: rest -> process_chars rest In_string acc
    | In_comment n, '(' :: '*' :: rest -> process_chars rest (In_comment (n + 1)) acc
    | In_comment 1, '*' :: ')' :: rest -> process_chars rest Code (' ' :: acc) 
    | In_comment n, '*' :: ')' :: rest -> process_chars rest (In_comment (n - 1)) acc
    | In_comment n, _ :: rest -> process_chars rest (In_comment n) acc 
  in

  let char_list = String.to_list content in
  let filtered_list = process_chars char_list Code [] in
  (* reverse to get proper order*)
  filtered_list |> List.rev |> String.of_char_list