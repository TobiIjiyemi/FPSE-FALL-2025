(*
  You will develop a command-line application to count how many occurrences of each OCaml language keyword is in a directory containing OCaml code.

  This comment to describe the assignment is long, but it contains valuable information, and you'll need to read it all.


  ------------
  GENERAL FLOW
  ------------
  
  Given a directory path on the command line (or the current directory if none is given), you should:

  - traverse the directory recursively to find each OCaml source file (.ml or .mli)

  - filter out any comments and literal strings from the code

  - count the number of occurences of each keyword in the file: sum up all
    of the occurrences over all files and sort the resulting dictionary
    from most to least common occurrence.

  - report the total sum of keyword counts to stdout in s-expression format terminated by a newline:

    (((Keyword <word>)<number>)((Keyword <word>)<number>) ... )


  ---------------------
  HOW TO COUNT KEYWORDS
  ---------------------

  Before counting keywords you should remove all comments and string literals; for example 

  (* 
    remove this text in case there is a match with a keyword here by mistake
  *)
  
  and 
  
  "remove this text too in case there is a match with a keyword in this literal string"

  Once comments/literals are removed, you need to count each occurence of a keyword in the file which is delimited on both ends by non-characters. So for example `fun iffy -> 0` does not contain the keyword `if` since it is not delimited on both ends by non-characters, but `if x > 0` does.


  --------------
  CLARIFICATIONS
  --------------
  
  Here are some clarifications you might find useful. Before asking for clarification on Courselore, please read all of the items below.
  - Nested comments are supported in OCaml, so be sure to keep track of nesting.
  - Comments can span multiple lines like this one we're in right now.
  - We don't require whitespace around the comment characters, so the following line would be a valid comment:
    (* this is a valid comment even though there is no space before it closes*)
  - You can assume there are no escaped double quotes (which allow string literals to themselves contain double quotes).
  - Literal strings, even those in comments, take precedence over comments, e.g.
    (* this is a single valid comment despite the " *) ". See how it doesn't mess up the rest of the file below *)
  - Do not output anything for keywords that don't appear in the source files.
  - A "non-character" is anything that is not alphanumeric and is not an underscore. Examples:
    - `not (if x = 0 then true else false)` contains the keywords {if, then, true, else, false} because '(' and ')' are "non-characters".
    - `fun if_if_if(*comment here*) -> true` contains only the keywords {fun, true} because '_' is not a "non-character".
  - Your code will only be tested on valid directories.
  - Your code will only be tested on compiling OCaml files.
  - Report keywords in order of occurrence--greatest to least. If multiple keywords have the same count, then report those keywords in alphabetical order.
  - Your output must be terminated by a newline to pass the Gradescope tests.
    

  ---------
  LIBRARIES
  ---------

  Any of the default set of libraries for the course may be used.

  We suggest `Sys_unix`, `Filename`, and `In_channel` for basic filesystem oeprations. They are usable
  in your `Utils` module with the provided `dune` file.

  `ppx_jane` helps you derive s-expression conversions with `[@@deriving sexp]` and comparison with `[@@deriving compare].
  It has been added as a pre-process extension in the given `dune` file.
  
  Note: don't use any other opam libraries beyond the official opam libraries
    on the FPSE Coding page. Add approved libraries to your dune file as needed.
    If you have the library installed and have only followed the course's instructions,
    then it's an approved library.


  -------
  TESTING
  -------

  Since this file compiles to an executable, the functions defined within it cannot be tested. We can, however, test the behavior of the executable by capturing its output. A few tests like this are provided in the `src-test/` directory, and these tests reference the `test/` directory.
  
  Any testable helper functions you write should be in utils.ml(i) or histogram.ml(i) so that you can test them. We have provided an empty utils.ml(i) where you are to add your helper functions.

  Add your tests to `src-test/tests.ml` and note the README for testing requirements. The tests on your helper functions will be considered in your grade.


  ---------------
  GETTING STARTED
  ---------------

  First go implement the Histogram module. See histogram.mli for the description. You are expected to use this module when counting keywords.

  Below we have parsed the arguments for you. You are free to modify it in any way.

  OCaml executables work by simply evaluating each top-level expression in the file, similar conceptually to a scripting language, so all the `let () =` code will run. Feel free to modify the code below as much as you want.

  A rough idea of what the top-level program in the `count_keywords` function could be something like:

    target_dir
    |> get_path_elts
    |> keywords_from_files
    |> histogram_from_keywords
    |> Histogram.to_string
    |> print_endline

  Please make sure to break the tasks down into separate functions for each distinct task. Put any auxiliary functions in utils.ml and utils.mli so they can be tested as well.
*)

open Core
(*
  Your main code will replace this default implementation.
*)
(* This is the main logic for the keyword counting application. *)
let count_keywords target_dir =
  (* regular expression to split code with delimiters as non-keyword character*)
  let splitter = Re.compile (Re.Perl.re "[^a-zA-Z0-9_]+") in
  Utils.get_ocaml_files target_dir
  |> List.map ~f:In_channel.read_all
  |> List.map ~f:Utils.filter_code
  |> String.concat ~sep:" "
  |> Re.split splitter
  |> List.filter_map ~f:Keyword.of_string
  |> Histogram.increment_many Histogram.empty
  |> Histogram.to_string
  |> print_endline

(* 
  We use Cmdliner to parse the command line arguments for us.

  This will read the first command line argument as a directory, and if it's
  missing, then it defaults to the current working directory.

  Then body of count_keywords is run with Cmdliner, too.
  Your code will replace the default implementation above that simply prints
  the target directory. You won't need to change any of the following code.
*)
let () =
  let cmd_main = 
    let open Cmdliner.Term.Syntax in
    Cmdliner.Cmd.v (Cmdliner.Cmd.info "keywordcount.exe") @@
    let+ target_dir = Cmdliner.Arg.(
      value & pos 0 dir (Core_unix.getcwd ()) & info [] ~docv:"DIRECTORY" ~doc:"Directory in which to find OCaml files"
    ) in
    count_keywords target_dir
  in
  match Cmdliner.Cmd.eval_value' cmd_main with
  | `Ok _ -> ()         (* executed cmd_main *)
  | `Exit i -> exit i   (* cmd_main failed *)