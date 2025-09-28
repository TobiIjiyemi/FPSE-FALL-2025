
open Core

type t = Keyword of string
  [@@deriving compare, sexp]
  [@@unboxed]

let all_keyword_strings = ["and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with"]

let of_string (candidate : string) : t option =
  if List.mem all_keyword_strings candidate ~equal:String.equal
  then Some (Keyword candidate)
  else None
