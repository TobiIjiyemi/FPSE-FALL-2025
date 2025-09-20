
open Core
open OUnit2

module D = Simpledict
module I = Dict_item
module T = Simpletree

let singleton k v =
  T.Branch { item = { I.key = k ; value = v } ; left = T.Leaf ; right = T.Leaf }

let d1 = T.Branch 
  { item = { I.key = "b" ; value = 1 }
  ; left = singleton "a" 2
  ; right = singleton "c" 3 }

let d2 = T.Branch 
  { item = { I.key = "b" ; value = 10 }
  ; left = singleton "a" 20
  ; right = T.Leaf }

let test_empty_and_size _ =
  assert_equal 0 (D.size D.empty);
  assert_equal 3 (D.size d1)

let test_map _ =
  let d' = D.map d1 ~f:(fun k v -> String.length k + v) in
  (* a -> 1+2, b -> 1+1, c -> 1+3 *)
  assert_equal 3 @@ Option.value_exn (D.lookup d' ~key:"a");
  assert_equal 2 @@ Option.value_exn (D.lookup d' ~key:"b");
  assert_equal 4 @@ Option.value_exn (D.lookup d' ~key:"c")

let test_insert_and_lookup _ =
  let d = D.insert D.empty ~key:"k" ~value:7 in
  assert_equal (Some 7)  @@ D.lookup d ~key:"k";
  (* Overwrite *)
  let d2 = D.insert d ~key:"k" ~value:42 in
  assert_equal (Some 42) @@ D.lookup d2 ~key:"k";
  assert_equal None @@ D.lookup d2 ~key:"zzz"

(* The following tests require insert to work *)

let test_keys _ =
  let d = 
    List.fold [("x",1);("a",2);("m",3)]
      ~f:(fun acc (k,v) -> D.insert acc ~key:k ~value:v)
      ~init:D.empty 
  in
  assert_equal ["a";"m";"x"] @@ D.keys d

let test_update _ =
  let d = D.insert d1 ~key:"p" ~value:9 in
  (* update existing *)
  let d_upd = D.update d ~key:"p" ~f:(function None -> 0 | Some v -> v * 2) in
  assert_equal (Some 18) @@ D.lookup d_upd ~key:"p";
  (* insert new *)
  let d_upd2 = D.update d_upd ~key:"q" ~f:(function None -> 7 | Some v -> v * 7) in
  assert_equal (Some 7) @@ D.lookup d_upd2 ~key:"q"

let test_merge _ =
  let merged = D.merge d1 d2 in
  (* b gets value from d2, a gets value from d2, c only in d1 *)
  assert_equal (Some 10) @@ D.lookup merged ~key:"b";
  assert_equal (Some 20) @@ D.lookup merged ~key:"a";
  assert_equal (Some 3) @@ D.lookup merged ~key:"c"

let test_combine _ =
  let combined = D.combine d1 d2 ~f:(fun _ -> function
      | `Left x -> x * 2
      | `Right y -> y * 3
      | `Both (x, y) -> Int.pow y x
    )
  in
  assert_equal (Some 400) @@ D.lookup combined ~key:"a";
  assert_equal (Some 10) @@ D.lookup combined ~key:"b";
  assert_equal (Some 6) @@ D.lookup combined ~key:"c";
  assert_equal None @@ D.lookup combined ~key:"y"

let series =
  "simpledict tests" >:::
    [ "empty_and_size" >:: test_empty_and_size
    ; "map" >:: test_map
    ; "insert_and_lookup" >:: test_insert_and_lookup
    ; "keys" >:: test_keys
    ; "update" >:: test_update
    ; "merge" >:: test_merge
    ; "combine" >:: test_combine ]