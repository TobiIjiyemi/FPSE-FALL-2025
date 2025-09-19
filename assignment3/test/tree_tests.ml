
open Core
open OUnit2

module T = Simpletree

let singleton x =
  T.Branch { item = x ; left = T.Leaf ; right = T.Leaf }

let t1 = T.Branch 
  { item = 2
  ; left = singleton 1
  ; right = singleton 3 }

let t_unbalanced = T.Branch
  { item = 2
  ; left = T.Branch 
    { item = 1
    ; left = singleton 0
    ; right = T.Leaf }
  ; right = T.Leaf }

let t_unordered = T.Branch 
  { item = 2
  ; left = singleton 5
  ; right = T.Leaf }

let test_size _ =
  assert_equal 0 @@ T.size T.Leaf;
  assert_equal 3 @@ T.size t1;
  assert_equal 3 @@ T.size t_unbalanced

let test_height _ =
  assert_equal 0 @@ T.height T.Leaf;
  assert_equal 2 @@ T.height t1;
  assert_equal 3 @@ T.height t_unbalanced

let test_is_balanced _ =
  assert_equal true @@ T.is_balanced t1;
  assert_equal false @@ T.is_balanced t_unbalanced

let test_is_ordered _ =
  assert_equal true @@ T.is_ordered t1 ~compare:Int.compare;
  assert_equal false @@ T.is_ordered t_unordered ~compare:Int.compare

let test_map _ =
  let expected = T.Branch 
    { item = "2"
    ; left = singleton "1"
    ; right = singleton "3" }
  in
  assert_equal expected @@ T.map t1 ~f:Int.to_string

let test_fold_left _ =
  assert_equal 6 @@ T.fold_left t1 ~acc:0 ~f:(fun acc v -> acc + v);
  assert_equal "123" @@ T.fold_left ~acc:"" ~f:(^) (T.Branch 
    { item = "2"
    ; left = T.Branch { item = "1"; left = T.Leaf; right = T.Leaf }
    ; right = T.Branch { item = "3"; left = T.Leaf; right = T.Leaf } }
  )

let series =
  "simpletree tests" >:::
  [ "size" >:: test_size
  ; "height" >:: test_height
  ; "is_balanced" >:: test_is_balanced
  ; "is_ordered" >:: test_is_ordered
  ; "map" >:: test_map
  ; "fold_left" >:: test_fold_left ]