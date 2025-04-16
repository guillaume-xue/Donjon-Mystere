open OUnit2
open Models.Map_model
open Utils.Types

let test_is_in_map _ =
  let map = { width = 5; height = 5; tiles = []; regions = []; floor = 1 } in
  assert_equal true (is_in_map 0 0 map);
  assert_equal true (is_in_map 4 4 map);
  assert_equal false (is_in_map 5 5 map);
  assert_equal false (is_in_map (-1) 0 map);
  assert_equal false (is_in_map 0 (-1) map);
  assert_equal false (is_in_map 6 2 map)

let test_is_wall _ =
  let map = {
    width = 5;
    height = 5;
    tiles = [
      { x = 0; y = 0; texture_id = 1; biome_id = 0 };
      { x = 1; y = 1; texture_id = 0; biome_id = 0 };
      { x = 2; y = 2; texture_id = 0; biome_id = 0 }
    ];
    regions = [];
    floor = 1
  } in
  assert_equal false (is_wall 0 0 map);
  assert_equal true (is_wall 1 1 map);
  assert_equal true (is_wall 2 2 map);
  assert_equal false (is_wall 3 3 map);
  assert_equal false (is_wall (-1) 0 map);
  assert_equal false (is_wall 0 (-1) map)

(* Export the suite *)
let suite = "MapModel Tests" >::: [
  "test_is_in_map" >:: test_is_in_map;
  "test_is_wall" >:: test_is_wall
]