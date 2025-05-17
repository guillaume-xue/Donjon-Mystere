open OUnit2
open Models.Prim_2
open Utils.Types

let tiles = [
  { x = 0; y = 0; texture_id = 1; biome_id = 0 };
  { x = 1; y = 0; texture_id = 0; biome_id = 0 };
  { x = 2; y = 0; texture_id = 0; biome_id = 0 };
  { x = 0; y = 1; texture_id = 0; biome_id = 0 };
  { x = 1; y = 1; texture_id = 0; biome_id = 0 };
  { x = 2; y = 1; texture_id = 0; biome_id = 0 };
  { x = 0; y = 2; texture_id = 0; biome_id = 0 };
  { x = 1; y = 2; texture_id = 0; biome_id = 0 };
  { x = 2; y = 2; texture_id = 1; biome_id = 0 };
] 

let tiles2 = [
  { x = 0; y = 0; texture_id = 1; biome_id = 0 };
  { x = 1; y = 0; texture_id = 1; biome_id = 0 };
  { x = 2; y = 0; texture_id = 0; biome_id = 0 };
  { x = 0; y = 1; texture_id = 1; biome_id = 0 };
  { x = 1; y = 1; texture_id = 1; biome_id = 0 };
  { x = 2; y = 1; texture_id = 1; biome_id = 0 };
  { x = 0; y = 2; texture_id = 0; biome_id = 0 };
  { x = 1; y = 2; texture_id = 0; biome_id = 0 };
  { x = 2; y = 2; texture_id = 1; biome_id = 0 };
] 

let zone1 = {
  id = 0;
  size = 1;
  tiles = [{ x = 0; y = 0; texture_id = 1; biome_id = 0 }];
}

let zone2 = {
  id = 1;
  size = 1;
  tiles = [{ x = 2; y = 2; texture_id = 1; biome_id = 0 }];
}


let test_prim _ = 
  Printf.printf "Testing prim :\n";

  let prim = connect_zones tiles [zone1; zone2] in
  Printf.printf "Prim result: \n";
  List.iter (fun tile -> Printf.printf "(%d, %d) : %d\n" tile.x tile.y tile.texture_id) prim;
  List.iter2 (fun tile tile2 ->
    assert_equal tile.texture_id tile2.texture_id;
  ) prim tiles2;
  Printf.printf "Prim test passed\n\n"












let suite = 
  "Prim Tests" >::: [
    "test_prim" >:: test_prim;
  ]