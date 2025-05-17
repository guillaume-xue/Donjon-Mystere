open OUnit2
open Models.Automate_1
open Utils.Types

let tiles = [
  { x = 0; y = 0; texture_id = 1; biome_id = 0 };
  { x = 1; y = 0; texture_id = 1; biome_id = 0 };
  { x = 2; y = 0; texture_id = 1; biome_id = 0 };
  { x = 0; y = 1; texture_id = 1; biome_id = 0 };
  { x = 1; y = 1; texture_id = 0; biome_id = 0 };
  { x = 2; y = 1; texture_id = 1; biome_id = 0 };
  { x = 0; y = 2; texture_id = 1; biome_id = 0 };
  { x = 1; y = 2; texture_id = 1; biome_id = 0 };
  { x = 2; y = 2; texture_id = 0; biome_id = 0 };
] 

let tiles2 = [
  { x = 0; y = 0; texture_id = 1; biome_id = 0 };
  { x = 1; y = 0; texture_id = 0; biome_id = 0 };
  { x = 2; y = 0; texture_id = 0; biome_id = 0 };
  { x = 0; y = 1; texture_id = 1; biome_id = 0 };
  { x = 1; y = 1; texture_id = 0; biome_id = 0 };
  { x = 2; y = 1; texture_id = 0; biome_id = 0 };
  { x = 0; y = 2; texture_id = 1; biome_id = 0 };
  { x = 1; y = 2; texture_id = 1; biome_id = 0 };
  { x = 2; y = 2; texture_id = 1; biome_id = 0 };
]

let tiles3 = [
  { x = 0; y = 0; texture_id = 1; biome_id = 0 };
  { x = 1; y = 0; texture_id = 0; biome_id = 0 };
  { x = 2; y = 0; texture_id = 1; biome_id = 0 };
  { x = 0; y = 1; texture_id = 1; biome_id = 0 };
  { x = 1; y = 1; texture_id = 0; biome_id = 0 };
  { x = 2; y = 1; texture_id = 0; biome_id = 0 };
  { x = 0; y = 2; texture_id = 1; biome_id = 0 };
  { x = 1; y = 2; texture_id = 1; biome_id = 0 };
  { x = 2; y = 2; texture_id = 1; biome_id = 0 };
]

let nb_voisins _ =
  Printf.printf "Testing automate :\n";

  let nb_voisins = nb_voisins_vivant tiles 1 2 in
  Printf.printf "Nombre de voisins vivants : %d\n" nb_voisins;
  assert_equal 3 nb_voisins;
  Printf.printf "nb_voisins_vivant test 1 passed\n";

  let nb_voisins = nb_voisins_vivant tiles 1 0 in
  Printf.printf "Nombre de voisins vivants : %d\n" nb_voisins;
  assert_equal 4 nb_voisins;
  Printf.printf "nb_voisins_vivant test 2 passed\n\n"

let auto_cell _ =
  Printf.printf "Testing regles_auto_cell :\n";

  let new_tiles = regles_auto_cell tiles2 1 in
  Printf.printf "Tuiles après application des règles :\n";
  List.iter (fun tile -> Printf.printf "(%d, %d) : %d\n" tile.x tile.y tile.texture_id) new_tiles;
  List.iter (fun tile ->
    if tile.x = 1 && tile.y = 1 then
      assert_equal 1 tile.texture_id
    else
      assert_equal 0 tile.texture_id
  ) new_tiles;
  Printf.printf "regles_auto_cell test passed\n";

  let new_tiles = regles_auto_cell tiles2 2 in
  Printf.printf "Tuiles après application des règles :\n";
  List.iter (fun tile -> Printf.printf "(%d, %d) : %d\n" tile.x tile.y tile.texture_id) new_tiles;
  List.iter (fun tile ->
      assert_equal 0 tile.texture_id
  ) new_tiles;
  Printf.printf "regles_auto_cell test passed\n\n"

let flood_fill_test _ =
  Printf.printf "Testing flood_fill :\n";

  let visited = ref [] in
  let size = flood_fill tiles2 visited 0 0 in
  Printf.printf "Taille de la zone remplie : %d\n" size;
  assert_equal 5 size;
  Printf.printf "flood_fill test passed\n\n"

let remove_zone_test _ =
  Printf.printf "Testing remove_zone :\n";

  let new_tiles = remove_zone tiles2 0 0 in
  Printf.printf "Tuiles après suppression de la zone :\n";
  List.iter (fun tile -> Printf.printf "(%d, %d) : %d\n" tile.x tile.y tile.texture_id) new_tiles;
  List.iter (fun tile ->
    assert_equal 0 tile.texture_id
  ) new_tiles;
  Printf.printf "remove_zone test passed\n\n"

let remove_small_zones_test _ =
  Printf.printf "Testing remove_small_zones :\n";

  let new_tiles = remove_small_zones tiles3 in
  Printf.printf "Tuiles après suppression des petites zones :\n";
  List.iter (fun tile -> Printf.printf "(%d, %d) : %d\n" tile.x tile.y tile.texture_id) new_tiles;
  List.iter (fun tile ->
    assert_equal 0 tile.texture_id
  ) new_tiles;
  Printf.printf "remove_small_zones test passed\n\n"









let suite =
  "Automate Tests" >::: [
    "nb_voisins" >:: nb_voisins;
    "auto_cell" >:: auto_cell;
    "flood_fill_test" >:: flood_fill_test;
    "remove_zone_test" >:: remove_zone_test;
    "remove_small_zones_test" >:: remove_small_zones_test;
  ]