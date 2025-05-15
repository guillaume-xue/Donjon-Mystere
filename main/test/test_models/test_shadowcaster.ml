open OUnit2
open Models.Shadowcaster
open Utils.Types

let empty_entity () = 
  {
    id = 0;
    last_id = 0;
    number = 0;
    position = { world_x = 0.0; world_y = 0.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0 };
    entity_textures_id = 0;
    moving = false;
    state = Idle;
    direction = Up;
    current_hp = 0;
    max_hp = 0;
    level = 0;
    current_xp = 0;
    max_xp = 0;
    action = Nothing;
    bag = { items = []; max_size = 5 ; selected_item = 0};
    step_cpt  = 0;
    speed  = 0.0;
    attaque  = 0;
    defense  = 0;
    attaque_speciale  = 0;
    defense_speciale  = 0;
    element  = Feu;
    competence = [];
    path = [];
    your_turn  = false;
  }

let test_compute_fov _ =
  Printf.printf "Testing compute_fov :\n";

  (* Création d'une grille de tuiles *)
  let grid = [
    { x = 0; y = 0; texture_id = 1; biome_id = 0 };
    { x = 1; y = 0; texture_id = 1; biome_id = 0 };
    { x = 2; y = 0; texture_id = 1; biome_id = 0 };
    { x = 0; y = 1; texture_id = 1; biome_id = 0 };
    { x = 1; y = 1; texture_id = 0; biome_id = 0 }; (* Mur *)
    { x = 2; y = 1; texture_id = 1; biome_id = 0 };
    { x = 0; y = 2; texture_id = 1; biome_id = 0 };
    { x = 1; y = 2; texture_id = 1; biome_id = 0 };
    { x = 2; y = 2; texture_id = 1; biome_id = 0 };
  ] in

  (* Création d'un joueur fictif *)
  let player = {
    (empty_entity ()) with
    position = { world_x = 1.0; world_y = 1.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0 };
  } in

  (* Calcul du champ de vision *)
  let max_x = 3 in
  let max_y = 3 in
  let radius = 2 in
  let visibility = compute_fov player radius grid max_x max_y in

  (* Vérification des résultats *)
  assert_equal 0.0 visibility.(1).(1); (* Position du joueur *)
  assert_bool "Expected tile (0, 0) to be visible" (visibility.(0).(0) > 0.0);
  assert_bool "Expected tile (2, 2) to be visible" (visibility.(2).(2) > 0.0);
  assert_bool "Expected tile (1, 1) to be visible" (visibility.(1).(1) = 0.0);
  assert_bool "Expected tile (1, 0) to be blocked" (visibility.(1).(0) > 0.0);
  assert_bool "Expected tile (1, 1) to be visible" (visibility.(1).(1) = 0.0);

  Printf.printf "compute_fov test passed\n"

(* Test suite *)
let suite =
  "Shadowcaster Tests" >::: [
    "test_compute_fov" >:: test_compute_fov;
  ]