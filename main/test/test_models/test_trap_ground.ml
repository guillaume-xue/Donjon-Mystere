open OUnit2
open Models.Trap_ground
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

let test_set_trap_ground_visibility _ =
  Printf.printf "Testing set_trap_ground_visibility :\n";

  let trap = { nature = Stairs_Up; tag_pos_x = 1; tag_pos_y = 2; visibility = false } in
  let updated_trap = set_trap_ground_visibility true trap in

  assert_equal true updated_trap.visibility;
  assert_equal Stairs_Up updated_trap.nature;
  assert_equal 1 updated_trap.tag_pos_x;
  assert_equal 2 updated_trap.tag_pos_y;

  Printf.printf "set_trap_ground_visibility test passed\n"

let test_set_trap_ground_pos_visibility _ =
  Printf.printf "Testing set_trap_ground_pos_visibility :\n";

  let traps = [
    { nature = Stairs_Up; tag_pos_x = 1; tag_pos_y = 2; visibility = false };
    { nature = Stairs_Down; tag_pos_x = 3; tag_pos_y = 4; visibility = false };
  ] in

  let updated_traps = set_trap_ground_pos_visibility 1 2 true traps in
  let first_trap = List.hd updated_traps in

  assert_equal true first_trap.visibility;
  assert_equal Stairs_Up first_trap.nature;

  Printf.printf "set_trap_ground_pos_visibility test passed\n"

let test_is_stairs _ =
  Printf.printf "Testing is_stairs :\n";

  let traps = [
    { nature = Stairs_Up; tag_pos_x = 1; tag_pos_y = 2; visibility = true };
    { nature = Stairs_Down; tag_pos_x = 3; tag_pos_y = 4; visibility = true };
  ] in

  let player = { (empty_entity ()) with position = { world_x = 1.0; world_y = 2.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0 } } in

  assert_bool "Expected player to be on stairs" (is_stairs traps player);

  let player_not_on_stairs = { player with position = { world_x = 5.0; world_y = 5.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0} } in
  assert_bool "Expected player not to be on stairs" (not (is_stairs traps player_not_on_stairs));

  Printf.printf "is_stairs test passed\n"

let test_is_trap_ground _ =
  Printf.printf "Testing is_trap_ground :\n";

  let traps = [
    { nature = Stairs_Up; tag_pos_x = 1; tag_pos_y = 2; visibility = true };
    { nature = Stairs_Down; tag_pos_x = 3; tag_pos_y = 4; visibility = true };
  ] in

  assert_bool "Expected trap at (1, 2)" (is_trap_ground traps 1 2);
  assert_bool "Expected no trap at (5, 5)" (not (is_trap_ground traps 5 5));

  Printf.printf "is_trap_ground test passed\n"

let test_get_trap_ground _ =
  Printf.printf "Testing get_trap_ground :\n";

  let traps = [
    { nature = Stairs_Up; tag_pos_x = 1; tag_pos_y = 2; visibility = true };
    { nature = Stairs_Down; tag_pos_x = 3; tag_pos_y = 4; visibility = true };
  ] in

  (match get_trap_ground traps 1 2 with
  | Some trap ->
    assert_equal Stairs_Up trap.nature;
    assert_equal 1 trap.tag_pos_x;
    assert_equal 2 trap.tag_pos_y;
  | None -> assert_failure "Expected to find a trap at (1, 2)");

  (match get_trap_ground traps 5 5 with
  | Some _ -> assert_failure "Expected no trap at (5, 5)"
  | None -> ());

  Printf.printf "get_trap_ground test passed\n"

(* Test suite *)
let suite =
  "TrapGround Tests" >::: [
    "test_set_trap_ground_visibility" >:: test_set_trap_ground_visibility;
    "test_set_trap_ground_pos_visibility" >:: test_set_trap_ground_pos_visibility;
    "test_is_stairs" >:: test_is_stairs;
    "test_is_trap_ground" >:: test_is_trap_ground;
    "test_get_trap_ground" >:: test_get_trap_ground;
  ]
