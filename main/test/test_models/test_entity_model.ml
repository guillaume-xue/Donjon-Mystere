open OUnit2
open Models.EntityModel
open Utils.Types

let empty_entity () = 
  {
    nom = "";
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
    bag = { items = []; max_size = 5 };
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


let test_set_entity_target _ =
  let entity = empty_entity () in
  Printf.printf "Testing set_entity_target :\n";

  (* Test de la direction Up *)
  Printf.printf "- Testing Up direction\n";
  let updated_entity = set_entity_target_with_direction Up entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal 0.0 target_x;
  assert_equal (-1.0) target_y;

  (* Test de la direction Down *)
  Printf.printf "- Testing Down direction\n";
  let updated_entity = set_entity_target_with_direction Down entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal 0.0 target_x;
  assert_equal 1.0 target_y;

  (* Test de la direction Left *)
  Printf.printf "- Testing Left direction\n";
  let updated_entity = set_entity_target_with_direction Left entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal (-1.0) target_x;
  assert_equal 0.0 target_y;

  (* Test de la direction Right *)
  Printf.printf "- Testing Right direction\n";
  let updated_entity = set_entity_target_with_direction Right entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal 1.0 target_x;
  assert_equal 0.0 target_y;

  (* Test de la direction DiagonalUpLeft *)
  Printf.printf "- Testing DiagonalUpLeft directions\n";
  let updated_entity = set_entity_target_with_direction DiagonalUpLeft entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal (-1.0) target_x;
  assert_equal (-1.0) target_y;

  (* Test de la direction DiagonalUpRight *)
  Printf.printf "- Testing DiagonalUpRight directions\n";
  let updated_entity = set_entity_target_with_direction DiagonalUpRight entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal 1.0 target_x;
  assert_equal (-1.0) target_y;

  (* Test de la direction DiagonalDownLeft *)
  Printf.printf "- Testing DiagonalDownLeft directions\n";
  let updated_entity = set_entity_target_with_direction DiagonalDownLeft entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal (-1.0) target_x;
  assert_equal 1.0 target_y;

  (* Test de la direction DiagonalDownRight *)
  Printf.printf "- Testing DiagonalDownRight directions\n";
  let updated_entity = set_entity_target_with_direction DiagonalDownRight entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal 1.0 target_x;
  assert_equal 1.0 target_y;

  (* Test de la direction No_move *)
  Printf.printf "- Testing No_move directions\n";
  let updated_entity = set_entity_target_with_direction No_move entity in
  let (target_x, target_y) = get_entity_target updated_entity in
  assert_equal 0.0 target_x;
  assert_equal 0.0 target_y;

  Printf.printf "set_entity_target test passed\n"

let test_set_i_competence_puissance _ =
  Printf.printf "Testing set_i_competence_puissance :\n";
  let entity = empty_entity () in

  (* Création de compétences pour les tests *)
  let competence1 = { id = 1; name = "Fireball"; description = "A powerful fire attack"; element = Feu; puissance = 50; precision = 90; attaqueType = Attaque} in
  let competence2 = { id = 2; name = "Water Splash"; description = "A splash of water"; element = Eau; puissance = 30; precision = 100; attaqueType = Attaque} in
  let competence3 = { id = 3; name = "Earthquake"; description = "A powerful earthquake"; element = Plante; puissance = 70; precision = 80; attaqueType = Attaque} in
  let competence4 = { id = 4; name = "Thunderbolt"; description = "A powerful electric attack"; element = Normal; puissance = 60; precision = 85; attaqueType = Attaque} in
  let new_entity = add_competence competence1 entity in
  let new_entity = add_competence competence2 new_entity in
  let new_entity = add_competence competence3 new_entity in
  let new_entity = add_competence competence4 new_entity in
  let new_entity = set_i_competence_puissance 0 70 new_entity in

  (* Test : modification de la puissance d'une compétence vide *)
  Printf.printf "- Testing empty competence\n";
  assert_raises (Invalid_argument "Index out of bounds") (fun () -> set_i_competence_puissance 0 50 entity);

  (* Test : modification de la puissance d'une compétence hors limites *)
  Printf.printf "- Testing overflow competence\n";
  assert_raises (Invalid_argument "Index out of bounds") (fun () -> set_i_competence_puissance 4 50 new_entity);

  (* Test : modification de la puissance d'une compétence existante *)
  Printf.printf "- Testing set competence puissance\n";
  assert_equal 70 (get_i_competence 0 new_entity).puissance;

  (* Test : vérification de la taille des compétences *)
  Printf.printf "- Testing size competence\n";
  assert_equal 4 (List.length new_entity.competence);

  Printf.printf "set_i_competence_puissance test passed\n"

let test_set_unable_item_bag _ =
  Printf.printf "Testing set_usable_item_bag :\n";
  let entity = empty_entity () in

  (* Création d'items pour les tests *)
  let item1 = { item_id = 1; item_skin_id = 101; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 1"; usable = true } in
  let item2 = { item_id = 2; item_skin_id = 102; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 2"; usable = true } in
  let new_entity = add_item_bag item1 entity in
  let new_entity = add_item_bag item2 new_entity in

  (* Test : modification d'un item inexistant (index négatif) *)
  Printf.printf "- Testing empty bag\n";
  assert_raises (Invalid_argument "Item not found in bag") (fun () -> set_usable_item_bag (-1) true entity);

  (* Test : modification d'un item inexistant (index hors limites) *)
  Printf.printf "- Testing overflow bag\n";
  assert_raises (Invalid_argument "Item not found in bag") (fun () -> set_usable_item_bag 2 true entity);

  (* Test : modification de l'état utilisable d'un item existant *)
  let new_entity = set_usable_item_bag 0 false new_entity in
  Printf.printf "- Testing set i usable item\n";
  assert_bool "- Testing set usable item\n" (false = (List.nth new_entity.bag.items 0).usable);
  let new_entity = set_usable_item_bag 1 false new_entity in
  Printf.printf "- Testing set i usable item different\n";
  assert_bool "- Testing set usable item\n" (not (true = (List.nth new_entity.bag.items 0).usable));

  Printf.printf "set_usable_item_bag test passed\n"

let test_remove_item_bag _ =
  Printf.printf "Testing remove_item_bag :\n";
  let entity = empty_entity () in

  (* Création d'items pour les tests *)
  let item1 = { item_id = 1; item_skin_id = 101; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 1"; usable = true } in
  let item2 = { item_id = 2; item_skin_id = 102; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 2"; usable = true } in
  let item3 = { item_id = 3; item_skin_id = 103; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Item 3"; usable = true } in

  (* Ajouter des items au sac *)
  let new_entity = add_item_bag item1 entity in
  let new_entity = add_item_bag item2 new_entity in
  let new_entity = add_item_bag item3 new_entity in

  (* Test : retirer un item à un index invalide (négatif) *)
  Printf.printf "- Testing invalid index (negative)\n";
  assert_raises (Invalid_argument "Item not found in bag") (fun () -> remove_item_bag (-1) new_entity);

  (* Test : retirer un item à un index invalide (trop grand) *)
  Printf.printf "- Testing invalid index (out of bounds)\n";
  assert_raises (Invalid_argument "Item not found in bag") (fun () -> remove_item_bag 3 new_entity);

  (* Test : retirer le premier item *)
  Printf.printf "- Testing remove first item\n";
  let updated_entity = remove_item_bag 0 new_entity in
  assert_equal 2 (List.length updated_entity.bag.items);
  assert_equal item2 (List.nth updated_entity.bag.items 0);

  (* Test : retirer le dernier item *)
  Printf.printf "- Testing remove last item\n";
  let updated_entity = remove_item_bag 1 updated_entity in
  assert_equal 1 (List.length updated_entity.bag.items);
  assert_equal item2 (List.nth updated_entity.bag.items 0);

  (* Test : retirer l'item restant *)
  Printf.printf "- Testing remove remaining item\n";
  let updated_entity = remove_item_bag 0 updated_entity in
  assert_equal 0 (List.length updated_entity.bag.items);

  Printf.printf "remove_item_bag test passed\n"

let test_is_enemy_at_target _ =
  Printf.printf "Testing is_enemy_at_target :\n";

  (* Création d'une liste d'ennemis *)
  let empty_entity1 = empty_entity () in
  let empty_entity2 = empty_entity () in
  let enemy1 = {empty_entity1 with position = {world_x = 1.0; world_y = 1.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0}} in
  let enemy2 = {empty_entity2 with position = {world_x = 2.0; world_y = 2.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0}} in
  let enemys = [enemy1; enemy2] in

  (* Test : ennemi présent à la position cible *)
  Printf.printf "- Testing enemy at target position\n";
  assert_bool "Expected enemy at (1, 1)" (is_enemy_at_target 1 1 enemys);

  (* Test : aucun ennemi à la position cible *)
  Printf.printf "- Testing no enemy at target position\n";
  assert_bool "Expected no enemy at (3, 3)" (not (is_enemy_at_target 3 3 enemys));

  (* Test : ennemi à une autre position *)
  Printf.printf "- Testing enemy at another position\n";
  assert_bool "Expected enemy at (2, 2)" (is_enemy_at_target 2 2 enemys);

  Printf.printf "is_enemy_at_target test passed\n"

let test_is_wall_at_target _ =
  Printf.printf "Testing is_wall_at_target :\n";

  (* Création d'une liste de tuiles *)
  let tiles = [
    {x = 0; y = 0; texture_id = 1; biome_id = 1}; (* Tuile valide *)
    {x = 1; y = 1; texture_id = 0; biome_id = 1}; (* Mur *)
    {x = 2; y = 2; texture_id = 1; biome_id = 1}; (* Tuile valide *)
  ] in

  (* Test : position sans mur *)
  Printf.printf "- Testing no wall at target position\n";
  assert_bool "Expected no wall at (0, 0)" (not (is_wall_at_target 0 0 tiles));

  (* Test : position avec un mur *)
  Printf.printf "- Testing wall at target position\n";
  assert_bool "Expected wall at (1, 1)" (is_wall_at_target 1 1 tiles);

  (* Test : position hors des tuiles *)
  Printf.printf "- Testing out-of-bounds position\n";
  assert_bool "Expected wall at (3, 3)" (is_wall_at_target 3 3 tiles);

  Printf.printf "is_wall_at_target test passed\n"

let test_is_targeted_by_enemy _ =
  Printf.printf "Testing is_targeted_by_enemy :\n";

  
  (* Création d'une liste d'ennemis *)
  let empty_entity1 = empty_entity () in
  let empty_entity2 = empty_entity () in
  let enemy1 = {empty_entity1 with position = {world_x = 0.0; world_y = 0.0; screen_x = 0; screen_y = 0; target_x = 1.0; target_y = 1.0}} in
  let enemy2 = {empty_entity2 with position = {world_x = 2.0; world_y = 2.0; screen_x = 0; screen_y = 0; target_x = 3.0; target_y = 3.0}} in
  let enemys = [enemy1; enemy2] in

  (* Test : position ciblée par un ennemi *)
  Printf.printf "- Testing position targeted by an enemy\n";
  assert_bool "Expected position (1, 1) to be targeted" (is_targeted_by_enemy 1 1 enemys);

  (* Test : position non ciblée par un ennemi *)
  Printf.printf "- Testing position not targeted by any enemy\n";
  assert_bool "Expected position (4, 4) not to be targeted" (not (is_targeted_by_enemy 4 4 enemys));

  (* Test : position ciblée par un autre ennemi *)
  Printf.printf "- Testing position targeted by another enemy\n";
  assert_bool "Expected position (3, 3) to be targeted" (is_targeted_by_enemy 3 3 enemys);

  Printf.printf "is_targeted_by_enemy test passed\n"

let test_move _ =
  Printf.printf "Testing move :\n";

  (* Création d'une entité *)
  let empty_entity = empty_entity () in
  let entity = {empty_entity with your_turn = true; moving = false; action = Nothing} in

  (* Test : déplacement dans une direction valide *)
  Printf.printf "- Testing valid move\n";
  let moved_entity = move Up entity true false in
  assert_bool "Expected entity to be moving" moved_entity.moving;
  assert_equal Moving moved_entity.state;
  assert_equal Up moved_entity.direction;

  (* Test : attaque en étant à portée *)
  Printf.printf "- Testing attack in range\n";
  let attack_entity = move Up entity true true in
  assert_bool "Expected entity not to be moving" (not attack_entity.moving);
  assert_equal Attack attack_entity.action;
  assert_equal Idle attack_entity.state;

  (* Test : aucune action si aucune touche n'est pressée *)
  Printf.printf "- Testing no key pressed\n";
  let no_action_entity = move Up entity false false in
  assert_bool "Expected entity not to be moving" (not no_action_entity.moving);
  assert_equal Nothing no_action_entity.action;

  (* Test : aucune action si ce n'est pas le tour de l'entité *)
  Printf.printf "- Testing not entity's turn\n";
  let not_turn_entity = {entity with your_turn = false} in
  let no_turn_action = move Up not_turn_entity true false in
  assert_bool "Expected entity not to be moving" (not no_turn_action.moving);
  assert_equal Nothing no_turn_action.action;

  Printf.printf "move test passed\n"

let test_action_player _ =
  Printf.printf "Testing action_player :\n";

  (* Création d'une entité *)
  let empty_entity = empty_entity () in
  let entity = {empty_entity with your_turn = true; action = Nothing} in

  (* Test : action Attack *)
  Printf.printf "- Testing Attack action\n";
  let attack_entity = action_player Attack entity true in
  assert_equal Attack attack_entity.action;

  (* Test : action OpenBag *)
  Printf.printf "- Testing OpenBag action\n";
  let open_bag_entity = action_player OpenBag entity true in
  assert_equal OpenBag open_bag_entity.action;

  (* Test : action PickUp *)
  Printf.printf "- Testing PickUp action\n";
  let pick_up_entity = action_player PickUp entity true in
  assert_equal PickUp pick_up_entity.action;

  (* Test : action Nothing *)
  Printf.printf "- Testing Nothing action\n";
  let nothing_entity = action_player Nothing entity true in
  assert_equal Nothing nothing_entity.action;

  (* Test : aucune action si ce n'est pas le tour de l'entité *)
  Printf.printf "- Testing not entity's turn\n";
  let not_turn_entity = {entity with your_turn = false} in
  let no_action_entity = action_player Attack not_turn_entity true in
  assert_equal Nothing no_action_entity.action;

  (* Test : aucune action si aucune touche n'est pressée *)
  Printf.printf "- Testing no key pressed\n";
  let no_key_entity = action_player Attack entity false in
  assert_equal Nothing no_key_entity.action;

  Printf.printf "action_player test passed\n"

let test_is_end_moving _ =
  Printf.printf "Testing is_end_moving :\n";

  (* Création d'une entité *)
  let empty_entity = empty_entity () in
  let entity = {empty_entity with moving = true; position = {world_x = 1.0; world_y = 1.0; screen_x = 0; screen_y = 0; target_x = 1.0; target_y = 1.0}} in

  (* Test : l'entité a atteint sa position cible *)
  Printf.printf "- Testing entity has reached target position\n";
  let updated_entity, has_reached = is_end_moving entity in
  assert_bool "Expected entity to stop moving" (not updated_entity.moving);
  assert_equal Nothing updated_entity.action;
  assert_bool "Expected entity to have reached target" has_reached;

  (* Test : l'entité n'a pas atteint sa position cible *)
  Printf.printf "- Testing entity has not reached target position\n";
  let entity_not_reached = {entity with position = {world_x = 0.0; world_y = 0.0; screen_x = 0; screen_y = 0; target_x = 1.0; target_y = 1.0}} in
  let updated_entity, has_reached = is_end_moving entity_not_reached in
  assert_bool "Expected entity to keep moving" updated_entity.moving;
  assert_bool "Expected entity not to have reached target" (not has_reached);

  Printf.printf "is_end_moving test passed\n"

let test_update_entity_texture_id _ =
  Printf.printf "Testing update_entity_texture_id :\n";

  (* Création d'une entité fictive *)
  let entity = {
    (empty_entity ()) with
    entity_textures_id = 0;
    state = Idle;
    direction = Down;
  } in

  (* Test : mise à jour de la texture en état Idle et direction Down *)
  Printf.printf "- Testing Idle state, Down direction\n";
  let updated_entity = update_entity_texture_id entity in
  Printf.printf "Expected: 1, Got: %d\n" updated_entity.entity_textures_id;
  assert_equal 1 updated_entity.entity_textures_id;

  (* Test : mise à jour de la texture en état Moving et direction Right *)
  Printf.printf "- Testing Moving state, Right direction\n";
  let moving_entity = { entity with state = Moving; direction = Right; entity_textures_id = 16 } in
  let updated_moving_entity = update_entity_texture_id moving_entity in
  Printf.printf "Expected: 25, Got: %d\n" updated_moving_entity.entity_textures_id;
  assert_equal 25 updated_moving_entity.entity_textures_id;

  (* Test : dépassement de l'identifiant maximum *)
  Printf.printf "- Testing texture ID overflow\n";
  let overflow_entity = { entity with entity_textures_id = 41 } in
  let updated_overflow_entity = update_entity_texture_id overflow_entity in
  Printf.printf "Expected: 41, Got: %d\n" updated_overflow_entity.entity_textures_id;
  assert_equal 41 updated_overflow_entity.entity_textures_id;

  Printf.printf "update_entity_texture_id test passed\n"

let test_player_get_target _ =
  Printf.printf "Testing player_get_target :\n";

  (* Création d'une entité fictive *)
  let player = {
    (empty_entity ()) with
    position = { world_x = 5.0; world_y = 5.0; screen_x = 0; screen_y = 0; target_x = 0.0; target_y = 0.0 };
  } in

  (* Test : direction Up *)
  Printf.printf "- Testing direction Up\n";
  let player_up = { player with direction = Up } in
  let (target_x, target_y) = player_get_target player_up in
  assert_equal 5.0 target_x;
  assert_equal 4.0 target_y;

  (* Test : direction Down *)
  Printf.printf "- Testing direction Down\n";
  let player_down = { player with direction = Down } in
  let (target_x, target_y) = player_get_target player_down in
  assert_equal 5.0 target_x;
  assert_equal 6.0 target_y;

  (* Test : direction Left *)
  Printf.printf "- Testing direction Left\n";
  let player_left = { player with direction = Left } in
  let (target_x, target_y) = player_get_target player_left in
  assert_equal 4.0 target_x;
  assert_equal 5.0 target_y;

  (* Test : direction Right *)
  Printf.printf "- Testing direction Right\n";
  let player_right = { player with direction = Right } in
  let (target_x, target_y) = player_get_target player_right in
  assert_equal 6.0 target_x;
  assert_equal 5.0 target_y;

  (* Test : direction DiagonalUpLeft *)
  Printf.printf "- Testing direction DiagonalUpLeft\n";
  let player_diag_up_left = { player with direction = DiagonalUpLeft } in
  let (target_x, target_y) = player_get_target player_diag_up_left in
  assert_equal 4.0 target_x;
  assert_equal 4.0 target_y;

  (* Test : direction DiagonalUpRight *)
  Printf.printf "- Testing direction DiagonalUpRight\n";
  let player_diag_up_right = { player with direction = DiagonalUpRight } in
  let (target_x, target_y) = player_get_target player_diag_up_right in
  assert_equal 6.0 target_x;
  assert_equal 4.0 target_y;

  (* Test : direction DiagonalDownLeft *)
  Printf.printf "- Testing direction DiagonalDownLeft\n";
  let player_diag_down_left = { player with direction = DiagonalDownLeft } in
  let (target_x, target_y) = player_get_target player_diag_down_left in
  assert_equal 4.0 target_x;
  assert_equal 6.0 target_y;

  (* Test : direction DiagonalDownRight *)
  Printf.printf "- Testing direction DiagonalDownRight\n";
  let player_diag_down_right = { player with direction = DiagonalDownRight } in
  let (target_x, target_y) = player_get_target player_diag_down_right in
  assert_equal 6.0 target_x;
  assert_equal 6.0 target_y;

  (* Test : direction No_move *)
  Printf.printf "- Testing direction No_move\n";
  let player_no_move = { player with direction = No_move } in
  let (target_x, target_y) = player_get_target player_no_move in
  assert_equal 5.0 target_x;
  assert_equal 5.0 target_y;

  Printf.printf "player_get_target test passed\n"

(* Test suite *)

let suite = 
  "EntityModel Tests" >::: [
    "test_set_entity_target" >:: test_set_entity_target;
    "test_set_i_competence_puissance" >:: test_set_i_competence_puissance;
    "test_set_unable_item_bag" >:: test_set_unable_item_bag;
    "test_remove_item_bag" >:: test_remove_item_bag;
    "test_is_enemy_at_target" >:: test_is_enemy_at_target;
    "test_is_wall_at_target" >:: test_is_wall_at_target;
    "test_is_targeted_by_enemy" >:: test_is_targeted_by_enemy;
    "test_move" >:: test_move;
    "test_action_player" >:: test_action_player;
    "test_is_end_moving" >:: test_is_end_moving;
    "test_update_entity_texture_id" >:: test_update_entity_texture_id;
    "test_player_get_target" >:: test_player_get_target;
  ]
