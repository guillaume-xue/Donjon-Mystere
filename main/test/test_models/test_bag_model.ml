open OUnit2
open Models.Bag_model
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
    bag = { items = []; max_size = 5; selected_item = 0};
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
    money = 0;
  }

let test_set_usable_item_bag _ =
  Printf.printf "Testing set_usable_item_bag :\n";

  (* Création d'un Pokémon fictif avec un sac contenant des objets *)
  let item1 = { item_id = 1; item_skin_id = 101; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Potion"; usable = false } in
  let item2 = { item_id = 2; item_skin_id = 102; quantity = 1; pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; description = "Antidote"; usable = false } in
  let bag = { items = [item1; item2]; max_size = 10; selected_item = 0} in
  let pokemon = { (empty_entity ()) with bag = bag } in

  (* Test : rendre le premier objet utilisable *)
  let updated_pokemon = set_usable_item_bag 0 true pokemon in
  let updated_item1 = List.nth updated_pokemon.bag.items 0 in
  assert_equal true updated_item1.usable;
  assert_equal "Potion" updated_item1.description;

  (* Test : rendre le deuxième objet inutilisable *)
  let updated_pokemon = set_usable_item_bag 1 false updated_pokemon in
  let updated_item2 = List.nth updated_pokemon.bag.items 1 in
  assert_equal false updated_item2.usable;
  assert_equal "Antidote" updated_item2.description;

  Printf.printf "set_usable_item_bag test passed\n"

(* Test suite *)
let suite =
  "BagModel Tests" >::: [
    "test_set_usable_item_bag" >:: test_set_usable_item_bag;
  ]

let () =
  run_test_tt_main suite