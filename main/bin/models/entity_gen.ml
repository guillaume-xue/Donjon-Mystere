open Utils.Types

(**
  [spawn_player] génère une position aléatoire pour le joueur sur la carte.
  @param map La carte sur laquelle le joueur doit être généré.
  @return Le joueur généré.
*)
let spawn_player map =
  let zone_rand = Random.int (List.length map.regions) in
  let case_rand = Random.int (List.length (List.nth map.regions zone_rand).tiles) in
  let tile = List.nth (List.nth map.regions zone_rand).tiles case_rand in
  { pos_x = float_of_int tile.x; pos_y = float_of_int tile.y; screen_x = 0; screen_y = 0; entity_textures_id = 0; target_x = float_of_int tile.x; target_y = float_of_int tile.y; moving = false; state = Idle; direction = Down; current_hp = 20; max_hp = 20; level = 1; current_xp = 0; max_xp = 100 }


(**
  [spawn_list_of_enemys] génère une liste d'ennemis sur la carte.
  @param map La carte sur laquelle les ennemis doivent être générés.
  @return La liste d'ennemis générée.
*)
let spawn_list_of_enemys map (player: pokemon) =
  let rec aux regions acc =
    match regions with
    | [] -> acc
    | region :: rest ->
      let case_rand = Random.int region.size in
      let tile = List.nth region.tiles case_rand in
      if player.pos_x = float_of_int tile.x && player.pos_y = float_of_int tile.y then
        aux rest acc
      else
        let enemy = { pos_x = float_of_int tile.x; pos_y = float_of_int tile.y; screen_x = 0; screen_y = 0; entity_textures_id = 0; target_x = float_of_int tile.x; target_y = float_of_int tile.y; moving = false; state = Idle; direction = Down; current_hp = 10; max_hp = 10; level = 1; current_xp = 0; max_xp = 100 } in
      aux rest (enemy :: acc)
  in
  aux map.regions []

let spawn_list_of_loot map =
  let rec aux regions cpt acc =
    match regions with
    | [] -> acc
    | region :: rest ->
      let case_rand = Random.int region.size in
      let tile = List.nth region.tiles case_rand in
      let loot = { item_id = cpt; item_skin_id = 0; quantity = 1; pos_x = float_of_int tile.x; pos_y = float_of_int tile.y; screen_x = 0; screen_y = 0; description = ""} in
      aux rest (cpt+1) (loot :: acc)
  in
  aux map.regions 0 []