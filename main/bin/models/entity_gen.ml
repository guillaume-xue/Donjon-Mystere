open Utils.Types
open Map_model
open Utils.Settings_map
open Trap_ground

(**
  [spawn_player] génère une position aléatoire pour le joueur sur la carte.
  @param map La carte sur laquelle le joueur doit être généré.
  @return Le joueur généré.
*)
let spawn_player map =
  let rec tile_rand () =
    let zone_rand = Random.int (List.length map.regions) in
    let case_rand = Random.int (List.length (List.nth map.regions zone_rand).tiles) in
    let tile = List.nth (List.nth map.regions zone_rand).tiles case_rand in
    if is_wall tile.x tile.y map then
      tile_rand ()
    else
      zone_rand, tile
  in
  let (zone_rand, tile) = tile_rand () in
  ({
    number = 0;
    pos_x = float_of_int tile.x;
    pos_y = float_of_int tile.y;
    screen_x = (screen_width / 2);
    screen_y = (screen_height / 2);
    entity_textures_id = 24;
    target_x = float_of_int tile.x;
    target_y = float_of_int tile.y;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = 20;
    max_hp = 20;
    level = 1;
    current_xp = 0;
    max_xp = 100;
    attacking = false;
    action = Nothing;
    bag = { items = []; max_size = 10 };
    step_cpt = 0;
    speed = 1.0
  }, zone_rand)

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
      let rand = (Random.int 8) + 3 in
      if player.pos_x = float_of_int tile.x && player.pos_y = float_of_int tile.y then
        aux rest acc
      else
        let enemy = {
          number = rand;
          pos_x = float_of_int tile.x;
          pos_y = float_of_int tile.y;
          screen_x = 0;
          screen_y = 0;
          entity_textures_id = 0;
          target_x = float_of_int tile.x;
          target_y = float_of_int tile.y;
          moving = false;
          state = Idle;
          direction = Down;
          current_hp = 10;
          max_hp = 10;
          level = 1;
          current_xp = 0;
          max_xp = 100;
          attacking = false;
          action = Nothing;
          bag = { items = []; max_size = 5 };
          step_cpt = 0;
          speed = 1.0
        } in
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
      let loot = { item_id = cpt; item_skin_id = 0; quantity = 1; pos_x = float_of_int tile.x; pos_y = float_of_int tile.y; screen_x = 0; screen_y = 0; description = ""; usable = true} in
      aux rest (cpt+1) (loot :: acc)
  in
  aux map.regions 0 []

let spawn_list_of_trap_and_ground map zone =
  let gen_stair () =
    let rec zone_tiles () =
      let zone_rand = Random.int (List.length map.regions) in
      if zone_rand = zone then 
        zone_tiles()
      else 
        zone_rand 
    in
    let zone_rand = zone_tiles() in
    let case_rand = Random.int (List.length (List.nth map.regions zone_rand).tiles) in
    let tile = List.nth (List.nth map.regions zone_rand).tiles case_rand in
    { 
      nature = Stairs_Up;
      tag_pos_x = tile.x;
      tag_pos_y = tile.y;
      visibility = true;
    }
  in
  (* Fonction pour générer un piège aléatoire *)
  let random_trap excluded_indices =
    (* Filtrer les pièges exclus *)
    let filtered_traps = List.mapi (fun i trap -> (i, trap)) traps
                        |> List.filter (fun (i, _) -> not (List.mem i excluded_indices))
                        |> List.map snd in
    (* Calculer la somme des probabilités *)
    let total_weight = List.fold_left (fun acc (_, weight) -> acc +. weight) 0.0 filtered_traps in
    (* Générer un nombre aléatoire entre 0 et la somme des probabilités *)
    let rand = Random.float total_weight in
    (* Trouver le piège correspondant *)
    let rec find_trap acc = function
      | [] -> failwith "No traps available"
      | (trap, weight) :: rest ->
          if rand <= acc +. weight then trap
          else find_trap (acc +. weight) rest
    in
    find_trap 0.0 filtered_traps
  in
  let rec gen_trap i res =
    if i < map.floor then
      begin
        let zone_rand = Random.int (List.length map.regions) in
        let case_rand = Random.int (List.length (List.nth map.regions zone_rand).tiles) in
        let tile = List.nth (List.nth map.regions zone_rand).tiles case_rand in
        if is_trap_ground res tile.x tile.y then 
          gen_trap i res
        else
          let trap = { 
            nature = (random_trap []);
            tag_pos_x = tile.x;
            tag_pos_y = tile.y;
            visibility = false;
          } in
          gen_trap (i + 1) (trap :: res);
      end
    else
      res
  in
  let traps_grounds = [gen_stair ()] in
  gen_trap 0 traps_grounds


