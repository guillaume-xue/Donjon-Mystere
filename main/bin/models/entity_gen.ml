open Utils.Types
open Map_model
open Utils.Settings_map
open Trap_ground
open Utils.Competences_data
open Combat
open A_star
open EntityModel

(**
  [entity_base_value] is a tuple containing the base values of the entity.
  @return A tuple containing the base values of the entity.
*)
let entity_base_value = (50, 55, 45, 60, 50)

(**
  [nom_pokemon] is a list of Pokémon names.
  @return A list of Pokémon names.
*)
let nom_pokemon = [
  "Germignon";
  "Héricendre";
  "Kaiminus";
  "Pyroli";
  "Aquali";
  "Phyllali";
  "Evoli";
  "Voltali";
  "Mentali";
  "Noctali";
  "Givrali";
]

(**
  [randLvl pokemon] generates a random level for the Pokémon.
  @param pokemon The Pokémon for which to generate a random level.
  @return A random level for the Pokémon.
*)
let randLvl (pokemon : pokemon) : int = 
  let r = Random.int 1 in
  match r with
    | 0 -> pokemon.level + Random.int 1
    | _ -> pokemon.level - Random.int 4

(**
  [genIV] generates random individual values for the Pokémon.
  @return A tuple containing the individual values for the Pokémon.
*)
let genIV () : int * int * int * int * int = 
  let pv = Random.int 25 in
  let att = Random.int 25 in
  let def = Random.int 25 in
  let att_sp = Random.int 25 in
  let def_sp = Random.int 25 in
  (pv, att, def, att_sp, def_sp)

(**
  [finalGen lvl] calculates the final stats of the Pokémon based on its level and individual values.
  @param lvl The level of the Pokémon.
  @return A tuple containing the final stats of the Pokémon.
*)
let finalGen (lvl : float) : int * int * int * int * int =
  let const1, const2 = (20, 5) in
  let (pv, att, def, att_sp, def_sp) = genIV () in
  let (pv_base, att_base, def_base, att_sp_base, def_sp_base) = entity_base_value in
  let pv_final = int_of_float ((float_of_int pv +. float_of_int pv_base) *. (lvl /. 50.0) +. float_of_int const1) in
  let att_final = int_of_float ((float_of_int att +. float_of_int att_base) *. (lvl /. 50.0) +. float_of_int const2) in
  let def_final = int_of_float ((float_of_int def +. float_of_int def_base) *. (lvl /. 50.0) +. float_of_int const2) in
  let att_sp_final = int_of_float ((float_of_int att_sp +. float_of_int att_sp_base) *. (lvl /. 50.0) +. float_of_int const2) in
  let def_sp_final = int_of_float ((float_of_int def_sp +. float_of_int def_sp_base) *. (lvl /. 50.0) +. float_of_int const2) in
  (pv_final, att_final, def_final, att_sp_final, def_sp_final)

(**
  [spawn_player_pos] generates a random position for the player on the map.
  @param map The map on which the player should be generated.
  @param player The player to be generated.
  @return A tuple containing the generated player and the zone in which it was generated.
*)
let spawn_player_pos (map : map) (player : pokemon) : pokemon * int =
  let rec tile_rand () : int * tile =
    let zone_rand = Random.int (List.length map.regions) in
    let case_rand = Random.int (List.length (List.nth map.regions zone_rand).tiles) in
    let tile = List.nth (List.nth map.regions zone_rand).tiles case_rand in
    if is_wall tile.x tile.y map then
      tile_rand ()
    else
      zone_rand, tile
  in
  let (zone_rand, tile) = tile_rand () in
  ({player with last_id = 0; 
    position = {
      world_x = float_of_int tile.x;
      world_y = float_of_int tile.y;
      screen_x = (screen_width / 2);
      screen_y = (screen_height / 2);
      target_x = float_of_int tile.x;
      target_y = float_of_int tile.y;
    };
    your_turn = true;}, zone_rand)

(**
  [spawn_player] génère une position aléatoire pour le joueur sur la carte.
  @param map La carte sur laquelle le joueur doit être généré.
  @param num_pokemon Le numéro du Pokémon à générer.
  @return Le joueur généré.
*)
let spawn_player (map : map) (num_pokemon : int) : pokemon * int =
  let (cur_hp, att, def, att_sp, def_sp) = finalGen 50.0 in
  let rec tile_rand () : int * tile =
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
    nom = List.nth nom_pokemon num_pokemon;
    id = 0;
    last_id = 0;
    number = num_pokemon;
    position = {
      world_x = float_of_int tile.x;
      world_y = float_of_int tile.y;
      screen_x = (screen_width / 2);
      screen_y = (screen_height / 2);
      target_x = float_of_int tile.x;
      target_y = float_of_int tile.y;
    };
    entity_textures_id = 24;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = cur_hp;
    max_hp = cur_hp;
    level = 5;
    current_xp = 0;
    max_xp = neededXp 5;
    action = Nothing;
    bag = { items = []; max_size = 10 ; selected_item = 0};
    step_cpt = 0;
    speed = 1.0;
    attaque = att;
    defense = def;
    attaque_speciale = att_sp;
    defense_speciale = def_sp;
    element = List.nth [Plante; Feu; Eau] num_pokemon;
    competence = [attaque_charge(); List.nth competences (element_to_index (List.nth [Plante; Feu; Eau] num_pokemon))];
    path = [];
    your_turn = true;
    money = 0;
  }, zone_rand)

(**
  [spawn_list_of_enemys] génère une liste d'ennemis sur la carte.
  @param map La carte sur laquelle les ennemis doivent être générés.
  @param player Le joueur sur la carte.
  @return La liste d'ennemis générée.
*)
let spawn_list_of_enemys (map: map) (player: pokemon) : pokemon list * int =
  let rec aux (regions : zone list) (acc : pokemon list) (cpt : int) : pokemon list * int =
    match regions with
    | [] -> acc, cpt
    | region :: rest ->
      let case_rand = Random.int region.size in
      let tile = List.nth region.tiles case_rand in
      let rand = (Random.int 8) + 3 in
      if List.exists (fun e -> 
        let (e_pos_x, e_pos_y) = get_entity_position e in
        e_pos_x = float_of_int tile.x && e_pos_y = float_of_int tile.y
        ) (player :: acc) then
        aux regions acc cpt
      else
        let lvl = randLvl player in
        let (cur_hp, att, def, att_sp, def_sp) = finalGen (float_of_int lvl) in
        let (pos_x, pos_y) = get_entity_position player in
        let enemy = {
          nom = List.nth nom_pokemon rand;
          id = cpt;
          last_id = 0;
          number = rand;
          position = {
            world_x = float_of_int tile.x;
            world_y = float_of_int tile.y;
            screen_x = 0;
            screen_y = 0;
            target_x = float_of_int tile.x;
            target_y = float_of_int tile.y;
          };
          entity_textures_id = 0;
          moving = false;
          state = Idle;
          direction = Down;
          current_hp = cur_hp;
          max_hp = cur_hp;
          level = lvl;
          current_xp = 0;
          max_xp = 0;
          action = Nothing;
          bag = { items = []; max_size = 5 ; selected_item = 0};
          step_cpt = 0;
          speed = 1.0;
          attaque = att;
          defense = def;
          attaque_speciale = att_sp;
          defense_speciale = def_sp;
          element = List.nth element_types (rand-3);
          competence = [attaque_charge(); List.nth competences (element_to_index (List.nth element_types (rand-3)))];
          path = a_star map.tiles (tile.x, tile.y) (int_of_float pos_x, int_of_float pos_y);
          your_turn = false;
          money = 0;
        } in
        aux rest (enemy :: acc) (cpt+1)
  in
  aux map.regions [] 1

(**
  [spawn_list_of_loot] generates a list of loot on the map.
  @param map The map on which the loot should be generated.
  @return A list of loot generated on the map.
*)
let spawn_list_of_loot (map : map) : loot list =
  let rec aux (regions : zone list) (cpt : int) (acc : loot list) : loot list =
    match regions with
    | [] -> acc
    | region :: rest ->
      let rec spawn_multiple_loot (boucle : int) (cpt : int) (acc : loot list) (memory : tile list) (region : zone) : loot list * int =
        if boucle < 1 then (acc, cpt)
        else
          let case_rand = Random.int region.size in
          let tile = List.nth region.tiles case_rand in
          if List.exists (fun t -> t.x = tile.x && t.y = tile.y) memory then
            spawn_multiple_loot boucle cpt acc memory region
          else
            let skin_id = Random.int 10 in
            let texture_id = match skin_id with
              | 0 -> 7
              | 1 -> 8
              | 2 -> 9
              | 3 -> 6
              | _ -> 0
            in
            let loot = { item_id = cpt; item_skin_id = texture_id; quantity = 1; pos_x = float_of_int tile.x; pos_y = float_of_int tile.y; screen_x = 0; screen_y = 0; description = item_nom skin_id; usable = true} in
            spawn_multiple_loot (boucle - 1) (cpt + 1) (loot :: acc) (tile :: memory) region
        in
      let nb_loot = Random.int 5 in
      let (loot, cpt) = spawn_multiple_loot nb_loot cpt [] [] region in
      aux rest cpt (loot @ acc)
  in
  aux map.regions 0 []

(**
  [spawn_list_of_trap_and_ground] generates a list of traps and ground on the map.
  @param map The map on which the traps and ground should be generated.
  @param zone The zone in which to generate the traps and ground.
  @return A list of traps and ground generated on the map.
*)
let spawn_list_of_trap_and_ground (map : map) (zone : int) : trap_and_ground list =
  let gen_stair () =
    let rec zone_tiles () : int =
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
    let rec find_trap (acc : float) (filtered_traps : (trap_and_ground_type * float) list)  =
      match filtered_traps with
      | [] -> failwith "No traps available"
      | (trap, weight) :: rest ->
          if rand <= acc +. weight then trap
          else find_trap (acc +. weight) rest
    in
    find_trap 0.0 filtered_traps
  in
  let rec gen_trap (i : int) (res : trap_and_ground list) : trap_and_ground list =
    if i < map.floor + 5 then
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


