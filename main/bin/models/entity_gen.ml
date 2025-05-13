open Utils.Types
open Map_model
open Utils.Settings_map
open Trap_ground
open Utils.Competences_data
open Combat
open A_star

let entity_base_value = (50, 55, 45, 60, 50)

let randLvl pokemon = 
  let r = Random.int 1 in
  match r with
    | 0 -> pokemon.level + Random.int 1
    | _ -> pokemon.level - Random.int 4

let genIV () = 
  let pv = Random.int 25 in
  let att = Random.int 25 in
  let def = Random.int 25 in
  let att_sp = Random.int 25 in
  let def_sp = Random.int 25 in
  (pv, att, def, att_sp, def_sp)

let finalGen lvl () =
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
  [spawn_player] génère une position aléatoire pour le joueur sur la carte.
  @param map La carte sur laquelle le joueur doit être généré.
  @return Le joueur généré.
*)
let spawn_player map num_pokemon =
  let (cur_hp, att, def, att_sp, def_sp) = finalGen 25.0 () in
  let rec tile_rand () =
    let zone_rand = Random.int (List.length map.regions) in
    let case_rand = Random.int (List.length (List.nth map.regions zone_rand).tiles) in
    
  (* Printf.printf "cur_hp: %d, att: %d, def: %d, att_sp: %d, def_sp: %d\n" cur_hp att def att_sp def_sp; *)
  let tile = List.nth (List.nth map.regions zone_rand).tiles case_rand in
    if is_wall tile.x tile.y map then
      tile_rand ()
    else
      zone_rand, tile
  in
  let (zone_rand, tile) = tile_rand () in
  ({
    id = num_pokemon;
    last_id = 0;
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
    current_hp = cur_hp;
    max_hp = cur_hp;
    level = 5;
    current_xp = 0;
    max_xp = neededXp 5;
    action = Nothing;
    bag = { items = []; max_size = 10 };
    step_cpt = 0;
    speed = 1.0;
    
    attaque = att;
    defense = def;
    attaque_speciale = att_sp;
    defense_speciale = def_sp;
    element = Feu;
    competence = [attaque_charge()];
    path = [];
    your_turn = true;
  }, zone_rand)

(**
  [spawn_list_of_enemys] génère une liste d'ennemis sur la carte.
  @param map La carte sur laquelle les ennemis doivent être générés.
  @return La liste d'ennemis générée.
*)
let spawn_list_of_enemys map (player: pokemon) =
  let rec aux regions acc cpt =
    match regions with
    | [] -> acc, cpt
    | region :: rest ->
      let case_rand = Random.int region.size in
      let tile = List.nth region.tiles case_rand in
      let rand = (Random.int 8) + 3 in
      if List.exists (fun e -> e.pos_x = float_of_int tile.x && e.pos_y = float_of_int tile.y) (player :: acc) then
        aux regions acc cpt
      else
        let lvl = randLvl player in
        let (cur_hp, att, def, att_sp, def_sp) = finalGen (float_of_int lvl) () in
        let enemy = {
          id = cpt;
          last_id = 0;
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
          current_hp = cur_hp;
          max_hp = cur_hp;
          level = lvl;
          current_xp = 0;
          max_xp = 0;
          action = Nothing;
          bag = { items = []; max_size = 5 };
          step_cpt = 0;
          speed = 1.0;
          attaque = att;
          defense = def;
          attaque_speciale = att_sp;
          defense_speciale = def_sp;
          element = Feu;
          competence = [attaque_grosyeux()];
          path = a_star map.tiles (tile.x, tile.y) (int_of_float player.pos_x, int_of_float player.pos_y);
          your_turn = false;
        } in
        aux rest (enemy :: acc) (cpt+1)
  in
  aux map.regions [] 1

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


