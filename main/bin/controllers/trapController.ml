open Utils.Types
open Models.EntityModel
open Models.EnemyModel
open Models.Trap_ground
open Models.Generation_map
open Models.Map_model

(**
  [update_trap_and_ground map player trap_and_ground enemys items] updates the trap and ground.
  @param map The map.
  @param player The player.
  @param trap_and_ground The trap and ground.
  @param enemys The enemy.
  @param items The items.
  @return The updated map, player, trap and ground, enemy and items.
*)
let update_trap_and_ground map player traps_and_grounds enemys item last_time =
  let msg = "" in
  let (pos_x, pos_y) = get_entity_position player in
  let pos_x = int_of_float pos_x in
  let pos_y = int_of_float pos_y in
  if is_trap_ground traps_and_grounds pos_x pos_y && player.your_turn then begin
    match get_trap_ground traps_and_grounds pos_x pos_y with
    | None -> (map, player, traps_and_grounds, enemys, item, last_time, msg)
    | Some trap_and_ground ->
      let traps_and_grounds = set_trap_ground_pos_visibility pos_x pos_y true traps_and_grounds in
      match trap_and_ground.nature, trap_and_ground.visibility with
      | Stairs_Up, true -> 
        Unix.sleep 1;
        let (new_map, new_player, new_trap_and_ground, new_enemys, new_items) = generation_map map.floor player.number in
        let list_of_last_time = List.init (7 + ((List.length(new_enemys))*2)) (fun _ -> 0.0) in (* Use for animations *)
        let new_map = set_map_floor new_map (new_map.floor + 1) in
        (new_map, new_player, new_trap_and_ground, new_enemys, new_items, list_of_last_time, msg)
      | Stairs_Down, true ->
        Unix.sleep 1;
        let (new_map, new_player, new_trap_and_ground, new_enemys, new_items) = generation_map map.floor player.number in
        let list_of_last_time = List.init (7 + ((List.length(new_enemys))*2)) (fun _ -> 0.0) in (* Use for animations *)
        let new_map = set_map_floor new_map (new_map.floor - 1) in
        (new_map, new_player, new_trap_and_ground, new_enemys, new_items, list_of_last_time, msg)
      | Bug_Switch, false ->
        let player = set_entity_step_cpt 3 player in
        let msg = "You are stuck in a web" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Chestnut_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 10) player in
        let msg = "You are stuck in a chestnut" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Drop_Hole, false ->
        (* FIXME *)
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Explosion_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 20) player in
        let msg = "You are exploded" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Fan_Switch, false ->
        let random_x = Random.int 4 in
        let direction = 
          match random_x with
          | 0 -> 
            Right
          | 1 -> 
            Left
          | 2 -> 
            Up
          | 3 -> 
            Down
          | _ -> 
            Down
        in
        let player = 
          match find_wall_in_direction pos_x pos_y direction map with
              | Some (x, y) -> 
                let x = float_of_int x in
                let y = float_of_int y in
                player 
                |> set_entity_position x y
                |> set_entity_target x y
              | None -> player
        in
        let msg = "You are blown away" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)    
      | Glue_Switch, false ->
        let length = List.length player.bag.items in
        if length = 0 then 
          (map, player, traps_and_grounds, enemys, item, last_time, msg)
        else if length = 1 then
          let player = 
            try
              set_usable_item_bag 0 false player 
            with _ -> player
          in
          let msg = "You are stuck in glue" in
          (map, player, traps_and_grounds, enemys, item, last_time, msg)
        else
          let rand = Random.int length in
          let player = 
            try
              set_usable_item_bag rand false player 
            with _ -> player
          in
          let msg = "You are stuck in glue" in
          (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Grimer_Switch, false ->
        (* FIXME *)
        let msg = "You are stuck in grimer" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Imprison_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 1) player in
        let msg = "You are imprisoned" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Mud_Switch, false ->
        let random_x = Random.int 2 in
        let player = 
          match random_x with
          | 0 -> 
            set_entity_current_hp (player.current_hp - 1) player
          | _ -> 
            try 
              set_i_competence_puissance 0 ((List.nth player.competence 0).puissance - 1) player
            with _ -> player
        in
        let msg = "You are stuck in mud" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Poison_Sting_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 1) player in
        let msg = "You are poisoned" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Pokemon_Switch, false ->
        (* FIXME *)
        let msg = "You are transformed" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Self_Destruct_Switch, false ->
        let player = set_entity_current_hp (player.current_hp - 20) player in
        let map = set_map_exploded pos_x pos_y map in
        let msg = "You are exploded" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Skill_Drop_Switch, false ->
        let player = 
          try 
            set_i_competence_puissance 0 0 player
          with _ -> player
        in
        let msg = "You are confused" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Slowpoke_Switch, false ->
        let player = set_entity_speed 0.5 player in
        let msg = "You are slowed down" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Spin_Swith, false ->
        let player = set_entity_step_cpt 3 player in
        let msg = "You are dizzy" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Summon_Switch, false ->
        let rec add_random_enemy player map enemys =
          let (p_pos_x, p_pos_y) = get_entity_position player in
          let random_x = p_pos_x +. float_of_int (Random.int 11 - 5) in
          let random_y = p_pos_y +. float_of_int (Random.int 11 - 5) in
          if not(is_wall (int_of_float random_x) (int_of_float random_y) map) then
            let new_enemy, player = create_enemy random_x random_y player in
            new_enemy :: enemys, player
          else
            add_random_enemy player map enemys
        in
        let last_time = last_time @ [0.0; 0.0] in
        let enemys, player = add_random_enemy player map enemys in
        let msg = "You are summoned" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | Warp_Trap, false ->
        let rec find_random_position () =
          let random_x = float_of_int (Random.int map.width) in
          let random_y = float_of_int (Random.int map.height) in
          if not (is_wall (int_of_float random_x) (int_of_float random_y) map) && not (List.exists (fun e -> let (e_pos_x, e_pos_y) = get_entity_position e in e_pos_x = random_x && e_pos_y = random_y) enemys) then
            (random_x, random_y)
          else
            find_random_position ()
        in
        let (new_x, new_y) = find_random_position () in
        let player =
          player
          |> set_entity_position new_x new_y
          |> set_entity_target new_x new_y
        in
        let msg = "You are teleported" in
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
      | _ ->
        (map, player, traps_and_grounds, enemys, item, last_time, msg)
  end else
    (map, player, traps_and_grounds, enemys, item, last_time, msg)
