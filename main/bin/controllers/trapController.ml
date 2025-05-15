open Utils.Types
open Models.EntityModel
open Models.EnemyModel
open Models.Trap_ground
open Models.Generation_map
open Models.Map_model
open Models.Game_state

(**
  [update_trap_and_ground map player trap_and_ground enemys items] updates the trap and ground.
  @param map The map.
  @param player The player.
  @param trap_and_ground The trap and ground.
  @param enemys The enemy.
  @param items The items.
  @return The updated map, player, trap and ground, enemy and items.
*)
let update_trap_and_ground game_states last_time =
  let (pos_x, pos_y) = get_entity_position game_states.player_state in
  let pos_x = int_of_float pos_x in
  let pos_y = int_of_float pos_y in
  if is_trap_ground game_states.traps_and_grounds_state pos_x pos_y && game_states.player_state.your_turn then begin
    match get_trap_ground game_states.traps_and_grounds_state pos_x pos_y with
    | None -> (game_states, last_time)
    | Some trap_and_ground ->
      let traps_and_grounds = set_trap_ground_pos_visibility pos_x pos_y true game_states.traps_and_grounds_state in
      let game_states = 
        game_states
        |> set_game_state_trap_and_ground traps_and_grounds
      in
      match trap_and_ground.nature, trap_and_ground.visibility with
      | Stairs_Up, true -> 
        Unix.sleep 1;
        let (new_map, new_player, new_trap_and_ground, new_enemys, new_items) = create_new_floor game_states.map_state.floor game_states.player_state in
        let list_of_last_time = List.init (100 + ((List.length(new_enemys))*2)) (fun _ -> 0.0) in (* Use for animations *)
        let new_map = set_map_floor new_map (new_map.floor + 1) in
        let game_states = 
          game_states 
          |> set_game_state_map new_map
          |> set_game_state_player new_player
          |> set_game_state_enemy new_enemys
          |> set_game_state_trap_and_ground new_trap_and_ground
          |> set_game_state_loots new_items
          |> set_game_state_msg []
        in
        (game_states, list_of_last_time)
      | Stairs_Down, true ->
        Unix.sleep 1;
        let (new_map, new_player, new_trap_and_ground, new_enemys, new_items) = create_new_floor game_states.map_state.floor game_states.player_state in
        let list_of_last_time = List.init (100 + ((List.length(new_enemys))*2)) (fun _ -> 0.0) in (* Use for animations *)
        let new_map = set_map_floor new_map (new_map.floor - 1) in
        let game_states = 
          game_states 
          |> set_game_state_map new_map
          |> set_game_state_player new_player
          |> set_game_state_enemy new_enemys
          |> set_game_state_trap_and_ground new_trap_and_ground
          |> set_game_state_loots new_items
          |> set_game_state_msg []
        in
        (game_states, list_of_last_time)
      | Bug_Switch, false ->
        let player = set_entity_step_cpt 3 game_states.player_state in
        let msg = "You are stuck in a web" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Chestnut_Switch, false ->
        let player = set_entity_current_hp (game_states.player_state.current_hp - 10) game_states.player_state in
        let msg = "You are stuck in a chestnut" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Drop_Hole, false ->
        (* FIXME *)
        (game_states, last_time)
      | Explosion_Switch, false ->
        let player = set_entity_current_hp (game_states.player_state.current_hp - 20) game_states.player_state in
        let msg = "You are exploded" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
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
          match find_wall_in_direction pos_x pos_y direction game_states.map_state with
              | Some (x, y) -> 
                let x = float_of_int x in
                let y = float_of_int y in
                game_states.player_state 
                |> set_entity_position x y
                |> set_entity_target x y
              | None -> game_states.player_state
        in
        let msg = "You are blown away" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)    
      | Glue_Switch, false ->
        let length = List.length game_states.player_state.bag.items in
        if length = 0 then 
          (game_states, last_time)
        else if length = 1 then
          let player = 
            try
              set_usable_item_bag 0 false game_states.player_state 
            with _ -> game_states.player_state
          in
          let msg = "You are stuck in glue" in
          let game_states = 
            game_states
            |> set_game_state_player player
            |> add_game_state_msg msg
          in
          (game_states, last_time)
        else
          let rand = Random.int length in
          let player = 
            try
              set_usable_item_bag rand false game_states.player_state 
            with _ -> game_states.player_state
          in
          let msg = "You are stuck in glue" in
          let game_states = 
            game_states
            |> set_game_state_player player
            |> add_game_state_msg msg
          in
          (game_states, last_time)
      | Grimer_Switch, false ->
        (* FIXME *)
        let msg = "You are stuck in grimer" in
        let game_states = 
          game_states
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Imprison_Switch, false ->
        let player = set_entity_current_hp (game_states.player_state.current_hp - 1) game_states.player_state in
        let msg = "You are imprisoned" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Mud_Switch, false ->
        let random_x = Random.int 2 in
        let player = 
          match random_x with
          | 0 -> 
            set_entity_current_hp (game_states.player_state.current_hp - 1) game_states.player_state
          | _ -> 
            try 
              set_i_competence_puissance 0 ((List.nth game_states.player_state.competence 0).puissance - 1) game_states.player_state
            with _ -> game_states.player_state
        in
        let msg = "You are stuck in mud" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Poison_Sting_Switch, false ->
        let player = set_entity_current_hp (game_states.player_state.current_hp - 1) game_states.player_state in
        let msg = "You are poisoned" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Pokemon_Switch, false ->
        (* FIXME *)
        let msg = "You are transformed" in
        let game_states = 
          game_states
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Self_Destruct_Switch, false ->
        let player = set_entity_current_hp (game_states.player_state.current_hp - 20) game_states.player_state in
        let map = set_map_exploded pos_x pos_y game_states.map_state in
        let msg = "You are exploded" in
        let game_states = 
          game_states
          |> set_game_state_map map
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Skill_Drop_Switch, false ->
        let player = 
          try 
            set_i_competence_puissance 0 0 game_states.player_state
          with _ -> game_states.player_state
        in
        let msg = "You are confused" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Slowpoke_Switch, false ->
        let player = set_entity_speed 0.5 game_states.player_state in
        let msg = "You are slowed down" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Spin_Swith, false ->
        let player = set_entity_step_cpt 3 game_states.player_state in
        let msg = "You are dizzy" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
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
        let enemys, player = add_random_enemy game_states.player_state game_states.map_state game_states.enemies_state in
        let msg = "You are summoned" in
        let game_states = 
          game_states
          |> set_game_state_enemy enemys
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | Warp_Trap, false ->
        let rec find_random_position () =
          let random_x = float_of_int (Random.int game_states.map_state.width) in
          let random_y = float_of_int (Random.int game_states.map_state.height) in
          if not (is_wall (int_of_float random_x) (int_of_float random_y) game_states.map_state) && not (List.exists (fun e -> let (e_pos_x, e_pos_y) = get_entity_position e in e_pos_x = random_x && e_pos_y = random_y) game_states.enemies_state) then
            (random_x, random_y)
          else
            find_random_position ()
        in
        let (new_x, new_y) = find_random_position () in
        let player =
          game_states.player_state
          |> set_entity_position new_x new_y
          |> set_entity_target new_x new_y
        in
        let msg = "You are teleported" in
        let game_states = 
          game_states
          |> set_game_state_player player
          |> add_game_state_msg msg
        in
        (game_states, last_time)
      | _ ->
        (game_states, last_time)
  end else
    (game_states, last_time)
