open Raylib
open Models.PlayerModel
open Views.PlayerView
open Views.MapView
open Utils.Types
open Utils.Funcs

(* Map *)
let my_map = ref { width = 0; height = 0; tiles = [] }

(* Player *)
let player = ref { pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; player_textures_id = 0; target_x = 0.0; target_y = 0.0; moving = false }

(* Last player position update time *)
let last_update_time = ref 0.0

(**
  [init_map_controller screen_width screen_height] initializes the map controller.
  @param screen_width The width of the screen.
  @param screen_height The height of the screen.
*)
let init_map_controller screen_width screen_height=
  init_map ();
  init_player ();
  player := (load_player_from_json "resources/map/player.json");
  player := set_player_screen !player (screen_width / 2) (screen_height / 2);
  my_map := load_map_from_json "resources/map/map.json"

(**
  [draw_game ()] draws the game.
*)
let draw_game () =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map !my_map !player;
  draw_player !player;
  end_drawing ()

(**
  [new_player_pos ()] updates the player's position gradually towards the target.
*)
let new_player_pos () =
  let current_time = get_time () in
  if current_time -. !last_update_time >= 0.1 then begin (* Update every 0.1 seconds *)
    let dx = !player.target_x -. !player.pos_x in (* Distance to target x *)
    let dy = !player.target_y -. !player.pos_y in (* Distance to target y *)
    let step = 0.1 in
    let new_x = if abs_float dx < step then !player.target_x else !player.pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then !player.target_y else !player.pos_y +. (if dy > 0.0 then step else -.step) in
    if not(!player.moving) then begin 
      let new_x = if new_x <> floor new_x then floor new_x +. 1.0 else new_x in
      let new_y = if new_y <> floor new_y then floor new_y +. 1.0 else new_y in
      player := set_player_pos !player new_x new_y;
      last_update_time := current_time
    end else begin
      player := set_player_pos !player new_x new_y;
      last_update_time := current_time
    end
  end

(**
  [check_key_pressed ()] checks if a key is pressed.
*)
let check_key_pressed () =
  if not(!player.moving) then begin
    if is_key_down Key.Right then begin
      player := set_target !player (!player.pos_x +. 1.0) !player.pos_y;
      player := set_player_moving !player true
    end else if is_key_down Key.Left then begin
      player := set_target !player (!player.pos_x -. 1.0) !player.pos_y;
      player := set_player_moving !player true
    end else if is_key_down Key.Up then begin
      player := set_target !player !player.pos_x (!player.pos_y -. 1.0);
      player := set_player_moving !player true
    end else if is_key_down Key.Down then begin
      player := set_target !player !player.pos_x (!player.pos_y +. 1.0);
      player := set_player_moving !player true
    end
  end
    
(**
  [update_game ()] updates the game.
*)
let update_game () =
  draw_game ();
  check_key_pressed ();
  new_player_pos ();
  player := is_end_movin !player

(**
  [get_player ()] returns the player.
  @return The player.
*)
let get_player () = !player