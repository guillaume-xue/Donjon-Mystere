open Raylib
open Models.PlayerModel
open Views.PlayerView
open Views.MapView
open Utils.Types
open Utils.Funcs

(* Map *)
let my_map = ref { width = 0; height = 0; tiles = []; regions = [] }

(* Player *)
let player = ref { pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; player_textures_id = 0; target_x = 0.0; target_y = 0.0; moving = false; state = Idle; direction = Down; current_hp = 0; max_hp = 0; level = 0; current_xp = 0; max_xp = 0 }

(* Last player position update time *)
let last_update_time = ref 0.0

(* Last texture update time *)
let last_texture_update_time = ref 0.0

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
  player := set_player_texture_id !player 24;
  my_map := load_map_from_json "resources/map/map.json"

(**
  [draw_game ()] draws the game.
*)
let draw_game () =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map !my_map !player;
  draw_player !player;
  draw_player_stats !player;
  end_drawing ()

(**
  [increment_texture_id ()] increments the texture id.
*)
let increment_texture_id () =
  if get_time () -. !last_texture_update_time >= 0.2 then begin
    player := update_player_texture_id !player;
    last_texture_update_time := get_time ()
  end

(**
  [is_wall ()] checks if the player is facing a wall.
  @return True if the player is facing a wall, false otherwise.
*)
let is_wall () =
  let tiles = !my_map.tiles in
  let target_x = int_of_float !player.target_x in
  let target_y = int_of_float !player.target_y in
  List.exists (fun tile -> tile.x = target_x && tile.y = target_y && tile.texture_id = 1) tiles

(**
  [new_player_pos ()] updates the player's position gradually towards the target.
*)
let new_player_pos () =
  let current_time = get_time () in
  if not (is_wall ()) then begin
    player := set_player_moving !player false;
    player := set_player_state !player Idle;
    player := set_target !player !player.pos_x !player.pos_y
  end;
  if current_time -. !last_update_time >= 0.01 then begin
    let dx = !player.target_x -. !player.pos_x in
    let dy = !player.target_y -. !player.pos_y in
    let step = 0.1 in
    let new_x = if abs_float dx < step then !player.target_x else !player.pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then !player.target_y else !player.pos_y +. (if dy > 0.0 then step else -.step) in
    if !player.pos_x = !player.target_x && !player.pos_y = !player.target_y then
      player := set_player_moving !player false;
    if not !player.moving then begin
      let new_x = if new_x <> floor new_x then floor new_x +. 1.0 else new_x in
      let new_y = if new_y <> floor new_y then floor new_y +. 1.0 else new_y in
      player := set_player_pos !player new_x new_y;
      player := set_player_state !player Idle;
    end else begin
      player := set_player_pos !player new_x new_y;
      player := set_player_state !player Moving;
    end;
    last_update_time := current_time;
  end

(**
  [check_key_pressed ()] checks if a key is pressed.
*)
let check_key_pressed () =
  if not(!player.moving) then begin
    if is_key_down Key.Right then begin
      player := set_target !player (!player.pos_x +. 1.0) !player.pos_y;
      player := set_player_direction !player Right;
      player := set_player_moving !player true
    end else if is_key_down Key.Left then begin
      player := set_target !player (!player.pos_x -. 1.0) !player.pos_y;
      player := set_player_direction !player Left;
      player := set_player_moving !player true
    end else if is_key_down Key.Up then begin
      player := set_target !player !player.pos_x (!player.pos_y -. 1.0);
      player := set_player_direction !player Up;
      player := set_player_moving !player true
    end else if is_key_down Key.Down then begin
      player := set_target !player !player.pos_x (!player.pos_y +. 1.0);
      player := set_player_direction !player Down;
      player := set_player_moving !player true
    end;
  end
    
(**
  [update_game ()] updates the game.
*)
let update_game () =
  draw_game ();
  check_key_pressed ();
  new_player_pos ();
  increment_texture_id ();
  player := is_end_moving !player

(**
  [get_player ()] returns the player.
  @return The player.
*)
let get_player () = !player