open Raylib
open Models.PlayerModel
open Views.PlayerView
open Views.MapView
open Utils.Types
open Utils.Json_conversions
open Utils.Settings_map


let my_map = ref { width = 0; height = 0; tiles = []; regions = [] } (* Map *)
let my_player = ref { pos_x = 0.0; pos_y = 0.0; screen_x = 0; screen_y = 0; 
                      player_textures_id = 0; target_x = 0.0; target_y = 0.0; 
                      moving = false; state = Idle; direction = Down; current_hp = 0; 
                      max_hp = 0; level = 0; current_xp = 0; max_xp = 0 } (* Player *)
let last_update_time = ref 0.0 (* Last Player position update time *)
let last_texture_update_time = ref 0.0 (* Last texture update time *)

(**
  [init_map_controller screen_width screen_height filename] initializes the map controller.
  @param screen_width The width of the screen.
  @param screen_height The height of the screen.
  @param filename The name of the map file.
*)
let init_map_controller screen_width screen_height filename =
  init_map ();
  init_player ();
  let (map, player) = load_map_player_from_json (map_dir ^ filename ^ ".json") in
  my_map := map;
  my_player := player;
  my_player := set_player_screen !my_player (screen_width / 2) (screen_height / 2);
  my_player := set_player_texture_id !my_player 24

(**
  [draw_game ()] draws the game.
*)
let draw_game () =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map !my_map !my_player;
  draw_player !my_player;
  draw_player_stats !my_player;
  end_drawing ()

(**
  [increment_texture_id ()] increments the texture id.
*)
let increment_texture_id () =
  if get_time () -. !last_texture_update_time >= 0.2 then begin
    my_player := update_player_texture_id !my_player;
    last_texture_update_time := get_time ()
  end

(**
  [is_wall ()] checks if the player is facing a wall.
  @return True if the player is facing a wall, false otherwise.
*)
let is_wall () =
  let tiles = !my_map.tiles in
  let target_x = int_of_float !my_player.target_x in
  let target_y = int_of_float !my_player.target_y in
  List.exists (fun tile -> tile.x = target_x && tile.y = target_y && tile.texture_id = 1) tiles

(**
  [new_player_pos ()] updates the player's position gradually towards the target.
*)
let new_player_pos () =
  let current_time = get_time () in
  if not (is_wall ()) then begin
    my_player := set_player_moving !my_player false;
    my_player := set_player_state !my_player Idle;
    my_player := set_target !my_player !my_player.pos_x !my_player.pos_y
  end;
  if current_time -. !last_update_time >= 0.01 then begin
    let dx = !my_player.target_x -. !my_player.pos_x in
    let dy = !my_player.target_y -. !my_player.pos_y in
    let step = 0.1 in
    let new_x = if abs_float dx < step then !my_player.target_x else !my_player.pos_x +. (if dx > 0.0 then step else -.step) in
    let new_y = if abs_float dy < step then !my_player.target_y else !my_player.pos_y +. (if dy > 0.0 then step else -.step) in
    if !my_player.pos_x = !my_player.target_x && !my_player.pos_y = !my_player.target_y then
      my_player := set_player_moving !my_player false;
    if not !my_player.moving then begin
      let new_x = if new_x <> floor new_x then floor new_x +. 1.0 else new_x in
      let new_y = if new_y <> floor new_y then floor new_y +. 1.0 else new_y in
      my_player := set_player_pos !my_player new_x new_y;
      my_player := set_player_state !my_player Idle;
    end else begin
      my_player := set_player_pos !my_player new_x new_y;
      my_player := set_player_state !my_player Moving;
    end;
    last_update_time := current_time;
  end

(**
  [check_key_pressed ()] checks if a key is pressed.
*)
let check_key_pressed () =
  if not(!my_player.moving) then begin
    if is_key_down Key.Right then begin
      my_player := set_target !my_player (!my_player.pos_x +. 1.0) !my_player.pos_y;
      my_player := set_player_direction !my_player Right;
      my_player := set_player_moving !my_player true
    end else if is_key_down Key.Left then begin
      my_player := set_target !my_player (!my_player.pos_x -. 1.0) !my_player.pos_y;
      my_player := set_player_direction !my_player Left;
      my_player := set_player_moving !my_player true
    end else if is_key_down Key.Up then begin
      my_player := set_target !my_player !my_player.pos_x (!my_player.pos_y -. 1.0);
      my_player := set_player_direction !my_player Up;
      my_player := set_player_moving !my_player true
    end else if is_key_down Key.Down then begin
      my_player := set_target !my_player !my_player.pos_x (!my_player.pos_y +. 1.0);
      my_player := set_player_direction !my_player Down;
      my_player := set_player_moving !my_player true
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
  my_player := is_end_moving !my_player


(**
  [save filename] saves the map to a file.
  @param filename The name of the file.
*)
let save filename =
  let json = map_player_to_json !my_map !my_player in
  write_json_to_file (map_dir ^ filename ^ ".json") json