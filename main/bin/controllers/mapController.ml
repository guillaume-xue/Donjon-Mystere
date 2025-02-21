open Raylib
open Models.PlayerModel
open Views.PlayerView
open Views.MapView
open Utils.Types
open Utils.Funcs

(* Map *)
let my_map = ref { width = 0; height = 0; tiles = [] }

(* Player *)
let player = ref { pos_x = 0; pos_y = 0; screen_x = 0; screen_y = 0; player_textures_id = 0 }

(* Last key press time *)
let last_key_press_time = ref 0.0

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
let draw_game ()=
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map !my_map !player;
  draw_player !player;
  end_drawing ()

(**
  [check_key_pressed ()] checks if a key is pressed.
*)
let check_key_pressed () =
  let current_time = get_time () in
  if current_time -. !last_key_press_time >= 0.2 then begin
    if is_key_down Key.Right then begin
      player := move_player !player 1 0;
      last_key_press_time := current_time
    end else if is_key_down Key.Left then begin
      player := move_player !player (-1) 0;
      last_key_press_time := current_time
    end else if is_key_down Key.Up then begin
      player := move_player !player 0 (-1);
      last_key_press_time := current_time
    end else if is_key_down Key.Down then begin
      player := move_player !player 0 1;
      last_key_press_time := current_time
    end
  end

(**
  [update_game ()] updates the game.
*)
let update_game ()=
  check_key_pressed ();
  draw_game ()
