open Raylib
open Models.PlayerModel
open Views.PlayerView
open Views.MapView
open Utils.Types
open Utils.Funcs

let my_map = ref { width = 0; height = 0; tiles = [] }
let player = ref { pos_x = 0; pos_y = 0; screen_x = 0; screen_y = 0; player_textures_id = 0 }

let init_map_controller screen_width screen_height=
  init_map ();
  init_player ();
  player := set_player_screen !player (screen_width / 2) (screen_height / 2);
  my_map := load_map_from_json "resources/map/map.json"

let draw_game ()=
  begin_drawing ();
  clear_background Color.raywhite;
  draw_map !my_map !player;
  draw_player !player;
  end_drawing ()

let check_key_press ()=
  if is_key_down Key.Right then
    player := move_player !player (-1) 0
  else if is_key_down Key.Left then
    player := move_player !player 1 0
  else if is_key_down Key.Up then
    player := move_player !player 0 1
  else if is_key_down Key.Down then
    player := move_player !player 0 (-1)

let update_game ()=
  check_key_press ();
  draw_game ()
