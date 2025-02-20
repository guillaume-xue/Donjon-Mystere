open Raylib
open Views.MapView
open Views.MenuView
open Views.PlayerView
open Models.GenerationMapCellulaire
open Models.PlayerModel
open Utils.Funcs

(**
  [check_screen_click ()] checks if the screen is clicked.
  @return [true] if the screen is clicked, [false] otherwise.
*)
let check_screen_click () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_x = get_mouse_x () in
    let mouse_y = get_mouse_y () in
    if mouse_x >= 0 && mouse_x <= 800 then (* Largeur de la fenêtre *)
      if mouse_y >= 0 && mouse_y <= 600 then (* Hauteur de la fenêtre *)
        true
      else
        false
    else
      false
  else
    false

(**
  [run ()] runs the game.
*)
let run () =
  let screen_width = 800 in
  let screen_height = 600 in

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 60;

  generation_Map_Cellulaire ();
  (* Initialisation *)
  init_menu screen_width screen_height;
  init_player ();
  set_player_screen (screen_width/2 - 12) (screen_height/2 - 12);
  init_map ();

  let is_click = ref false in
  let my_map = ref (load_map_from_json "resources/map/map.json") in

  let draw ()=
    begin_drawing ();
    clear_background Color.raywhite;
    draw_map !my_map !player;
    draw_player !player;
    end_drawing ()
  in

  let update ()=
    if is_key_down Key.Right then
      move_player (-1) 0
    else if is_key_down Key.Left then
      move_player 1 0
    else if is_key_down Key.Up then
      move_player 0 1
    else if is_key_down Key.Down then
      move_player 0 (-1)
  in

  (* Boucle principale *)
  let rec main_loop () =
    if window_should_close () then
      close_window ()
    else
      if !is_click then
        begin
          draw ();
          update ();
          main_loop ()
        end
      else
        begin
          draw_menu ();
          is_click := check_screen_click ();
          main_loop ()
        end
  in
  main_loop ()