open Raylib
open View.MapView
open View.MenuView
open Util.Types
open Yojson.Basic
open Yojson.Basic.Util

(* Fonction pour charger une map depuis un fichier json *)
let load_map_from_json (filename: string): map =
  let json = from_file filename in
  let width = json |> member "width" |> to_int in
  let height = json |> member "height" |> to_int in
  let tiles = json |> member "tiles" |> to_list |> List.map (fun tile ->
    {
      x = tile |> member "x" |> to_int;
      y = tile |> member "y" |> to_int;
      texture_id = tile |> member "texture_id" |> to_int;
      biome_id = tile |> member "biome_id" |> to_int;
    }
  ) in
  { width; height; tiles; regions = [] }


(* Fonction pour vérifier si l'écran a été cliqué *)
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

(* Fonction principale *)
let run () =
  let screen_width = 800 in
  let screen_height = 600 in

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 60;

  (* Initialisation *)
  init_map ();
  init_menu screen_width screen_height;

  let is_click = ref false in
  let my_map = ref (load_map_from_json "resources/map/map.json") in

  (* Boucle principale *)
  let rec main_loop () =
    if window_should_close () then
      close_window ()
    else
      if !is_click then
        begin
          draw_map !my_map;
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