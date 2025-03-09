open Views.MenuView
open Models.Generation_map
open Utils.Types
open Utils.Settings_map
open Utils.Json_conversions
open MapController
open Raylib

let list_of_maps = ref []

let index_select_x = ref 0
let index_select_y = ref 0
let last_key_press_time = ref 0.0  (* Ajout de la variable pour stocker le temps du dernier clic *)
let map_name = ref "map"

(**
  [init_menu_controller screen_width screen_height] initializes the menu controller.
  @param screen_width The width of the screen.
  @param screen_height The height of the screen.
*)
let init_menu_controller () =
  list_of_maps := read_json_files_in_directory map_dir;
  init_menu !list_of_maps

let get_map_selected () =
  List.nth !list_of_maps !index_select_y

(**
  [check_screen_click ()] checks if the screen is clicked.
  @return [true] if the screen is clicked, [false] otherwise.
*)
let check_intro_screen_click () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_x = get_mouse_x () in
    let mouse_y = get_mouse_y () in
    if mouse_x >= 0 && mouse_x <= 800 then (* Largeur de la fenêtre *)
      if mouse_y >= 0 && mouse_y <= 600 then (* Hauteur de la fenêtre *)
        Select
      else
        Intro
    else
      Intro
  else
    Intro

(**
    [check_select_screen_selected ()] checks if player selected an option.
    @return [Game] if player selected an option, [Select].
*)
let check_select_screen_selected (screenState : screenState) =
  let current_time = get_time () in
  if current_time -. !last_key_press_time > 0.1 then (  (* Vérifie si 0.2 secondes se sont écoulées *)
    last_key_press_time := current_time;
    if is_key_down Key.Enter then
      if !index_select_x = 0 && !index_select_y = 0 then (
        Select_New
      ) else (
        init_map_controller screen_width screen_height (get_map_selected ());
        Game
      )
    else if is_key_down Key.Down then (
      if (!index_select_y = 1 && !index_select_x = 0) || 
         (!index_select_y = (List.length !list_of_maps) - 1 && !index_select_x = 1) then
        screenState
      else (
        index_select_y := !index_select_y + 1;
        Select_Other
        )
    ) else if is_key_down Key.Up then (
      if !index_select_x = 0 && !index_select_y = 0 then
        Select
      else (
        if !index_select_y = 0 then
          screenState
        else(
          index_select_y := !index_select_y - 1;
          Select_Other
          )
        )
    ) else if is_key_down Key.Left then (
      if !index_select_x = 0 then
        screenState
      else (
        index_select_x := 0;
        index_select_y := 1;
        Select_Other
        )
    ) else if is_key_down Key.Right then (
      if !index_select_x = 0 && !index_select_y = 0 then
        Select
      else(
        index_select_x := 1;
        index_select_y := 0;
        Select_Other
        )
    ) else
      screenState
  ) else
    screenState


(**
  [is_any_key_pressed ()] checks if any key is pressed.
  @return True if any key is pressed, false otherwise.
*)
let is_any_key_pressed () =
  List.exists is_key_down [Key.A; Key.B; Key.C; Key.D; Key.E; Key.F; Key.G; Key.H; Key.I; Key.J; Key.K; Key.L; Key.M; Key.N; Key.O; Key.P; Key.Q; Key.R; Key.S; Key.T; Key.U; Key.V; Key.W; Key.X; Key.Y; Key.Z]

(**
  [key_pressed_to_char key] converts a key to a character.
  @param key The key to convert.
  @return The character corresponding to the key.
*)
let key_pressed_to_char key =
  match key with
  | Key.A -> 'a'
  | Key.B -> 'b'
  | Key.C -> 'c'
  | Key.D -> 'd'
  | Key.E -> 'e'
  | Key.F -> 'f'
  | Key.G -> 'g'
  | Key.H -> 'h'
  | Key.I -> 'i'
  | Key.J -> 'j'
  | Key.K -> 'k'
  | Key.L -> 'l'
  | Key.M -> 'm'
  | Key.N -> 'n'
  | Key.O -> 'o'
  | Key.P -> 'p'
  | Key.Q -> 'q'
  | Key.R -> 'r'
  | Key.S -> 's'
  | Key.T -> 't'
  | Key.U -> 'u'
  | Key.V -> 'v'
  | Key.W -> 'w'
  | Key.X -> 'x'
  | Key.Y -> 'y'
  | Key.Z -> 'z'
  | _ -> ' '

let check_new_map_name () =
  if is_any_key_pressed () then (
    if !map_name = "map" then
      map_name := "";
    if String.length !map_name < 16 then (
      let char = Char.escaped (key_pressed_to_char (get_key_pressed ())) in
      if char <> " " then
        map_name := !map_name ^ char;
    );
    Select_New
  ) else if is_key_pressed Key.Backspace then (
    if String.length !map_name > 0 then
      map_name := String.sub !map_name 0 (String.length !map_name - 1);
    Select_New
  ) else if is_key_pressed Key.Enter then begin
    generation_map !map_name;
    init_map_controller screen_width screen_height !map_name;
    Game
  end else
    Select_New

(**
  [update_intro ()] updates the intro screen.
*)
let update_intro () =
  draw_intro ()

(**
  [update_select ()] updates the select screen.
*)
let update_select () =
  draw_select ();
  set_arrow_x !index_select_x;
  set_arrow_y !index_select_y

let update_select_new () =
  draw_select_new !map_name

let update_select_other () =
  draw_select_other ();
  set_arrow_x !index_select_x;
  set_arrow_y !index_select_y