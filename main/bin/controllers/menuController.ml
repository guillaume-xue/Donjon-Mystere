open Views.MenuView
open Models.Generation_map
open Utils.Types
open Utils.Settings_map
open Utils.Json_conversions
open MapController
open Raylib

let list_of_maps = ref [] (* List of maps name *)
let index_select_x = ref 0 (* Variable of cursor x *)
let index_select_y = ref 0 (* Variable of cursor y *)
let last_key_press_time = ref 0.0  (* Variable to store the time of the last key press *)
let map_name = ref "map " (* new map name *)

(**
  [init_menu_controller ()] initializes the menu controller.
*)
let init_menu_controller () =
  list_of_maps := read_json_files_in_directory map_dir;
  init_menu !list_of_maps

(**
  [get_map_selected ()] returns the currently selected map.
  @return The selected map.
*)
let get_map_selected () =
  List.nth !list_of_maps !index_select_y

(**
  [check_intro_screen_click ()] checks if the screen is clicked.
  @return [Select] if the screen is clicked within the window bounds, [Intro] otherwise.
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
    [check_select_screen_selected screenState] checks if player selected an option.
    @param screenState The current screen state.
    @return The new screen state based on the player's selection.
*)
let check_select_screen_selected (screenState : screenState) =
  let current_time = get_time () in
  if current_time -. !last_key_press_time > 0.1 then begin
    last_key_press_time := current_time;
    match is_key_down Key.Enter, is_key_down Key.Down, is_key_down Key.Up, is_key_down Key.Left, is_key_down Key.Right with
    | true, _, _, _, _ -> (* Enter *)
      if !index_select_x = 0 && !index_select_y = 0 then begin (* New game *)
        index_select_x := 3;
        Select_New
      end else if !index_select_x = 1 then begin (* Continue *)
        init_map_controller screen_width screen_height (get_map_selected ());
        Game
      end else
        Select_Other
    | _, true, _, _, _ -> (* Down *)
      if (!index_select_y = 1 && !index_select_x = 0) || 
         (!index_select_y = (List.length !list_of_maps) - 1 && !index_select_x = 1) then
        screenState
      else begin
        index_select_y := !index_select_y + 1;
        Select_Other
      end
    | _, _, true, _, _ -> (* Up *)
      if !index_select_x = 0 && !index_select_y = 0 then begin
        Select
      end else if !index_select_x = 0 && !index_select_y = 1 then begin
        index_select_y := !index_select_y - 1;
        Select
      end else if !index_select_x = 1 && !index_select_y = 0 then
        Select_Other
      else begin
        index_select_y := !index_select_y - 1;
        screenState
      end
    | _, _, _, true, _ -> (* Left *)
      if !index_select_x = 0 then
        screenState
      else begin
        index_select_x := 0;
        index_select_y := 0;
        Select
      end

    | _, _, _, _, true -> (* Right *)
      if List.length !list_of_maps = 0 then
        Select_Other
      else if !index_select_x = 0 && !index_select_y = 0 then
        Select
      else begin
        index_select_x := 1;
        index_select_y := 0;
        Select_Other
      end
    | _ -> screenState
  end else
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

(**
  [check_new_map_name ()] checks and updates the new map name based on key presses.
  @return The new screen state based on the player's input.
*)
let check_new_map_name () =
  if is_any_key_pressed () then begin
    if !map_name = "map " then
      map_name := "";
    if String.length !map_name < 16 then begin (* Maximum length of map name *)
      let char = Char.escaped (key_pressed_to_char (get_key_pressed ())) in
      if char <> " " then
        map_name := !map_name ^ char;
    end;
    Select_New
  end else if is_key_pressed Key.Backspace then begin (* Backspace *)
    if String.length !map_name > 0 then (* Minimum length of map name *)
      map_name := String.sub !map_name 0 (String.length !map_name - 1);
    Select_New
  end else if is_key_pressed Key.Enter then begin (* Enter *)
    if not (List.exists (fun map -> map = !map_name) !list_of_maps) && (* new map name not exist *)
            !map_name <> "map " then begin
      generation_map !map_name;
      init_map_controller screen_width screen_height !map_name;
      Game
    end else
      Select_New
  end else
    Select_New

(**
  [update_intro ()] updates the intro screen.
*)
let update_intro () =
  draw_intro ()

(**
  [get_text_talk ()] gets the text to display.
  @return The text to display.
*)
let get_text_talk () =
  if !index_select_x = 0 && !index_select_y = 0 then (* New game *)
    if List.length !list_of_maps = 6 then (* Maximum number of maps *)
      "Il n'y a plus de place pour une nouvelle partie."
    else if List.exists (fun map -> map = !map_name) !list_of_maps then
      "La carte existe deja."
    else
      "Voulez-vous creer une nouvelle partie ?"
  else if !index_select_x = 1 then
    "Voulez-vous continuer la partie en cours ?"
  else if !index_select_x = 3 then
    "Entrez le nom de la nouvelle carte."
  else
    "Choisissez une carte."

(**
  [update_arrow ()] updates the arrow position.
*)
let update_arrow () =
  set_arrow_x !index_select_x;
  set_arrow_y !index_select_y

(**
  [update_select ()] updates the select screen.
*)
let update_select () =
  draw_select (get_text_talk ());
  update_arrow ()

(**
  [update_select_new ()] updates the new map selection screen.
*)
let update_select_new () =
  draw_select_new !map_name (get_text_talk ())

(**
  [update_select_other ()] updates the other selection screen.
*)
let update_select_other () =
  draw_select_other (get_text_talk ());
  update_arrow ()

