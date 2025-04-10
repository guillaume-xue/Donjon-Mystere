open Views.MenuView
open Models.Generation_map
open Utils.Types
open Utils.Funcs
open Raylib

(**
  [init_menu_controller ()] initializes the menu controller.
  @return The menu controller stats.
*)
let init_menu_controller () =
  let index_select_x = 0 in (* Variable of cursor x *)
  let index_select_y = 0 in (* Variable of cursor y *)
  let arrow_pos_x = 70 in
  let arrow_pos_y = 58 in
  let menu_item_info = (index_select_x, index_select_y, arrow_pos_x, arrow_pos_y) in
  let menu_stats = (init_menu_textures ()) in
  (menu_item_info, menu_stats)

(**
  [get_map_selected list_of_maps index_select_y] gets the selected map.
  @param list_of_maps The list of maps.
  @param index_select_y The index of the selected map.
  @return The selected map.
*)
let get_map_selected list_of_maps index_select_y =
  List.nth list_of_maps index_select_y

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
    [check_select_screen_selected screenState list_of_maps index_select_x index_select_y last_key_press_time map_name] checks and updates the selected screen based on key presses.
    @param screenState The current screen state.
    @param list_of_maps The list of maps.
    @param index_select_x The index of the selected x.
    @param index_select_y The index of the selected y.
    @param last_key_press_time The last time a key was pressed.
    @param map_name The name of the map.
    @return The new screen state based on the player's input.
*)
let check_select_screen_selected screenState list_of_maps index_select_x index_select_y last_key_press_time map_name =
  let current_time = get_time () in
  if current_time -. last_key_press_time > 0.1 then begin
    match is_key_down Key.Enter, is_key_down Key.Down, is_key_down Key.Up, is_key_down Key.Left, is_key_down Key.Right with
    | true, _, _, _, _ -> (* Enter *)
      if index_select_x = 0 && index_select_y = 0 then begin (* New game *)
        (Select_New, 3, index_select_y, current_time, map_name)
      end else if index_select_x = 1 then begin (* Continue *)
        (Game, index_select_x, index_select_y, current_time, get_map_selected list_of_maps index_select_y)
      end else
        (Select_Other, index_select_x, index_select_y, current_time, map_name)
    | _, true, _, _, _ -> (* Down *)
      if (index_select_y = 1 && index_select_x = 0) || 
         (index_select_y = (List.length list_of_maps) - 1 && index_select_x = 1) then
        (screenState, index_select_x, index_select_y, current_time, map_name)
      else begin
        (Select_Other, index_select_x, index_select_y + 1, current_time, map_name)
      end
    | _, _, true, _, _ -> (* Up *)
      if index_select_x = 0 && index_select_y = 0 then begin
        (Select, index_select_x, index_select_y, current_time, map_name)
      end else if index_select_x = 0 && index_select_y = 1 then begin
        (Select, index_select_x, index_select_y - 1, current_time, map_name)
      end else if index_select_x = 1 && index_select_y = 0 then
        (Select_Other, index_select_x, index_select_y, current_time, map_name)
      else begin
        (screenState, index_select_x, index_select_y - 1, current_time, map_name)
      end
    | _, _, _, true, _ -> (* Left *)
      if index_select_x = 0 then
        (Select, index_select_x, index_select_y, current_time, map_name)
      else begin
        (Select, 0, 0, current_time, map_name)
      end
    | _, _, _, _, true -> (* Right *)
      if List.length list_of_maps = 0 then
        (Select_Other, index_select_x, index_select_y, current_time, map_name)
      else if index_select_x = 0 && index_select_y = 0 then
        (Select, index_select_x, index_select_y, current_time, map_name)
      else begin
        (Select_Other, 1, 0, current_time, map_name)
      end
    | _ -> (screenState, index_select_x, index_select_y, current_time, map_name)
  end else
    (screenState, index_select_x, index_select_y, last_key_press_time, map_name)

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
  [check_new_map_name map_name list_of_maps] checks the new map name.
  @param map_name The name of the map.
  @param list_of_maps The list of maps.
  @return The new screen state and the new map name.
*)
let check_new_map_name (map_name: string) list_of_maps =
  if is_any_key_pressed () then
    if map_name = "map " then (Select_New, "")
    else if String.length map_name < 16 then begin (* Maximum length of map name *)
      let char = Char.escaped (key_pressed_to_char (get_key_pressed ())) in
      if char <> " " then (Select_New, map_name ^ char)
      else (Select_New, map_name)
    end else
      (Select_New, map_name)
  else if is_key_pressed Key.Backspace then begin (* Backspace *)
    if String.length map_name > 0 then (Select_New, String.sub map_name 0 (String.length map_name - 1))
    else (Select_New, map_name)
  end else if is_key_pressed Key.Enter then begin (* Enter *)
    if not (List.exists (fun map -> map = map_name) list_of_maps) && map_name <> "map " then begin
      create_map_json map_name;
      (Game, map_name)
    end else
      (Select_New, map_name)
  end else
    (Select_New, map_name)

(**
  [update_intro title_texture background_texture title_pos_x title_pos_y text_pos_x text_pos_y] updates the intro screen.
  @param title_texture The title texture.
  @param background_texture The background texture.
  @param title_pos_x The title position x.
  @param title_pos_y The title position y.
  @param text_pos_x The text position x.
  @param text_pos_y The text position y.
*)
let update_intro title_texture background_texture title_pos_x title_pos_y text_pos_x text_pos_y =
  draw_intro title_texture background_texture title_pos_x title_pos_y text_pos_x text_pos_y

(**
  [get_text_talk map_name list_of_maps index_select_x index_select_y] gets the text to display.
  @param map_name The name of the map.
  @param list_of_maps The list of maps.
  @param index_select_x The index of the selected x.
  @param index_select_y The index of the selected y.
  @return The text to display.
*)
let get_text_talk map_name list_of_maps index_select_x index_select_y =
  if index_select_x = 0 && index_select_y = 0 then (* New game *)
    if List.length list_of_maps = 6 then (* Maximum number of maps *)
      "Il n'y a plus de place pour une nouvelle partie."
    else
      "Voulez-vous creer une nouvelle partie ?"
  else if index_select_x = 1 then
    "Voulez-vous continuer la partie en cours ?"
  else if index_select_x = 3 then
    if List.exists (fun map -> map = map_name) list_of_maps then
      "La carte existe deja."
    else
      "Entrez le nom de la nouvelle carte."
  else
    "Choisissez une carte."

(**
  [update_arrow index_select_x index_select_y] updates the arrow position.
  @param index_select_x The index of the selected x.
  @param index_select_y The index of the selected y.
  @return The updated arrow position.
*)
let update_arrow index_select_x index_select_y =
  let arrow_pos_x = set_arrow_x index_select_x in
  let arrow_pos_y = set_arrow_y index_select_y in
  (arrow_pos_x, arrow_pos_y)

(**
  [update_select map_name list_of_maps index_select_x index_select_y background_texture select_texture arrow_texture arrow_pos_x arrow_pos_y] updates the select screen.
  @param map_name The name of the map.
  @param list_of_maps The list of maps.
  @param index_select_x The index of the selected x.
  @param index_select_y The index of the selected y.
  @param background_texture The background texture.
  @param select_texture The select texture.
  @param arrow_texture The arrow texture.
  @param arrow_pos_x The arrow position x.
  @param arrow_pos_y The arrow position y.
*)
let update_select map_name list_of_maps index_select_x index_select_y background_texture select_texture arrow_texture arrow_pos_x arrow_pos_y =
  draw_select (get_text_talk map_name list_of_maps index_select_x index_select_y) background_texture select_texture arrow_texture arrow_pos_x arrow_pos_y

(**
  [update_select_new map_name list_of_maps index_select_x index_select_y background_texture select_new_texture arrow_texture] updates the new selection screen.
  @param map_name The name of the map.
  @param list_of_maps The list of maps.
  @param index_select_x The index of the selected x.
  @param index_select_y The index of the selected y.
  @param background_texture The background texture.
  @param select_new_texture The select new texture.
  @param arrow_texture The arrow texture.
*)
let update_select_new map_name list_of_maps index_select_x index_select_y background_texture select_new_texture arrow_texture =
  draw_select_new map_name (get_text_talk map_name list_of_maps index_select_x index_select_y) background_texture select_new_texture arrow_texture

(**
  [update_select_other map_name list_of_maps index_select_x index_select_y background_texture select_other_texture arrow_texture arrow_pos_x arrow_pos_y] updates the other selection screen.
  @param map_name The name of the map.
  @param list_of_maps The list of maps.
  @param index_select_x The index of the selected x.
  @param index_select_y The index of the selected y.
  @param background_texture The background texture.
  @param select_other_texture The select other texture.
  @param arrow_texture The arrow texture.
  @param arrow_pos_x The arrow position x.
  @param arrow_pos_y The arrow position y.
*)
let update_select_other map_name list_of_maps index_select_x index_select_y background_texture select_other_texture arrow_texture arrow_pos_x arrow_pos_y =
  draw_select_other (get_text_talk map_name list_of_maps index_select_x index_select_y) background_texture select_other_texture list_of_maps arrow_texture arrow_pos_x arrow_pos_y

(**
  [check_screen_state screen_state map_name menu_item_info menu_stats list_of_maps last_time] checks the screen state.
  @param screen_state The current screen state.
  @param map_name The name of the map.
  @param menu_item_info The menu item info.
  @param menu_stats The menu stats.
  @param list_of_maps The list of maps.
  @param last_time The last time.
  @return The updated screen state, map name, menu item info, and last time.
*)
let check_screen_state screen_state map_name menu_item_info menu_stats list_of_maps last_time =
  let (index_select_x, index_select_y, arrow_pos_x, arrow_pos_y) = menu_item_info in
  let (title_texture, background_texture, select_texture, select_other_texture, select_new_texture, arrow_texture, title_pos_x, title_pos_y, text_pos_x, text_pos_y) = menu_stats in
  match screen_state with
  | Intro ->
    begin
      let state = check_intro_screen_click () in 
      update_intro title_texture background_texture title_pos_x title_pos_y text_pos_x text_pos_y;
      (state, map_name, (index_select_x, index_select_y, arrow_pos_x, arrow_pos_y), last_time)
    end
  | Select ->
    begin
      let (state, x, y, time, new_map_name) = check_select_screen_selected screen_state list_of_maps index_select_x index_select_y (List.nth last_time 0) map_name in
      let last_time = replace_nth last_time 0 time in
      let (arrow_pos_x, arrow_pos_y) = update_arrow x y in
      update_select new_map_name list_of_maps index_select_x index_select_y background_texture select_texture arrow_texture arrow_pos_x arrow_pos_y;
      (state, new_map_name, (x, y, arrow_pos_x, arrow_pos_y), last_time)
    end
  | Select_New ->
    begin
      let (state, new_map_name) = check_new_map_name map_name list_of_maps in
      update_select_new new_map_name list_of_maps index_select_x index_select_y background_texture select_new_texture arrow_texture;
      (state, new_map_name, (index_select_x, index_select_y, arrow_pos_x, arrow_pos_y), last_time)
    end
  | Select_Other ->
    begin
      let (state, x, y, time, new_map_name) = check_select_screen_selected screen_state list_of_maps index_select_x index_select_y (List.nth last_time 0) map_name in
      let last_time = replace_nth last_time 0 time in
      let (arrow_pos_x, arrow_pos_y) = update_arrow x y in
      update_select_other new_map_name list_of_maps index_select_x index_select_y background_texture select_other_texture arrow_texture arrow_pos_x arrow_pos_y;
      (state, new_map_name , (x, y, arrow_pos_x, arrow_pos_y), last_time)
    end
  | Game ->
    begin
      (Game, map_name, (index_select_x, index_select_y, arrow_pos_x, arrow_pos_y), last_time)
    end
  | _ -> (screen_state, map_name, (index_select_x, index_select_y, arrow_pos_x, arrow_pos_y), last_time)
