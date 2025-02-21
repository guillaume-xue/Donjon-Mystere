open Views.MenuView
open Models.GenerationMapCellulaire
open Utils.Types
open Raylib

(**
  [init_menu_controller screen_width screen_height] initializes the menu controller.
  @param screen_width The width of the screen.
  @param screen_height The height of the screen.
*)
let init_menu_controller screen_width screen_height =
  init_menu screen_width screen_height

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
    [check_select_screen_select ()] checks if player selected an option.
    @return [Game] if player selected an option, [Select].
*)
let check_select_screen_select () =
  if is_key_down Key.Enter then
    if is_arrow_up() then (
      generation_Map_Cellulaire();
      Game
    ) else
      Game
  else if is_key_down Key.Down then (
    set_arrow_down ();
    set_text_select_down ();
    Select
  ) else if is_key_down Key.Up then (
    set_arrow_up ();
    set_text_select_up ();
    Select
  ) else
    Select

(**
  [update_intro ()] updates the intro screen.
*)
let update_intro () =
  draw_intro ()

(**
  [update_select ()] updates the select screen.
*)
let update_select () =
  draw_select ()
