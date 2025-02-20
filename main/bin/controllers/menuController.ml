open Views.MenuView
open Utils.Types
open Raylib

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

let check_select_screen_click () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_x = get_mouse_x () in
    let mouse_y = get_mouse_y () in
    if mouse_x >= 0 && mouse_x <= 800 then (* Largeur de la fenêtre *)
      if mouse_y >= 0 && mouse_y <= 600 then (* Hauteur de la fenêtre *)
        Game
      else
        Select
    else
      Select
  else
    Select

let update_intro () =
  draw_intro ()

let update_select () =
  draw_select ()
