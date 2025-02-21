open Raylib
open Utils.Types
open MenuController
open MapController

(**
  [run ()] runs the game.
*)
let run () =
  let screen_width = 800 in
  let screen_height = 600 in

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 60;

  (* Initialisation *)
  init_map_controller screen_width screen_height;
  init_menu_controller screen_width screen_height;

  (* État de l'écran *)
  let screen_state = ref Intro in

  (* Verification de l'etat de l'ecran *)
  let check_screen_state () =
    match !screen_state with
    | Intro ->
      begin
        screen_state := check_intro_screen_click ();
        update_intro ()
      end
    | Select ->
      begin
        screen_state := check_select_screen_select ();
        update_select ()
      end
    | Game ->
      begin
        update_game ();
        draw_game ()
      end
    | _ -> ()
  in

  (* Boucle principale *)
  let rec main_loop () =
    if window_should_close () then
      close_window ()
    else
      begin
        check_screen_state ();
        main_loop ()
      end
  in
  main_loop ()