open Raylib
open Utils.Types
open Utils.Settings_map
open MenuController
open MapController

(**
  [run ()] runs the game.
*)
let run () =

  init_window screen_width screen_height "Mystery Dungeon";
  set_target_fps 60;

  (* Initialisation *)
  init_menu_controller ();

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
        screen_state := check_select_screen_selected !screen_state;
        update_select ()
      end
    | Select_New ->
      begin
        screen_state := check_new_map_name ();
        update_select_new ();
      end
    | Select_Other ->
      begin
        screen_state := check_select_screen_selected !screen_state;
        update_select_other ();
      end
    | Game ->
      begin
        update_game ()
      end
    | _ -> ()
  in

  (* Boucle principale *)
  let rec main_loop () =
    if window_should_close () then
      begin
        save (get_map_selected ());
        close_window ()
      end
    else
      begin
        check_screen_state ();
        main_loop ()
      end
  in
  main_loop ()