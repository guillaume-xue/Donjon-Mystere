open Raylib
open Ctypes
open MenuController
open View.MenuView

let run () =
  let screen_width = 800 in
  let screen_height = 600 in

  init_window screen_width screen_height "Mystery Dungeon";

  (* Charger une image et la redimensionner *)
  let image = load_image "resources/menu/title.png" in
  let image_width = Image.width image in
  let image_height = Image.height image in
  let new_width = image_width * 2 in
  let new_height = image_height * 2 in
  image_resize (addr image) new_width new_height;

  let texture = load_texture_from_image image in
  unload_image image; (* On peut décharger l'image car elle est maintenant en texture *)

  (* Générer un nombre aléatoire entre 1 et 7 *)
  Random.self_init ();
  let rand = Random.int 7 + 1 in

  let file = "resources/menu/image-menu-" ^ string_of_int rand ^ ".png" in

  let background = load_image file in
  image_resize (addr background) screen_width screen_height;
  let background_texture = load_texture_from_image background in
  unload_image background;

  set_target_fps 60;

  (* Boucle principale *)
  let rec main_loop () =
    if window_should_close () then
      close_window ()
    else
      begin
        begin_drawing ();
        clear_background Color.raywhite;

        (* Afficher l'image redimensionnée *)
        draw_menu texture background_texture screen_height screen_width new_height new_width;

        (* Vérifier si un bouton est cliqué *)
        check_button_click ();

        end_drawing ();
        main_loop ()
      end
  in
  main_loop ()