open Raylib
open Utils.Settings_map
open Utils.Funcs

(**
  [init_menu_textures ()] initialise les textures du menu.
  @return Les textures du menu.
*)
let init_menu_textures () : Texture2D.t option * Texture2D.t option * Texture2D.t option * Texture2D.t option * Texture2D.t option * Texture2D.t option * Texture2D.t option * Texture2D.t list * int * int * int * int =
  (* Charger et redimensionner l'image du titre *)
  let title = load_image "resources/images/menu/title.png" in
  let image_width = Image.width title in
  let image_height = Image.height title in
  let new_width = image_width * 2 in
  let new_height = image_height * 2 in
  image_resize (addr title) new_width new_height;
  let title_texture = Some (load_texture_from_image title) in
  unload_image title;
  let title_pos_x = (screen_width - new_width) / 2 in
  let title_pos_y = (screen_height - new_height) / 5 in

  (* Charger une image de fond aléatoire *)
  Random.self_init ();
  let nb_bg = Random.int 7 + 1 in
  let file = "resources/images/menu/image-menu-" ^ string_of_int nb_bg ^ ".png" in
  let background = load_image file in
  image_resize (addr background) screen_width screen_height;
  let background_texture = Some (load_texture_from_image background) in
  unload_image background;

  let text_pos_x = screen_width / 2 - 80 in
  let text_pos_y = screen_height - 150 in

  (* Charger select texture *)
  let select = load_image "resources/images/menu/select.png" in
  image_resize (addr select) screen_width screen_height;
  let select_texture = Some (load_texture_from_image select) in
  unload_image select;

  (* Charger select_other texture *)
  let select_other = load_image "resources/images/menu/select_other.png" in
  image_resize (addr select_other) screen_width screen_height;
  let select_other_texture = Some (load_texture_from_image select_other) in
  unload_image select_other;

  (* Charger select_new texture *)
  let select_new = load_image "resources/images/menu/select_new.png" in
  image_resize (addr select_new) screen_width screen_height;
  let select_new_texture = Some (load_texture_from_image select_new) in
  unload_image select_new;

  (* Charger arrow texture *)
  let arrow = load_image "resources/images/menu/arrow.png" in
  image_resize (addr arrow) 20 20;
  let arrow_texture = Some (load_texture_from_image arrow) in
  unload_image arrow;

  let choose_pokemon = load_image "resources/images/menu/choose_pokemon.png" in
  image_resize (addr choose_pokemon) screen_width screen_height;
  let choose_pokemon_texture = Some (load_texture_from_image choose_pokemon) in
  unload_image choose_pokemon;

  let pokemon_icon = load_image "resources/images/menu/pokemon.png" in
  let pokemon_icon_texture = init_textures_size 0 3 pokemon_icon [] 126 |> List.rev in
  

  (title_texture, background_texture, select_texture, select_other_texture, select_new_texture, arrow_texture, choose_pokemon_texture, pokemon_icon_texture, title_pos_x, title_pos_y, text_pos_x, text_pos_y)

(**
  [draw_intro title_texture background_texture title_pos_x title_pos_y text_pos_x text_pos_y] dessine l'écran d'introduction.
  @param title_texture La texture du titre.
  @param background_texture La texture de l'arrière-plan.
  @param title_pos_x La position x du titre.
  @param title_pos_y La position y du titre.
  @param text_pos_x La position x du texte.
  @param text_pos_y La position y du texte.
*)
let draw_intro (title_texture : Texture2D.t option) (background_texture : Texture2D.t option) (title_pos_x : int) (title_pos_y : int) (text_pos_x : int) (text_pos_y : int) : unit =
  let time = get_time () in
  let blink = int_of_float (time *. 2.0) mod 3 = 0 in

  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le titre *)
  (match title_texture with
  | Some texture -> draw_texture texture title_pos_x title_pos_y Color.white
  | None -> ());

  if blink then
    draw_text_bold "Appuie pour jouer" text_pos_x text_pos_y 21 Color.yellow;
  end_drawing ()

(**
  [draw_select text_talk background_texture select_texture arrow_texture arrow_pos_x arrow_pos_y] dessine le menu de sélection.
  @param text_talk Le texte à afficher.
  @param background_texture La texture de l'arrière-plan.
  @param select_texture La texture de la sélection.
  @param arrow_texture La texture de la flèche.
  @param arrow_pos_x La position x de la flèche.
  @param arrow_pos_y La position y de la flèche.
*)
let draw_select (text_talk : string) (background_texture : Texture2D.t option) (select_texture : Texture2D.t option) (arrow_texture : Texture2D.t option) (arrow_pos_x : int) (arrow_pos_y : int) : unit =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in
  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le fond *)
  (match select_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());
  
  (* Dessiner une fleche *)
  if blink then 
    (match arrow_texture with
    | Some texture -> draw_texture texture arrow_pos_x arrow_pos_y Color.white
    | None -> ());
  (* Dessiner texte *)
  draw_text "Nouvelle Partie" 100 60 20 Color.white;
  draw_text "Autre" 100 90 20 Color.white;
  draw_text text_talk 100 450 20 Color.white;
  end_drawing ()

(**
  [draw_select_other text_talk background_texture select_other_texture list_of_maps arrow_texture arrow_pos_x arrow_pos_y] dessine le menu de sélection d'autre.
  @param text_talk Le texte à afficher.
  @param background_texture La texture de l'arrière-plan.
  @param select_other_texture La texture de la sélection.
  @param list_of_maps La liste des cartes.
  @param arrow_texture La texture de la flèche.
  @param arrow_pos_x La position x de la flèche.
  @param arrow_pos_y La position y de la flèche.
*)
let draw_select_other (text_talk : string) (background_texture : Texture2D.t option) (select_other_texture : Texture2D.t option) (list_of_maps : string list) (arrow_texture : Texture2D.t option) (arrow_pos_x : int) (arrow_pos_y : int) : unit =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in
  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le fond *)
  (match select_other_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());
  
  (* Dessiner une fleche *)
  if blink then 
    (match arrow_texture with
    | Some texture -> draw_texture texture arrow_pos_x arrow_pos_y Color.white
    | None -> ());

  (* Dessiner texte *)
  draw_text "Nouvelle Partie" 100 60 20 Color.white;
  draw_text "Autre" 100 90 20 Color.white;
  draw_text text_talk 100 450 20 Color.white;

  (* Dessiner la liste des cartes *)
  if List.length list_of_maps > 0 then
    List.iteri (fun i map ->
      draw_text map 450 (60 + i * 30) 20 Color.white
    ) list_of_maps
  else
    draw_text "Aucune carte" 450 60 20 Color.white;

  end_drawing ()

(**
  [draw_select_new map_name text_talk background_texture select_new_texture arrow_texture] dessine le menu de sélection de nouvelle carte.
  @param map_name Le nom de la carte.
  @param text_talk Le texte à afficher.
  @param background_texture La texture de l'arrière-plan.
  @param select_new_texture La texture de la sélection.
  @param arrow_texture La texture de la flèche.
*)
let draw_select_new (map_name : string) (text_talk : string) (background_texture : Texture2D.t option) (select_new_texture : Texture2D.t option) (arrow_texture : Texture2D.t option) : unit =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in
  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner le fond *)
  (match select_new_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  (* Dessiner une fleche *)
  if blink then 
    (match arrow_texture with
    | Some texture -> draw_texture texture 240 210 Color.white
    | None -> ());
  
  (* Dessiner texte *)
  draw_text map_name 270 210 20 Color.white;
  draw_text text_talk 100 450 20 Color.white;
  draw_text "[Entrer]" 640 510 20 Color.white;
  end_drawing ()

(**
  [draw_pokemon_chooser background_texture choose_pokemon_texture arrow_texture pokemon_icon_texture arrow_pos_y y] dessine le menu de sélection de Pokémon.
  @param background_texture La texture de l'arrière-plan.
  @param choose_pokemon_texture La texture de la sélection.
  @param arrow_texture La texture de la flèche.
  @param pokemon_icon_texture La liste des textures des icônes de Pokémon.
  @param arrow_pos_y La position y de la flèche.
  @param y La position y du Pokémon sélectionné.
*)
let draw_pokemon_chooser (background_texture : Texture2D.t option) (choose_pokemon_texture : Texture2D.t option) (arrow_texture : Texture2D.t option) (pokemon_icon_texture : Texture2D.t list) (arrow_pos_y : int) (y : int) : unit =
  let time = get_time () in
  let blink = int_of_float (time *. 3.0) mod 2 = 0 in

  begin_drawing ();
  clear_background Color.raywhite;
  (* Dessiner le fond *)
  (match background_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());

  let pokemon_icon = List.nth pokemon_icon_texture y in
  draw_texture pokemon_icon 555 65 Color.white;

  (* Dessiner le fond *)
  (match choose_pokemon_texture with
  | Some texture -> draw_texture texture 0 0 Color.white
  | None -> ());
  
  (* Dessiner une fleche *)
  if blink then 
    (* Dessiner une fleche *)
    (match arrow_texture with
    | Some texture -> draw_texture texture 70 arrow_pos_y  Color.white
    | None -> ());

  (* Dessiner texte *)
  draw_text "Choisir un Pokemon" 100 450 20 Color.white;
  draw_text "[Entrer]" 640 510 20 Color.white;
  draw_text "Germignon" 100 60 20 Color.white;
  draw_text "Héricendre" 100 90 20 Color.white;
  draw_text "Kaiminus" 100 120 20 Color.white;
  
  end_drawing ()

(**
  [set_arrow_y y] récupère la position y de la flèche.
  @return La position y de la flèche.
*)
let set_arrow_y (y : int) : int =
  58 + y * 30

(**
  [set_arrow_x x] récupère la position x de la flèche.
  @return La position x de la flèche.
*)
let set_arrow_x (x : int) : int =
  70 + x * 350

(**
  [draw_die_start ()] dessine l'écran de fin de jeu.
*)
let draw_die_end () : unit =
  draw_text ("Tu es mort, retour au menu") (screen_width / 2 - 125) (screen_height / 2 - 50) 20 Color.white;