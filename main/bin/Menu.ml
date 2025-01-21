open Graphics

(* Définition des couleurs *)
let couleur_fond = rgb 200 200 200
let couleur_texte = rgb 0 0 0
let couleur_bouton = rgb 150 150 150
let couleur_survol = rgb 180 180 180

(* Structure d'un bouton *)
type bouton = {
  x: int;
  y: int;
  largeur: int;
  hauteur: int;
  texte: string;
}

(* Dessine un bouton *)
let dessiner_bouton bouton couleur =
  set_color couleur;
  fill_rect bouton.x bouton.y bouton.largeur bouton.hauteur;
  set_color couleur_texte;
  moveto (bouton.x + bouton.largeur/2 - (String.length bouton.texte * 4))
         (bouton.y + bouton.hauteur/2);
  draw_string bouton.texte

(* Vérifie si la souris est sur un bouton *)
let est_dans_bouton bouton x y =
  x >= bouton.x && x <= bouton.x + bouton.largeur &&
  y >= bouton.y && y <= bouton.y + bouton.hauteur

(* Fonction principale du menu *)
let menu () =
  open_graph " 800x600";
  set_window_title "Menu Principal";
  
  let bouton_jouer = {
    x = 300; y = 400;
    largeur = 200; hauteur = 50;
    texte = "Jouer"
  } in
  
  let bouton_options = {
    x = 300; y = 300;
    largeur = 200; hauteur = 50;
    texte = "Options"
  } in
  
  let bouton_quitter = {
    x = 300; y = 200;
    largeur = 200; hauteur = 50;
    texte = "Quitter"
  } in
  
  let boucle_menu () =
    try
      while true do
        (* Dessiner le fond *)
        set_color couleur_fond;
        fill_rect 0 0 800 600;
        
        (* Dessiner les boutons *)
        List.iter (fun b -> dessiner_bouton b couleur_bouton)
          [bouton_jouer; bouton_options; bouton_quitter];
        
        (* Gestion des événements *)
        let e = wait_next_event [Button_down; Mouse_motion] in
        if e.button then
          if est_dans_bouton bouton_quitter e.mouse_x e.mouse_y then
            raise Exit
          else if est_dans_bouton bouton_jouer e.mouse_x e.mouse_y then
            print_endline "Jouer sélectionné"
          else if est_dans_bouton bouton_options e.mouse_x e.mouse_y then
            print_endline "Options sélectionné"
      done
    with Exit -> close_graph ()
  in
  boucle_menu ()