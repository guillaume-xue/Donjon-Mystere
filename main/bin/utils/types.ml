(** 
  Type [tile] représentant une tuile dans une carte cellulaire.

  @param x Coordonnée x de la tuile.
  @param y Coordonnée y de la tuile.
  @param texture_id Identifiant de la texture associée à la tuile.
*)
type tile = {
  x: int;
  y: int;
  texture_id: int;
}

(** 
  Type [map] représentant une carte.

  @param width La largeur de la carte.
  @param height La hauteur de la carte.
  @param tiles La liste des tuiles de la carte.
*)
type map = {
  width: int;
  height: int;
  tiles: tile list;
}


(** 
  Type [player] représentant un joueur.

  @param pos_x Coordonnée x du joueur.
  @param pos_y Coordonnée y du joueur.
  @param screen_x Coordonnée x de l'écran.
  @param screen_y Coordonnée y de l'écran.
  @param player_textures_id Identifiant de la texture associée au joueur.
*)
type player = {
  pos_x: int;
  pos_y: int;
  screen_x: int;
  screen_y: int;
  player_textures_id: int;
}