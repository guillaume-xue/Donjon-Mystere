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