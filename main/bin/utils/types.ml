(** 
  Type [direction] représentant une direction.

  @param Up Direction vers le haut.
  @param Down Direction vers le bas.
  @param Left Direction vers la gauche.
  @param Right Direction vers la droite.
  @param DiagonalUpLeft Direction diagonale vers le haut à gauche.
  @param DiagonalUpRight Direction diagonale vers le haut à droite.
  @param DiagonalDownLeft Direction diagonale vers le bas à gauche.
  @param DiagonalDownRight Direction diagonale vers le bas à droite.
*)
type direction = 
  | Up
  | Down
  | Left
  | Right
  | DiagonalUpLeft
  | DiagonalUpRight
  | DiagonalDownLeft
  | DiagonalDownRight

(**
  Type [playerState] représentant l'état d'un joueur.

  @param Idle Joueur immobile.
  @param Moving Joueur en mouvement.
*)
type playerState =
  | Idle
  | Moving

(** 
  Type [screenState] représentant l'état de l'écran.

  @param Intro Écran d'introduction.
  @param Select Écran de sélection.
  @param NewGame Écran de nouvelle partie.
  @param LoadGame Écran de chargement de partie.
  @param Game Écran de jeu.
*)
type screenState = 
  | Intro 
  | Select 
  | NewGame 
  | LoadGame 
  | Game

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
  @param target_x Coordonnée x cible du joueur.
  @param target_y Coordonnée y cible du joueur.
  @param moving Indique si le joueur est en mouvement.
  @param state État du joueur.
  @param direction Direction du joueur.
*)

type player = {
  pos_x: float;
  pos_y: float;
  screen_x: int;
  screen_y: int;
  player_textures_id: int;
  target_x: float;
  target_y: float;
  moving: bool;
  state: playerState;
  direction: direction;
}