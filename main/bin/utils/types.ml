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
  Type [entityState] représentant l'état d'un joueur.

  @param Idle Joueur immobile.
  @param Moving Joueur en mouvement.
*)
type entityState =
  | Idle
  | Moving

(** 
  Type [screenState] représentant l'état de l'écran.

  @param Intro Écran d'introduction.
  @param Select Écran de sélection.
  @param Select_New Écran de sélection de nouvelle partie.
  @param Select_Other Écran de sélection d'autre partie.
  @param NewGame Écran de nouvelle partie.
  @param LoadGame Écran de chargement de partie.
  @param Game Écran de jeu.
*)
type screenState = 
  | Intro 
  | Select
  | Select_New
  | Select_Other 
  | NewGame 
  | LoadGame 
  | Game

(** 
  Type [tile] représente une tuile de la carte.

  @param x Coordonnée x de la tuile.
  @param y Coordonnée y de la tuile.
  @param texture_id Identifiant de la texture associée à la tuile.
*)
type tile = {
  x: int;
  y: int;
  texture_id: int;
  biome_id: int;
}

(** 
  Type [zone] représente une un groupe de tuiles connectées.
  (la map est chargée avec des zones distinctes au premier passage)

  @param id Identifiant unique de la zone.
  @param size Taille de la zone (nombre de tuiles).
  @param tiles Liste des tuiles appartenant à la zone.
*)
type zone = {
  id: int;
  size: int;
  tiles: tile list;
}

(** 
  Type [map] représente une carte.

  @param width La largeur de la carte.
  @param height La hauteur de la carte.
  @param tiles La liste des tuiles de la carte.
  @param regions La liste des zones distinctes de la carte.
*)
type map = {
  width: int;
  height: int;
  tiles: tile list;
  regions: zone list;
}

(** 
  Type [arete] représentant une arête entre deux zones.

  @param zone1 La première zone.
  @param zone2 La deuxième zone.
  @param coord1 Les coordonnées du bord le plus proche de la première zone.
  @param coord2 Les coordonnées du bord le plus proche de la deuxième zone.
  @param distance La distance entre les deux zones.
*)
type arete = {
  zone1: zone;
  zone2: zone;
  coord1: (int * int);
  coord2: (int * int);
  distance: float;
}

(** 
  Type [pokemon] représentant un pokemon.
  @param pos_x Position x du pokemon.
  @param pos_y Position y du pokemon.
  @param screen_x Position x du pokemon sur l'écran.
  @param screen_y Position y du pokemon sur l'écran.
  @param entity_textures_id Identifiant de la texture associée au pokemon.
  @param target_x Position x de la cible du pokemon.
  @param target_y Position y de la cible du pokemon.
  @param moving Booléen indiquant si le pokemon est en mouvement.
  @param state État du pokemon.
  @param direction Direction du pokemon.
  @param current_hp Points de vie actuels du pokemon.
  @param max_hp Points de vie maximum du pokemon.
  @param level Niveau du pokemon.
  @param current_xp Points d'expérience actuels du pokemon.
  @param max_xp Points d'expérience maximum du pokemon.
*)
type pokemon = {
  pos_x: float;
  pos_y: float;
  screen_x: int;
  screen_y: int;
  entity_textures_id: int;
  target_x: float;
  target_y: float;
  moving: bool;
  state: entityState;
  direction: direction;
  current_hp: int;
  max_hp: int;
  level: int;
  current_xp: int;
  max_xp: int;
}

(** 
  Type [entity] représente une entité.

  @param Player Joueur.
  @param Enemy Ennemi.
*)
type entity = 
  | Player of pokemon
  | Enemy of pokemon

type loot = {
  item_id: int;
  item_skin_id: int;
  quantity: int;
  pos_x: float;
  pos_y: float;
  screen_x: int;
  screen_y: int;
  description: string;
}


(** 
  Module implémentant une file de priorité simple en utilisant une liste de paires (valeur, priorité). 
*)
module PriorityQueue = struct
  (** Le type d'une file de priorité, où 'a est le type des valeurs stockées dans la file. *)
  type 'a t = ('a * float) list ref

  (** [create] crée une nouvelle file de priorité vide. *)
  let create () = ref []

  (** [add] ajoute une valeur avec la priorité donnée à la file de priorité [pq]. 
      La file est maintenue triée par priorité, avec la plus petite priorité en premier. *)
  let add pq priority value =
    pq := List.merge (fun (_, p1) (_, p2) -> compare p1 p2) !pq [(value, priority)]

  (** [pop] enlève et retourne la valeur avec la plus petite priorité de la file de priorité [pq].
      Lève [Failure "PriorityQueue is empty"] si la file est vide. *)
  let pop pq =
    match !pq with
    | [] -> failwith "PriorityQueue is empty"
    | (value, priority) :: rest ->
      pq := rest;
      (priority, value)

  (** [is_empty] retourne [true] si la file de priorité [pq] est vide, et [false] sinon. *)
  let is_empty pq = !pq = []
end

