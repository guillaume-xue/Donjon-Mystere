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
  @param Attack Attaque.
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
  | No_move

type trap_and_ground_type =
  | Stairs_Up             (* proceed to the next up floor of a dungeon *)
  | Stairs_Down           (* proceed to the next down floor of a dungeon *)
  | Bug_Switch            (* Induces sleep on Pokémon that steps on it*)
  | Chestnut_Switch       (* Causes spiky chestnuts to fall, inflicts 10 fix damage on the Pokémon that steps on the trap *)
  | Drop_Hole             (* Pokémon that steps on trap falls to the next floor, other team members will faint even with the Resurrect Seed *)
  | Explosion_Switch      (* An Electrode explodes to inflict damage on all Pokémon in the immediately surrounding 20 squares. Break walls, and wipe out items *)
  | Fan_Switch            (* Sends Pokémon blasting against wall in any direction, causes damage unless holding Passthrough Scarf or is a ghost Pokémon *)
  | Glue_Switch           (* Randomly disables one of the held items or item in bag until you leave the dungeon *)
  | Grimer_Switch         (* Turns one food item in your bag into a Grimer Food *)
  | Imprison_Switch       (* Disables one attack until changes floors *)
  | Mud_Switch            (* Randomly decreases one stat by 1 *)
  | Poison_Sting_Switch   (* Induces poison on Pokémon that steps on it *)
  | Pokemon_Switch        (* Changes the wild Pokémon or items in the same room *)
  | Self_Destruct_Switch  (* A Voltorb self-destructs to inflict damage on all Pokémon in the immediately surrounding 8 squares, also breaking some walls and wiping up items *)
  | Skill_Drop_Switch     (* Reduces one random attack's PP to 0 *)
  | Slowpoke_Switch       (* Slows walking pace of Pokémon that steps on it *)
  | Spin_Swith            (* Induces confusion on Pokémon that steps on it *)
  | Summon_Switch         (* Causes several wild Pokémon to appear. Trap disappears afterwards *)
  | Warp_Trap             (* Warps to another part of the floor *)

(** 
  Type [trap_and_ground] représentant un piège ou un sol.

  @param nature Type du piège.
  @param tag_pos_x Position x de la tuile.
  @param tag_pos_y Position y de la tuile.
  @param visibility Visibilité du piège.
 *)
type trap_and_ground = {
  nature: trap_and_ground_type;
  tag_pos_x: int;
  tag_pos_y: int;
  visibility : bool;
}

(**
  Type [interaction] représentant une interaction.

  @param Nothing Aucune interaction.
  @param Attack Attaque.
  @param PickUp Ramasser un objet.
  @param OpenBag Ouvrir le sac.
*)
type interaction =
  | Nothing
  | Attack
  | PickUp
  | OpenBag

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
  @param ChoosePokemon Écran de choix de pokemon.
  @param Game Écran de jeu.
*)
type screenState = 
  | Intro 
  | Select
  | Select_New
  | Select_Other 
  | NewGame 
  | ChoosePokemon
  | Game

(**
  Type [bag_textures] représentant les textures du sac à dos.
  @param bag_background_tex Texture de fond du sac.
  @param square_tex Texture du carré.
  @param items_tex Liste des textures des objets.
*)
type bag_textures = {
  bag_background_tex: Raylib.Texture.t option;
  square_tex: Raylib.Texture.t option;
  items_tex: Raylib.Texture.t list;
}

(**
  Type [game_textures] représentant les textures du jeu.
  @param tiles_tex Liste des textures des tuiles.
  @param entities_tex Liste des textures des entités.
  @param items_tex Liste des textures des objets.
  @param bag_tex Textures du sac à dos.
  @param shadow_cast_tex Liste des textures de l'ombre projetée.
  @param traps_and_grounds_tex Liste des textures des pièges et du sol.
  @param attack_msg_tex Texture du message
*)
type game_textures = {
  tiles_tex: Raylib.Texture.t list;
  entities_tex: Raylib.Texture.t list;
  items_tex: Raylib.Texture.t list;
  bag_tex: bag_textures;
  shadow_cast_tex: Raylib.Texture.t list;
  traps_and_grounds_tex: Raylib.Texture.t list;
  attack_msg_tex: Raylib.Texture.t option;
}

(** 
  Type [tile] représente une tuile de la carte.

  @param x Coordonnée x de la tuile.
  @param y Coordonnée y de la tuile.
  @param texture_id Identifiant de la texture associée à la tuile.
  @param biome_id Identifiant du biome associé à la tuile.
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
  @param floor L'étage actuel de la carte.
*)
type map = {
  width: int;
  height: int;
  tiles: tile list;
  regions: zone list;
  floor: int;
  music: string option;
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
  Type [loot] représentant un loot.

  @param item_id Identifiant de l'objet.
  @param item_skin_id Identifiant de la texture associée à l'objet.
  @param quantity Quantité de l'objet.
  @param pos_x Position x du sur l'écran.
  @param pos_y Position y du sur l'écran.
  @param screen_x Position x du loot sur l'écran.
  @param screen_y Position y du loot sur l'écran.
  @param description Description de l'objet.
  @param usable Booléen indiquant si l'objet est utilisable.
*)
type loot = {
  item_id: int;
  item_skin_id: int;
  quantity: int;
  pos_x: float;
  pos_y: float;
  screen_x: int;
  screen_y: int;
  description: string;
  usable: bool;
}

(** 
  Type [element] représentant un élément.
*)
type element = 
  | Feu
  | Eau
  | Plante
  | Normal
  | Electrique
  | Psy
  | Tenebre
  | Glace

(**
  Type [attaqueType] représentant le type d'attaque
*)
type attaqueType =
  | Attaque
  | AttaqueSpeciale
  | Passive

(**
  Type [competence] représentant une compétence.

  @param id Identifiant de la compétence.
  @param name Nom de la compétence.
  @param description Description de la compétence.
  @param element Élément de la compétence.
  @param puissance Puissance de la compétence.
  @param precision Précision de la compétence.
  @param attaqueType Type d'attaque de la compétence.
*)
type competence = {
  id: int;
  name: string;
  description: string;
  element: element;
  puissance: int;
  precision: int;
  attaqueType: attaqueType;
}

(**
  Type [bag] représentant un sac à dos.

  @param items Liste des objets dans le sac.
  @param max_size Taille maximum du sac.
*)
type bag = {
  items: loot list;
  max_size: int;
  selected_item: int;
}

(**
  Type [position] représentant la position d'un pokemon.

  @param world_x Coordonnée x dans le monde.
  @param world_y Coordonnée y dans le monde.
  @param screen_x Coordonnée x sur l'écran.
  @param screen_y Coordonnée y sur l'écran.
  @param target_x Coordonnée x cible.
  @param target_y Coordonnée y cible.
*)
type position = {
  world_x : float;
  world_y : float;
  screen_x : int;
  screen_y : int;
  target_x : float;
  target_y : float;
}

(** 
  Type [pokemon] représentant un pokemon.

  @param nom Nom du pokemon.
  @param id Identifiant du pokemon.
  @param last_id Dernier identifiant du pokemon.
  @param number Numéro du pokemon (texture).
  @param position Position du pokemon.
  @param entity_textures_id Identifiant de la texture associée au pokemon.
  @param moving Booléen indiquant si le pokemon est en mouvement.
  @param state État du pokemon.
  @param direction Direction du pokemon.
  @param current_hp Points de vie actuels du pokemon.
  @param max_hp Points de vie maximum du pokemon.
  @param level Niveau du pokemon.
  @param current_xp Points d'expérience actuels du pokemon.
  @param max_xp Points d'expérience maximum du pokemon.
  @param action Action actuelle du pokemon.
  @param bag Sac à dos du pokemon.
  @param step_cpt Compteur de pas du pokemon.
  @param speed Vitesse du pokemon.
  @param attaque Attaque du pokemon.
  @param defense Défense du pokemon.
  @param attaque_speciale Attaque spéciale du pokemon.
  @param defense_speciale Défense spéciale du pokemon.
  @param element Élément du pokemon.
  @param competence Liste des compétences du pokemon.
  @param path Chemin du pokemon.
  @param your_turn Booléen indiquant si c'est le tour du pokemon.
*)
type pokemon = {
  nom: string;
  id: int;
  last_id: int;
  number: int;
  position: position;
  entity_textures_id: int;
  moving: bool;
  state: entityState;
  direction: direction;
  current_hp: int;
  max_hp: int;
  level: int;
  current_xp: int;
  max_xp: int;
  action: interaction;
  bag: bag;
  step_cpt : int;
  speed : float;
  attaque : int;
  defense : int;
  attaque_speciale : int;
  defense_speciale : int;
  element : element;
  competence : competence list;
  path : (int * int) list;
  your_turn : bool;
  money: int;
}

(** 
  Type [entity] représente une entité.

  @param Player Joueur.
  @param Enemy Ennemi.
*)
type entity = 
  | Player of pokemon
  | Enemy of pokemon

type game_state = {
  map_state: map;
  player_state: pokemon;
  enemies_state: pokemon list;
  loots_state: loot list;
  traps_and_grounds_state: trap_and_ground list;
  msgs_state: string list;
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

(** [print_direction dir] affiche une direction sous forme de chaîne de caractères. *)
let print_direction dir =
  let direction_to_string = function
    | Up -> "Up"
    | Down -> "Down"
    | Left -> "Left"
    | Right -> "Right"
    | DiagonalUpLeft -> "DiagonalUpLeft"
    | DiagonalUpRight -> "DiagonalUpRight"
    | DiagonalDownLeft -> "DiagonalDownLeft"
    | DiagonalDownRight -> "DiagonalDownRight"
    | No_move -> "No_move"
  in
  print_endline (direction_to_string dir)
