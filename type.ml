(* Type pour représenter le levier et les portes *)
type cross = 
  | Simple_or_Double of bool
  | Lever of int
  | Door_1 of int
  | Door_2  of int

(* Type pour représenter l'eau, la glace et le temps de transformation *)
type water_ice =
  | Water of bool
  | Ice of bool
  | Delay of int

(* Type pour représenter les cases du donjon *)
type cell =
  | Wall                   (* Mur, infranchissable *)
  | Floor                  (* Sol, franchissable *)
  | Water_Ice of water_ice (* Eau pouvant se geler pour débloquer des passages *)
  | Exit                   (* Sortie, permet d'aller au niveau suivant ou de s'arrêter*)
  | Door of cross          (* Porte, permet de relier deux salles *)
  | Monster of int         (* Monstre avec un identifiant pour le lier à une entité *)
  | Treasure of int        (* Trésor avec un identifiant pour l'inventaire *)
  | Player                 (* Joueur *)

(* Type pour représenter une carte 2D *)
type map = cell array array  (* Matrice 2D de cases *)

(* Type pour les statistiques des entités *)
type stats = {
  hp : int;                 (* Points de vie *)
  max_hp : int;             (* Points de vie maximum *)
  attack : int;             (* Points d'attaque *)
  defense : int;            (* Points de défense *)
  speed : int;              (* Vitesse pour déterminer les actions *)
}

(* Type pour représenter un monstre *)
type monster = {
  id : int;                 (* Identifiant unique *)
  name : string;            (* Nom du monstre *)
  position : int * int;     (* Position (x, y) sur la carte *)
  stats : stats;            (* Statistiques du monstre *)
  behavior : string;        (* Comportement, ex. "aggressif", "fuite" *)
}

(* Type pour représenter les objets *)
type item =
  | Weapon of { name : string; damage : int }  (* Arme avec nom et dégâts *)
  | Armor of { name : string; defense : int }  (* Armure avec nom et défense *)
  | Potion of { name : string; effect : string; potency : int }  (* Potion *)

(* Type pour représenter le joueur *)
type player = {
  name : string;            (* Nom du joueur *)
  position : int * int;     (* Position (x, y) sur la carte *)
  stats : stats;            (* Statistiques du joueur *)
  inventory : item list;    (* Inventaire contenant des objets *)
}

(* Type pour représenter l’état du jeu *)
type game_state = {
  dungeon : map;            (* Carte du donjon *)
  player : player;          (* Joueur principal *)
  monsters : monster list;  (* Liste des monstres présents *)
  level : int;              (* Niveau actuel du donjon *)
  turn : int;               (* Numéro du tour actuel *)
}

(* Type pour l’état général de la partie *)
type game_status =
  | InProgress of game_state  (* Partie en cours *)
  | GameOver                  (* Partie terminée *)
  | Victory                   (* Victoire atteinte *)
