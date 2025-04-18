open Types

(* TODO: Ajouter un menu option pour une meilleur interaction *)
let map_size_x = 50 (* Taille du terrain X *)
let map_size_y = 50 (* Taille du terrain Y *)
let density = 0.45 (* Proportion initiale de cellules vivantes *)
let iterations = 5 (* Nombre d'itérations de l'automate cellulaire *)
let map_min_size = 30 (* Taille minimale pour garder une zone *)
let proba_route_bonus = 0.2 (* Probabilité d'ajouter une route supplémentaire *)
let screen_width = 800 (* Largeur de la fenêtre *)
let screen_height = 600 (* Hauteur de la fenêtre *)
let map_dir = "resources/json/maps/" (* Dossier de sauvegarde des maps *)
let player_texture_size = 21.0 (* Taille de textures pour le joueur *)
let tile_texture_size = 24.0 (* Taille de textures pour les tiles *)
let loot_texture_size = 16.0 (* Taille de textures pour les loots *)
let map_marge = 10 (* Marge de la carte *)

(* pourcentage des traps_grounds generation *)
let traps = [
  (Bug_Switch, 6.94);
  (Chestnut_Switch, 5.78);
  (Drop_Hole, 3.59);
  (Explosion_Switch, 2.31);
  (Fan_Switch, 2.89);
  (Glue_Switch, 2.89);
  (Grimer_Switch, 5.78);
  (Imprison_Switch, 2.89);
  (Mud_Switch, 2.89);
  (Poison_Sting_Switch, 5.78);
  (Pokemon_Switch, 5.78);
  (Self_Destruct_Switch, 1.74);
  (Skill_Drop_Switch, 5.78);
  (Slowpoke_Switch, 5.78);
  (Spin_Swith, 5.20);
  (Summon_Switch, 5.78);
  (Warp_Trap, 5.78);
]