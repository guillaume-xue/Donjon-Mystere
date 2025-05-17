open Types
open Yojson.Basic
open Yojson.Basic.Util
open Funcs

(**
  [load_map_player_from_json filename] loads a map and a player from a combined JSON file.
  @param filename The name of the file to load.
  @return A tuple containing the map and the player loaded from the file.
*)
let load_map_player_from_json (filename: string): (map * pokemon * pokemon list * loot list * trap_and_ground list) =
  let json = from_file filename in
  let map_json = json |> member "map" in
  let player_json = json |> member "player" in
  let enemy_json = json |> member "enemy" |> to_list in
  let loot_json = json |> member "loot" |> to_list in
  let trap_and_ground_json = json |> member "trap_and_ground" |> to_list in

  let map = {
    width = map_json |> member "width" |> to_int;
    height = map_json |> member "height" |> to_int;
    tiles = map_json |> member "tiles" |> to_list |> List.map (fun tile ->
      {
        x = tile |> member "x" |> to_int;
        y = tile |> member "y" |> to_int;
        texture_id = tile |> member "texture_id" |> to_int;
        biome_id = tile |> member "biome_id" |> to_int;
      }
    );
    regions = [];
    floor = map_json |> member "floor" |> to_int;
    music = 
      match map_json |> member "music" with
      | `String s -> Some s
      | _ -> None;
  } in

  let player = {
    nom = player_json |> member "nom" |> to_string;
    id = player_json |> member "id" |> to_int;
    last_id = player_json |> member "last_id" |> to_int;
    number = player_json |> member "number" |> to_int;
    position = { 
      world_x = player_json |> member "position" |> member "world_x" |> to_float;
      world_y = player_json |> member "position" |> member "world_y" |> to_float;
      screen_x = player_json |> member "position" |> member "screen_x" |> to_int;
      screen_y = player_json |> member "position" |> member "screen_y" |> to_int;
      target_x = player_json |> member "position" |> member "target_x" |> to_float;
      target_y = player_json |> member "position" |> member "target_y" |> to_float;
    };
    entity_textures_id = 0;
    moving = false;
    state = Idle;
    direction = Down;
    current_hp = player_json |> member "current_hp" |> to_int;
    max_hp = player_json |> member "max_hp" |> to_int;
    level = player_json |> member "level" |> to_int;
    current_xp = player_json |> member "current_xp" |> to_int;
    max_xp = player_json |> member "max_xp" |> to_int;
    action = Nothing;
    bag = {
      items = player_json |> member "bag" |> member "items" |> to_list |> List.map (fun item ->
        {
          item_id = item |> member "item_id" |> to_int;
          item_skin_id = item |> member "item_skin_id" |> to_int;
          quantity = item |> member "quantity" |> to_int;
          pos_x = item |> member "pos_x" |> to_float;
          pos_y = item |> member "pos_y" |> to_float;
          screen_x = item |> member "screen_x" |> to_int;
          screen_y = item |> member "screen_y" |> to_int;
          description = item |> member "description" |> to_string;
          usable = item |> member "usable" |> to_bool;
        }
      );
      max_size = player_json |> member "bag" |> member "max_size" |> to_int;
      selected_item = 0;
    };
    step_cpt = player_json |> member "step_cpt" |> to_int;
    speed = player_json |> member "speed" |> to_float;

    attaque = player_json |> member "attaque" |> to_int;
    defense = player_json |> member "defense" |> to_int;
    attaque_speciale = player_json |> member "attaque_speciale" |> to_int;
    defense_speciale = player_json |> member "defense_speciale" |> to_int;
    element = (player_json |> member "element" |> to_string |> function
      | "Feu" -> Feu
      | "Eau" -> Eau
      | "Plante" -> Plante
      | "Normal" -> Normal
      | "Electrique" -> Electrique
      | "Psy" -> Psy
      | "Tenebre" -> Tenebre
      | "Glace" -> Glace
      | _ -> failwith "Unknown element");
    competence = player_json |> member "competence" |> to_list |> List.map (fun competence_json ->
      {
        id = competence_json |> member "id" |> to_int;
        name = competence_json |> member "name" |> to_string;
        description = competence_json |> member "description" |> to_string;
        element = (competence_json |> member "element" |> to_string |> function
          | "Feu" -> Feu
          | "Eau" -> Eau
          | "Plante" -> Plante
          | "Normal" -> Normal
          | "Electrique" -> Electrique
          | "Psy" -> Psy
          | "Tenebre" -> Tenebre
          | "Glace" -> Glace
          | _ -> failwith "Unknown element");
        puissance = competence_json |> member "puissance" |> to_int;
        precision = competence_json |> member "precision" |> to_int;
        attaqueType = (competence_json |> member "attaqueType" |> to_string |> function
          | "Attaque" -> Attaque
          | "AttaqueSpeciale" -> AttaqueSpeciale
          | "Passive" -> Passive
          | _ -> failwith "Unknown attaqueType");
      }
    );
    path = [];
    your_turn = player_json |> member "your_turn" |> to_bool;
    money = player_json |> member "money" |> to_int;
  } in

  let enemy = enemy_json |> List.map (fun enemy_json ->
    {
      nom = enemy_json |> member "nom" |> to_string;
      id = enemy_json |> member "id" |> to_int;
      last_id = enemy_json |> member "last_id" |> to_int;
      number = enemy_json |> member "number" |> to_int;
      position = { 
        world_x = enemy_json |> member "position" |> member "world_x" |> to_float;
        world_y = enemy_json |> member "position" |> member "world_y" |> to_float;
        screen_x = enemy_json |> member "position" |> member "screen_x" |> to_int;
        screen_y = enemy_json |> member "position" |> member "screen_y" |> to_int;
        target_x = enemy_json |> member "position" |> member "target_x" |> to_float;
        target_y = enemy_json |> member "position" |> member "target_y" |> to_float;
      };
      entity_textures_id = 0;
      moving = false;
      state = Idle;
      direction = Down;
      current_hp = enemy_json |> member "current_hp" |> to_int;
      max_hp = enemy_json |> member "max_hp" |> to_int;
      level = enemy_json |> member "level" |> to_int;
      current_xp = enemy_json |> member "current_xp" |> to_int;
      max_xp = enemy_json |> member "max_xp" |> to_int;
      action = Nothing;
      bag = {
        items = enemy_json |> member "bag" |> member "items" |> to_list |> List.map (fun item ->
          {
            item_id = item |> member "item_id" |> to_int;
            item_skin_id = item |> member "item_skin_id" |> to_int;
            quantity = item |> member "quantity" |> to_int;
            pos_x = item |> member "pos_x" |> to_float;
            pos_y = item |> member "pos_y" |> to_float;
            screen_x = item |> member "screen_x" |> to_int;
            screen_y = item |> member "screen_y" |> to_int;
            description = item |> member "description" |> to_string;
            usable = item |> member "usable" |> to_bool;
          }
        );
        max_size = enemy_json |> member "bag" |> member "max_size" |> to_int;
        selected_item = 0;
      };
      step_cpt = enemy_json |> member "step_cpt" |> to_int;
      speed = enemy_json |> member "speed" |> to_float;
      attaque = enemy_json |> member "attaque" |> to_int;
      defense = enemy_json |> member "defense" |> to_int;
      attaque_speciale = enemy_json |> member "attaque_speciale" |> to_int;
      defense_speciale = enemy_json |> member "defense_speciale" |> to_int;
      element = (enemy_json |> member "element" |> to_string |> function
        | "Feu" -> Feu
        | "Eau" -> Eau
        | "Plante" -> Plante
        | "Normal" -> Normal
        | "Electrique" -> Electrique
        | "Psy" -> Psy
        | "Tenebre" -> Tenebre
        | "Glace" -> Glace
        | _ -> failwith "Unknown element");
      competence = enemy_json |> member "competence" |> to_list |> List.map (fun competence_json ->
        {
          id = competence_json |> member "id" |> to_int;
          name = competence_json |> member "name" |> to_string;
          description = competence_json |> member "description" |> to_string;
          element = (competence_json |> member "element" |> to_string |> function
            | "Feu" -> Feu
            | "Eau" -> Eau
            | "Plante" -> Plante
            | "Normal" -> Normal
            | "Electrique" -> Electrique
            | "Psy" -> Psy
            | "Tenebre" -> Tenebre
            | "Glace" -> Glace
            | _ -> failwith "Unknown element");
          puissance = competence_json |> member "puissance" |> to_int;
          precision = competence_json |> member "precision" |> to_int;
          attaqueType = (competence_json |> member "attaqueType" |> to_string |> function
            | "Attaque" -> Attaque
            | "AttaqueSpeciale" -> AttaqueSpeciale
            | "Passive" -> Passive
            | _ -> failwith "Unknown attaqueType");
        }
      );
      path = enemy_json |> member "path" |> to_list |> List.map (function
      | `List [`Int x; `Int y] -> (x, y)
      | _ -> failwith "Invalid path format"
    );
      your_turn = enemy_json |> member "your_turn" |> to_bool;
      money = enemy_json |> member "money" |> to_int;
    }) in

  let loot = loot_json |> List.map (fun loot_json ->
    {
      item_id = loot_json |> member "item_id" |> to_int;
      item_skin_id = loot_json |> member "item_skin_id" |> to_int;
      quantity = loot_json |> member "quantity" |> to_int;
      pos_x = loot_json |> member "pos_x" |> to_float;
      pos_y = loot_json |> member "pos_y" |> to_float;
      screen_x = loot_json |> member "screen_x" |> to_int;
      screen_y = loot_json |> member "screen_y" |> to_int;
      description = loot_json |> member "description" |> to_string;
      usable = loot_json |> member "usable" |> to_bool;
    }) in

  let trap_and_ground = trap_and_ground_json |> List.map (fun trap_and_ground_json ->
    {
      nature = trap_and_ground_json |> member "nature" |> to_int |> int_to_trap_ground;
      tag_pos_x = trap_and_ground_json |> member "tag_pos_x" |> to_int;
      tag_pos_y = trap_and_ground_json |> member "tag_pos_y" |> to_int;
      visibility = trap_and_ground_json |> member "visibility" |> to_bool;
    }) in

  (map, player, enemy, loot, trap_and_ground)

(** 
  [tile_to_yojson tile ] convertit une tuile en une représentation JSON.

  @param tile La tuile à convertir.

  @return Une valeur JSON de type [`Assoc] représentant la tuile, avec les clés suivantes :
  - ["x"] : la position x de la tuile.
  - ["y"] : la position y de la tuile.
  - ["texture_id"] : l'identifiant de la texture associée à la tuile.
  - ["biome_id"] : l'identifiant du biome associé à la tuile.
*)
let tile_to_yojson (tile : tile) =
  `Assoc [
    ("x", `Int tile.x);
    ("y", `Int tile.y);
    ("texture_id", `Int tile.texture_id);
    ("biome_id", `Int tile.biome_id)
  ]

(** 
  [map_to_yojson map] convertit une carte en une représentation JSON.

  @param map La carte à convertir.

  @return Une valeur JSON de type [`Assoc] représentant la carte, avec les clés suivantes :
    - ["width"] : la largeur de la carte.
    - ["height"] : la hauteur de la carte.
    - ["tiles"] : une liste de tuiles converties en JSON.
    - ["floor"] : l'étage de la carte.
*)
let map_to_yojson (map : map) =
  `Assoc [
    ("width", `Int map.width);
    ("height", `Int map.height);
    ("tiles", `List (List.map tile_to_yojson map.tiles));
    ("floor", `Int map.floor)
  ]

(**
  [bag_to_yojson bag] convertit un sac en une représentation JSON.

  @param bag Le sac à convertir.

  @return Une valeur JSON de type [`Assoc] représentant le sac, avec les clés suivantes :
  - ["items"] : une liste d'objets convertis en JSON.
  - ["max_size"] : la taille maximale du sac.
*)
let bag_to_yojson (bag : bag) =
  `Assoc [
    ("items", `List (List.map (fun item ->
      `Assoc [
        ("item_id", `Int item.item_id);
        ("item_skin_id", `Int item.item_skin_id);
        ("quantity", `Int item.quantity);
        ("pos_x", `Float item.pos_x);
        ("pos_y", `Float item.pos_y);
        ("screen_x", `Int item.screen_x);
        ("screen_y", `Int item.screen_y);
        ("description", `String item.description);
        ("usable", `Bool item.usable)
      ]
    ) bag.items));
    ("max_size", `Int bag.max_size)
  ]

(**
  [element_to_json element] convertit un élément en une représentation JSON.

  @param element L'élément à convertir.

  @return Une valeur JSON de type [`String] représentant l'élément.
*)
let element_to_json (element: element) =
  `String (match element with
    | Feu -> "Feu"
    | Eau -> "Eau"
    | Plante -> "Plante"
    | Normal -> "Normal"
    | Electrique -> "Electrique"
    | Psy -> "Psy"
    | Tenebre -> "Tenebre"
    | Glace -> "Glace"
  )
  
(**
  [competence_to_json competence] convertit une compétence en une représentation JSON.

  @param competence La compétence à convertir.

  @return Une valeur JSON de type [`Assoc] représentant la compétence, avec les clés suivantes :
  - ["id"] : l'identifiant de la compétence.
  - ["name"] : le nom de la compétence.
  - ["description"] : la description de la compétence.
  - ["element"] : l'élément de la compétence.
  - ["puissance"] : la puissance de la compétence.
  - ["precision"] : la précision de la compétence.
  - ["attaqueType"] : le type d'attaque de la compétence.
*)
let competence_to_json (competence: competence) =
  `Assoc [
    ("id", `Int competence.id);
    ("name", `String competence.name);
    ("description", `String competence.description);
    ("element", element_to_json competence.element);
    ("puissance", `Int competence.puissance);
    ("precision", `Int competence.precision);
    ("attaqueType", `String (match competence.attaqueType with
      | Attaque -> "Attaque"
      | AttaqueSpeciale -> "AttaqueSpeciale"
      | Passive -> "Passive"
    ))
  ]

(**
  [competences_to_json competences] convertit une liste de compétences en une représentation JSON.

  @param competences La liste de compétences à convertir.

  @return Une valeur JSON de type [`List] représentant la liste de compétences.
*)
let competences_to_json (competences: competence list) =
  `List (List.map competence_to_json competences)

(**
  [position_to_json position] convertit une position en une représentation JSON.

  @param position La position à convertir.

  @return Une valeur JSON de type [`Assoc] représentant la position, avec les clés suivantes :
  - ["world_x"] : la position x dans le monde.
  - ["world_y"] : la position y dans le monde.
  - ["screen_x"] : la position x à l'écran.
  - ["screen_y"] : la position y à l'écran.
  - ["target_x"] : la position cible x.
  - ["target_y"] : la position cible y.
*)
let position_to_json (position: position) =
  `Assoc [
    ("world_x", `Float position.world_x);
    ("world_y", `Float position.world_y);
    ("screen_x", `Int position.screen_x);
    ("screen_y", `Int position.screen_y);
    ("target_x", `Float position.target_x);
    ("target_y", `Float position.target_y)
  ]

(**
  [pokemon_to_yojson player] converts a player to a JSON representation.

  @param player The player to convert.

  @return A JSON value of type [`Assoc] representing the player, with the following keys:
  - ["nom"]: the name of the player.
  - ["id"]: the id of the player.
  - ["last_id"]: the last id of the player.
  - ["number"]: the number of the player.
  - ["position"]: the position of the player.
  - ["entity_textures_id"]: the texture id of the player.
  - ["current_hp"]: the current HP of the player.
  - ["max_hp"]: the maximum HP of the player.
  - ["level"]: the level of the player.
  - ["current_xp"]: the current XP of the player.
  - ["max_xp"]: the maximum XP of the player.
  - ["bag"]: the bag of the player.
  - ["step_cpt"]: the step count of the player.
  - ["speed"]: the speed of the player.
  - ["attaque"]: the attack of the player.
  - ["defense"]: the defense of the player.
  - ["attaque_speciale"]: the special attack of the player.
  - ["defense_speciale"]: the special defense of the player.
  - ["element"]: the element of the player.
  - ["competence"]: the competence of the player.
  - ["path"]: the path of the player.
  - ["your_turn"]: indicates if it's the player's turn.
  - ["money"]: the money of the player.
*)
let pokemon_to_yojson (player: pokemon) =
  `Assoc [
    ("nom", `String player.nom);
    ("id", `Int player.id);
    ("last_id", `Int player.last_id);
    ("number", `Int player.number);
    ("position", position_to_json player.position);
    ("entity_textures_id", `Int player.entity_textures_id);
    ("current_hp", `Int player.current_hp);
    ("max_hp", `Int player.max_hp);
    ("level", `Int player.level);
    ("current_xp", `Int player.current_xp);
    ("max_xp", `Int player.max_xp);
    ("bag", bag_to_yojson player.bag);
    ("step_cpt", `Int player.step_cpt);
    ("speed", `Float player.speed);
    ("attaque", `Int player.attaque);
    ("defense", `Int player.defense);
    ("attaque_speciale", `Int player.attaque_speciale);
    ("defense_speciale", `Int player.defense_speciale);
    ("element", element_to_json player.element);
    ("competence", competences_to_json player.competence);
    ("path", `List (List.map (fun (x, y) -> `List [`Int x; `Int y]) player.path));
    ("your_turn", `Bool player.your_turn);
    ("money", `Int player.money);
  ]

(**
  [pokemons_to_yojson pokemons] convertit une liste de Pokémon en une représentation JSON.

  @param pokemons La liste de Pokémon à convertir.

  @return Une valeur JSON de type [`List] représentant la liste de Pokémon.
*)
let pokemons_to_yojson (pokemons: pokemon list) =
  `List (List.map pokemon_to_yojson pokemons)

(** 
  [loot_to_json loot] convertit un butin en une représentation JSON.

  @param loot Le butin à convertir.

  @return Une valeur JSON de type [`Assoc] représentant le butin, avec les clés suivantes :
  - ["item_id"] : l'identifiant de l'objet.
  - ["item_skin_id"] : l'identifiant du skin de l'objet.
  - ["quantity"] : la quantité de l'objet.
  - ["pos_x"] : la position x du butin.
  - ["pos_y"] : la position y du butin.
  - ["screen_x"] : la position x
  - ["screen_y"] : la position y
  - ["description"] : la description du butin.  
  - ["usable"] : indique si l'objet est utilisable.
*)
let loot_to_json (loot: loot) =
  `Assoc [
    ("item_id", `Int loot.item_id);
    ("item_skin_id", `Int loot.item_skin_id);
    ("quantity", `Int loot.quantity);
    ("pos_x", `Float loot.pos_x);
    ("pos_y", `Float loot.pos_y);
    ("screen_x", `Int loot.screen_x);
    ("screen_y", `Int loot.screen_y);
    ("description", `String loot.description);
    ("usable", `Bool loot.usable);
  ]

(**
  [loots_to_json loots] convertit une liste de butins en une représentation JSON.

  @param loots La liste de butins à convertir.

  @return Une valeur JSON de type [`List] représentant la liste de butins.
*)
let loots_to_json (loots: loot list) =
  `List (List.map loot_to_json loots)

(**
  [trap_ground_to_int trap_and_ground] convertit un piège ou un sol en un entier.

  @param trap_and_ground Le piège ou le sol à convertir.

  @return Un entier représentant le piège ou le sol.
*)
let trap_and_ground_to_json (trap_and_ground: trap_and_ground) =
  `Assoc [
    ("nature", `Int (trap_and_ground.nature |> trap_ground_to_int));
    ("tag_pos_x", `Int trap_and_ground.tag_pos_x);
    ("tag_pos_y", `Int trap_and_ground.tag_pos_y);
    ("visibility", `Bool trap_and_ground.visibility);
  ]

(**
  [traps_and_grounds_to_json traps_and_grounds] convertit une liste de pièges et de sols en une représentation JSON.

  @param traps_and_grounds La liste de pièges et de sols à convertir.

  @return Une valeur JSON de type [`List] représentant la liste de pièges et de sols.
*)
let traps_and_grounds_to_json (trap_and_ground: trap_and_ground list) =
  `List (List.map trap_and_ground_to_json trap_and_ground)

(**
  [map_player_to_json map player enemy items traps_and_grounds] converts a map, a player, enemies, items, and traps/grounds to a JSON representation.

  @param map The map to convert.
  @param player The player to convert.
  @param enemy The enemy to convert.
  @param items The items to convert.
  @param traps_and_grounds The traps and grounds to convert.

  @return A JSON value of type [`Assoc] representing the map, player, enemies, items, and traps/grounds.
*)
let map_player_to_json (map: map) (player: pokemon) (enemy: pokemon list) (items: loot list) (traps_and_grounds: trap_and_ground list) =
  `Assoc [
    ("map", map_to_yojson map);
    ("player", pokemon_to_yojson player);
    ("enemy", pokemons_to_yojson enemy);
    ("loot", loots_to_json items);
    ("trap_and_ground", traps_and_grounds_to_json traps_and_grounds)
  ]

(** 
  [write_json_to_file] écrit le JSON [json] dans un fichier nommé [filename].
  
  @param filename Le nom du fichier dans lequel écrire le JSON.
  @param json Le JSON à écrire dans le fichier.
*)
let write_json_to_file (filename : string) (json : t) : unit =
  let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o666 filename in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc

(**
  [read_json_files_in_directory dir_path] lit tous les noms de fichiers .json dans un dossier.

  @param dir_path Le chemin du dossier.

  @return Une liste de noms de fichiers .json.
*)
let read_json_files_in_directory (dir_path : string) : string list =
  let files = Sys.readdir dir_path in
  Array.to_list files
  |> List.filter (fun file -> Filename.check_suffix file ".json")
  |> List.map Filename.chop_extension
