open Utils.Types
open Utils.Settings_map

(** 
  [nb_voisins_vivant] calcule le nombre de voisins vivants pour une cellule située aux coordonnées (x, y) dans une grille de tuiles.

  @param tiles une liste de tuiles représentant la grille. Chaque tuile est supposée avoir des champs [x], [y] pour ses coordonnées et [texture_id] pour son état (vivant ou mort).
  @param x la coordonnée x de la cellule pour laquelle on veut compter les voisins vivants.
  @param y la coordonnée y de la cellule pour laquelle on veut compter les voisins vivants.
  @return le nombre de voisins vivants autour de la cellule située aux coordonnées (x, y). Le nombre de voisins vivants est déterminé en additionnant les [texture_id] des tuiles voisines.
*)
let nb_voisins_vivant (tiles: tile list) (x: int) (y: int) : int=
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, -1); (1, -1); (-1, 1)] in
  List.fold_left (fun acc tile ->
    if List.exists (fun (dx, dy) -> tile.x = x + dx && tile.y = y + dy) directions then
      acc + tile.texture_id
    else
      acc
  ) 0 tiles

(** 
  [regles_auto_cell] applique les règles de l'automate cellulaire sur une liste de tuiles pour un nombre donné d'itérations.

  Les règles de l'automate cellulaire sont les suivantes :
  - Si la tuile a une texture_id de 1 et a au moins 4 voisins vivants, elle reste vivante (texture_id reste 1), sinon elle meurt (texture_id devient 0).
  - Si la tuile a une texture_id de 0 et a exactement 5 ou 6 voisins vivants, elle devient vivante (texture_id devient 1), sinon elle reste morte (texture_id reste 0).
  - Pour toute autre valeur de texture_id, la tuile devient morte (texture_id devient 0).

  @param tiles La liste des tuiles initiales.
  @param n Le nombre d'itérations à appliquer.
  @return La liste des tuiles après l'application des règles de l'automate cellulaire.
*)
let rec regles_auto_cell (tiles: tile list) (n: int) : tile list =
  if n = 0 then tiles
  else
    let new_tiles = List.map (fun tile ->
      let voisins = nb_voisins_vivant tiles tile.x tile.y in
      let texture_id =
        match tile.texture_id with
        | 1 -> if voisins >= 4 then 1 else 0
        | 0 -> if voisins = 5 || voisins = 6 then 1 else 0
        | _ -> 0
      in
      { tile with texture_id }
    ) tiles in
    regles_auto_cell new_tiles (n - 1)

(** 
  [flood_fill] effectue un algorithme de remplissage par diffusion (flood fill) 
  sur une carte représentée par [tiles], en partant de la position [(x, y)]. 

  L'algorithme utilise une pile pour effectuer une recherche en profondeur (DFS) et explore les quatre directions cardinales 
  (haut, bas, gauche, droite) à partir de chaque tuile. Si une tuile voisine a un identifiant de texture de 1 et n'a pas encore 
  été visitée, elle est ajoutée à la pile pour exploration future.

  @param tiles une liste de tuiles représentant la carte, où chaque tuile a des coordonnées (x, y) et un identifiant de texture.
  @param visited une référence vers une liste de positions déjà visitées.
  @param x la coordonnée x de la position de départ.
  @param y la coordonnée y de la position de départ.
  @return la taille de la zone remplie par diffusion, c'est-à-dire le nombre de tuiles connectées ayant la même texture_id.
*)
let flood_fill (tiles: tile list) (visited: (int * int) list) (x: int) (y: int) : int * (int * int) list =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let rec dfs (stack : (int * int) list) (zone_size : int) (visited: (int * int) list) : int * (int * int) list =
    match stack with
    | [] -> (zone_size, visited)
    | (cx, cy) :: rest ->
      if not (List.exists (fun (vx, vy) -> vx = cx && vy = cy) visited) then
        let visited = (cx, cy) :: visited in
        let new_stack = List.fold_left (fun acc (dx, dy) ->
          let nx, ny = (cx + dx, cy + dy) in
          if List.exists (fun tile -> tile.x = nx && tile.y = ny && tile.texture_id = 1) tiles then
            (nx, ny) :: acc
          else
            acc
        ) rest directions in
        dfs new_stack (zone_size + 1) visited
      else
        dfs rest zone_size visited
  in
  dfs [(x, y)] 0 visited

(** 
  [remove_zone] supprime une zone de tuiles connectées à partir de la position (x, y) 
  en utilisant une recherche en profondeur (DFS). La fonction prend une liste de tuiles [tiles] 
  et les coordonnées [x] et [y] de la tuile de départ. Elle retourne une nouvelle liste de tuiles 
  où la zone connectée a été mise à jour avec des identifiants de texture à 0.

  @param tiles La liste des tuiles à mettre à jour.
  @param x La coordonnée x de la tuile de départ.
  @param y La coordonnée y de la tuile de départ.
  @return Une nouvelle liste de tuiles avec la zone connectée mise à jour.
*)
let remove_zone (tiles: tile list) (x: int) (y: int) : tile list =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let rec dfs (stack: (int * int) list) (updated_tiles: tile list) : tile list =
    match stack with
    | [] -> updated_tiles
    | (cx, cy) :: rest ->
      let updated_tiles = List.map (fun tile ->
        if tile.x = cx && tile.y = cy then { tile with texture_id = 0 } else tile
      ) updated_tiles in
      let new_stack = List.fold_left (fun acc (dx, dy) ->
        let nx, ny = (cx + dx, cy + dy) in
        if List.exists (fun tile -> tile.x = nx && tile.y = ny && tile.texture_id = 1) updated_tiles then
          (nx, ny) :: acc
        else
          acc
      ) rest directions in
      dfs new_stack updated_tiles
  in
  dfs [(x, y)] tiles

(** 
  [remove_small_zones] supprime les petites zones de tuiles avec une texture spécifique.

  La fonction [remove_small_zones] parcourt la liste des tuiles et utilise une fonction auxiliaire récursive 
  [remove_small_zones_aux] pour traiter chaque tuile. Si une tuile a une texture spécifique (texture_id = 1) 
  et n'a pas encore été visitée, elle utilise un algorithme de remplissage par diffusion (flood fill) pour 
  déterminer la taille de la zone connectée. Si la taille de la zone est inférieure à une taille minimale 
  définie ([map_min_size]), la zone est supprimée de la liste des tuiles. Sinon, la tuile est conservée 
  et marquée comme visitée. La fonction renvoie la liste des tuiles après suppression des petites zones.

  @param tiles La liste des tuiles à traiter.
  @param visited Une référence à une liste de coordonnées des tuiles déjà visitées.
  @return La liste des tuiles après suppression des petites zones.
*)
let remove_small_zones (tiles: tile list) : tile list =
  let rec remove_small_zones_aux (tiles: tile list) (visited: (int * int) list) : tile list =
    match tiles with
    | [] -> []
    | tile :: rest ->
      if tile.texture_id = 1 && not (List.exists (fun (vx, vy) -> vx = tile.x && vy = tile.y) visited) then
        let (zone_size, visited2) = flood_fill tiles visited tile.x tile.y in
        if zone_size < map_min_size then
          remove_small_zones_aux (remove_zone tiles tile.x tile.y) visited2
        else
          tile :: remove_small_zones_aux rest ((tile.x, tile.y) :: visited2)
      else
        tile :: remove_small_zones_aux rest visited
  in
  remove_small_zones_aux tiles []
