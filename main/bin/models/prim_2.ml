open Utils.Types
open Utils.Settings_map

(** 
  [calcul_arete] calcule toutes les arêtes possibles entre toutes 
  les zones et garde les arêtes avec la plus faible distance.

  @param zones La liste des zones.
  @return Une liste d'arêtes.
*)
let calcul_arete (zones : zone list) : arete list =
  let find_closest_arete (zone1 : zone) (zone2 : zone) : arete option =
    let min_arete = ref None in
    (* Ici on test toute les combinaisons entre les tuiles de deux zones *)
    List.iter (fun tile1 ->
      List.iter (fun tile2 ->
        let dist = sqrt (float_of_int ((tile1.x - tile2.x) * (tile1.x - tile2.x) + (tile1.y - tile2.y) * (tile1.y - tile2.y))) in
        match !min_arete with
        (* On utilise le format du type "arete" *)
        | None -> min_arete := Some { zone1; zone2; coord1 = (tile1.x, tile1.y); coord2 = (tile2.x, tile2.y); distance = dist }
        | Some arete -> if dist < arete.distance then min_arete := Some { zone1; zone2; coord1 = (tile1.x, tile1.y); coord2 = (tile2.x, tile2.y); distance = dist }
      ) zone2.tiles
    ) zone1.tiles;
    !min_arete
  in
  (* Et ici toute les combinaisons de zones par pair *)
  let rec aux (zones : zone list) (aretes : arete list) : arete list =
    match zones with
    | [] -> aretes
    | zone :: rest ->
      let new_arete = List.fold_left (fun acc z ->
        match find_closest_arete zone z with
        | None -> acc
        | Some arete -> arete :: acc
      ) aretes rest in
      aux rest new_arete
  in
  aux zones []

(** 
  [prim] applique l'algorithme de Prim pour trouver un arbre couvrant minimal (MST) des zones.

  La fonction utilise une table de hachage [visited] pour suivre les zones visitées et une file 
  de priorité [pq] pour gérer les arêtes en fonction de leur distance.

  La fonction commence par initialiser la première zone et ajoute toutes ses arêtes à la file 
  de priorité. Ensuite, elle utilise une fonction récursive [aux] pour construire le MST en ajoutant 
  les arêtes les plus courtes qui connectent les zones non visitées.
  
  @param zones La liste des zones (ce sont les noeuds).
  @param aretes La liste des arêtes entre les zones.
  @return Une liste d'arêtes représentant le MST.
*)

let prim (zones: zone list) (aretes : arete list) : arete list =
  let visited = Hashtbl.create (List.length zones) in
  let pq = PriorityQueue.create () in

  (* Initialiser avec la première zone *)
  let start_zone = List.hd zones in
  Hashtbl.add visited start_zone.id true;

  (* Ajouter les arêtes de la première zone à la file de priorité *)
  List.iter (fun arete ->
    if arete.zone1.id = start_zone.id || arete.zone2.id = start_zone.id then
      PriorityQueue.add pq arete.distance arete
  ) aretes;

  let rec aux (mst : arete list) : arete list =
    if PriorityQueue.is_empty pq then mst
    else
      (* L'arete avec la plus faible distance *)
      let _, arete = PriorityQueue.pop pq in
      (* La 2eme zone de l'arete si sa 1ère est déjà visitée *)
      let new_zone =
        if Hashtbl.mem visited arete.zone1.id then arete.zone2
        else arete.zone1
      in
      (* Si la nouvelle zone n'a pas été visité *)
      if not (Hashtbl.mem visited new_zone.id) then (
        Hashtbl.add visited new_zone.id true;
        (* Ajouter les arêtes de la nouvelle zone à la file de priorité *)
        List.iter (fun e ->
          if e.zone1.id = new_zone.id || e.zone2.id = new_zone.id then
            PriorityQueue.add pq e.distance e
        ) aretes;
        aux (arete :: mst)
      ) else
        aux mst
  in
  aux []

(** 
  [create_path] crée un chemin entre deux coordonnées
  à partir de l'algorithme de Bresenham.

  @param coord1 Les coordonnées de départ.
  @param coord2 Les coordonnées d'arrivée.
  @return Une liste de tuiles représentant le chemin.
*)
let create_path ((x1, y1) : int * int) ((x2, y2) : int * int) : tile list =
  let bresenham (x0 : int) (y0 : int) (x1 : int) (y1 : int) : tile list =
    (* Différence absolue *)
    let dx = abs (x1 - x0) in
    let dy = abs (y1 - y0) in 
    (* Direction à prendre (pente de la courbe) *)
    let sx = if x0 < x1 then 1 else -1 in
    let sy = if y0 < y1 then 1 else -1 in
    let rec loop (x : int) (y : int) (err : int) (acc : tile list) : tile list =
      (* Si dernière tuile, la rajouter *)
      if x = x1 && y = y1 then { x; y; texture_id = 1; biome_id = 0 } :: acc
      else
        (* Calcul de l'erreur, doublé, pour travailler avec des entiers *)
        let e2 = 2 * err in
        let new_acc = { x; y; texture_id = 1; biome_id = 0 } :: acc in
        (* Si l'erreur est plus grande que la différence en y, on effectue un déplacement en x *)
        if e2 > -dy then loop (x + sx) y (err - dy) new_acc
        (* Si l'erreur est plus grande que la différence en x, on effectue un déplacement en y *)
        else if e2 < dx then loop x (y + sy) (err + dx) new_acc
        (* Sinon un déplacement en x et y *)
        else loop (x + sx) (y + sy) (err - dy + dx) new_acc
    in
    loop x0 y0 (dx - dy) []
  in
  bresenham x1 y1 x2 y2

let update_tiles (tiles : tile list) (path : tile list) : tile list =
  List.map (fun t ->
    match List.find_opt (fun p -> p.x = t.x && p.y = t.y) path with
    | Some new_tile -> new_tile
    | None -> t
  ) tiles

(**
  [connect_zones] connecte les zones entre elles en utilisant l'algorithme de Prim
  et ajoute des chemins supplémentaires avec une probabilité donnée.

  @param tiles La liste de tuiles.
  @param zones La liste de zones.
  @return Une liste de tuiles avec les chemins ajoutés.
*)
let connect_zones (tiles : tile list) (zones : zone list) : tile list =
  let aretes = calcul_arete zones in
  let mst = prim zones aretes in

  (* Choisir parmi la liste d'arete de potentiel nouveau chemin *)
  let filtered_aretes = List.filter (fun _ ->
    Random.float 1.0 <= proba_route_bonus
  ) aretes in

  (* Ajouter les chemins du MST aux tuiles *)
  List.fold_left (fun acc arete ->
    let path = create_path arete.coord1 arete.coord2 in
    update_tiles acc path
  ) tiles (mst @ filtered_aretes)
