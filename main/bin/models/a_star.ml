open Utils.Types

(**
  [manhattan_distance] calcule la distance de Manhattan entre deux points.

  @param (x1, y1) La première coordonnée.
  @param (x2, y2) La deuxième coordonnée.
  @return La distance de Manhattan entre les deux points.
*)
let manhattan_distance (((x1, y1): int * int), ((x2, y2): int * int)) : int =
  abs (x1 - x2) + abs (y1 - y2)

(**
  [a_star] implémente l'algorithme A* pour trouver le chemin le plus court entre deux points.

  @param tiles La liste des tuiles.
  @param start La position de départ.
  @param goal La position d'arrivée.
  @return Une liste de coordonnées représentant le chemin trouvé.
*)
let a_star (tiles: tile list) (start: int * int) (goal: int * int) : (int * int) list =

  let pq = PriorityQueue.create () in
  let visited = ref [] in
  let came_from = Hashtbl.create 100 in
  let g_score = Hashtbl.create 100 in
  let f_score = Hashtbl.create 100 in

  (* Initialisation *)
  Hashtbl.add g_score start 0;
  Hashtbl.add f_score start (manhattan_distance (start, goal));
  PriorityQueue.add pq (manhattan_distance (start, goal)) start;

  let rec reconstruct_path (current: int * int) (path: (int * int) list) : (int * int) list =
    if Hashtbl.mem came_from current then
      reconstruct_path (Hashtbl.find came_from current) (current :: path)
    else
      List.filter (fun (x, y) -> (x, y) <> start && (x, y) <> goal) (current :: path)
    in

  let rec search () : (int * int) list =
    if PriorityQueue.is_empty pq then
      []
    else
      let _, current = PriorityQueue.pop pq in
      if current = goal then
        reconstruct_path current []
      else if List.exists ((=) current) !visited then
        search ()
      else (
        visited := current :: !visited;
        let neighbors = List.filter (fun tile ->
          tile.texture_id <> 0 && (* Pas un mur *)
          List.exists (fun (dx, dy) -> tile.x = fst current + dx && tile.y = snd current + dy)
            [(1, 0); (-1, 0); (0, 1); (0, -1)]
        ) tiles in
        List.iter (fun neighbor ->
          let neighbor_pos = (neighbor.x, neighbor.y) in
          let tentative_g_score = Hashtbl.find g_score current + 1 in
          if not (Hashtbl.mem g_score neighbor_pos) || tentative_g_score < Hashtbl.find g_score neighbor_pos then (
            Hashtbl.replace came_from neighbor_pos current;
            Hashtbl.replace g_score neighbor_pos tentative_g_score;
            Hashtbl.replace f_score neighbor_pos (tentative_g_score + manhattan_distance (neighbor_pos, goal));
            PriorityQueue.add pq (Hashtbl.find f_score neighbor_pos) neighbor_pos
          )
        ) neighbors;
        search ()
      )
  in
  search ()