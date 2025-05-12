open Utils.Types

(* Calcul de la distance de Manhattan entre deux points *)
let manhattan_distance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

(* Fonction A* pour trouver le meilleur chemin entre deux tuiles *)
let a_star tiles start goal : (int * int) list=

  let pq = PriorityQueue.create () in
  let visited = ref [] in
  let came_from = Hashtbl.create 100 in
  let g_score = Hashtbl.create 100 in
  let f_score = Hashtbl.create 100 in

  (* Initialisation *)
  Hashtbl.add g_score start 0;
  Hashtbl.add f_score start (manhattan_distance start goal);
  PriorityQueue.add pq (manhattan_distance start goal) start;

  let rec reconstruct_path current path =
    if Hashtbl.mem came_from current then
      reconstruct_path (Hashtbl.find came_from current) (current :: path)
    else
      List.filter (fun (x, y) -> (x, y) <> start && (x, y) <> goal) (current :: path)
    in

  let rec search () =
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
            Hashtbl.replace f_score neighbor_pos (tentative_g_score + manhattan_distance neighbor_pos goal);
            PriorityQueue.add pq (Hashtbl.find f_score neighbor_pos) neighbor_pos
          )
        ) neighbors;
        search ()
      )
  in
  search ()

let update_a_star tiles start goal =
  (* match start.mode_combat.path with
  | Some path1 ->
    if List.length path1.nodes > 2 then begin
      let index = List.length path1.nodes - 3 in
      let nodes = List.nth path1.nodes index in
      let new_path = a_star tiles nodes (int_of_float goal.pos_x, int_of_float goal.pos_y) in
      (match new_path with
      | Some path ->
        let dist = manhattan_distance (int_of_float start.pos_x, int_of_float start.pos_y) (int_of_float goal.pos_x, int_of_float goal.pos_y) in
        Printf.printf "Pokemon %d\n%!" dist;
          { start with
            mode_combat = {
              on_off = false;
              tour = 0;
              text = [];
              path = Some { 
                nodes = (List.rev (List.tl (List.tl (List.rev path1.nodes)))) @ path.nodes; 
                cost = 0.0;
              };
            }
          }
      | None -> start)
      end
    else begin
      { start with
        mode_combat = {
          on_off = false;
          tour = 0;
          text = [];
          path = a_star tiles (int_of_float start.pos_x, int_of_float start.pos_y) (int_of_float goal.pos_x, int_of_float goal.pos_y);
        }
      }
    end
  | None ->
    start *)
  { start with
    path = a_star tiles (int_of_float start.pos_x, int_of_float start.pos_y) (int_of_float goal.pos_x, int_of_float goal.pos_y);
  }

