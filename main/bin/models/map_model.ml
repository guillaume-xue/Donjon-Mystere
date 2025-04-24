open Utils.Types

(**
  [is_in_map x y map] checks if the coordinates (x, y) are within the bounds of the map.
  @param x The x coordinate.
  @param y The y coordinate.
  @param map The map to check against.
  @return True if the coordinates are within the map, false otherwise.
*)
let is_in_map x y map =
  (* Check if the coordinates are within the bounds of the map *)
  x >= 0 && x < map.width && y >= 0 && y < map.height

(**
  [is_wall x y tiles] checks if the tile at (x, y) is a wall.
  @param x The x coordinate of the tile.
  @param y The y coordinate of the tile.
  @param tiles The list of tiles.
  @return True if the tile is a wall, false otherwise.
*)
let is_wall x y map =
  let rec aux tiles =
    match tiles with
    | [] -> false
    | tile :: rest ->
      if tile.x = x && tile.y = y && tile.texture_id = 0 then
        true
      else
        aux rest
  in
  if is_in_map x y map then
    aux map.tiles
  else
    false

let set_map_floor map floor = 
  { 
    width = map.width; 
    height = map.height; 
    tiles = map.tiles; 
    regions = map.regions;
    floor = floor
  }

let find_wall_in_direction x y direction map =
  let rec aux aux_x aux_y =
    let (new_x, new_y) = 
      match direction with
      | Up -> (aux_x, (aux_y - 1))
      | Down -> (aux_x, (aux_y + 1))
      | Left -> ((aux_x - 1), aux_y)
      | Right -> ((aux_x + 1), aux_y)
      | _ -> (aux_x, aux_y)
    in
    if is_wall new_x new_y map then
      Some (aux_x, aux_y)
    else
      aux new_x new_y
  in
  aux x y

let set_map_tile (tiles : tile list) (map : map) =
  {
    width = map.width;
    height = map.height;
    tiles = tiles;
    regions = map.regions;
    floor = map.floor
  }

let rec take n lst =
  match lst with
  | [] -> []
  | hd :: tl -> if n > 0 then hd :: take (n - 1) tl else []

let set_map_exploded x y map =
  let radius = 8 in
  let tiles_in_radius =
    List.filter
      (fun tile ->
        let dx = tile.x - x in
        let dy = tile.y - y in
        dx * dx + dy * dy <= radius * radius && tile.texture_id = 0)
      map.tiles
  in
  let num_to_remove = Random.int (List.length tiles_in_radius + 1) in
  let tiles_to_keep =
    List.filter
      (fun tile -> not (List.mem tile (take num_to_remove tiles_in_radius)))
      map.tiles
  in
  set_map_tile tiles_to_keep map
