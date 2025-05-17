open Utils.Types

(**
  [is_in_map x y map] checks if the coordinates (x, y) are within the bounds of the map.
  @param x The x coordinate.
  @param y The y coordinate.
  @param map The map to check against.
  @return True if the coordinates are within the map, false otherwise.
*)
let is_in_map (x : int) (y : int) (map : map) : bool =
  (* Check if the coordinates are within the bounds of the map *)
  x >= 0 && x < map.width && y >= 0 && y < map.height

(**
  [is_wall x y tiles] checks if the tile at (x, y) is a wall.
  @param x The x coordinate of the tile.
  @param y The y coordinate of the tile.
  @param tiles The list of tiles.
  @return True if the tile is a wall, false otherwise.
*)
let is_wall (x : int) (y : int) (map : map) : bool =
  let rec aux (tiles : tile list) : bool =
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

(**
  [set_map_floor map floor] sets the floor of the map.
  @param map The map to update.
  @param floor The new floor to set.
  @return A new map with the updated floor.
*)
let set_map_floor (map : map) (floor : int) : map = 
  { 
    width = map.width; 
    height = map.height; 
    tiles = map.tiles; 
    regions = map.regions;
    floor = floor;
    music = map.music
  }

(**
  [find_wall_in_direction x y direction map] finds the first wall in the specified direction from (x, y).
  @param x The x coordinate.
  @param y The y coordinate.
  @param direction The direction to search in.
  @param map The map to search in.
  @return An option containing the coordinates of the wall if found, or None if not found.
*)
let find_wall_in_direction (x : int) (y : int) (direction : direction) (map : map) : (int * int) option =
  let rec aux (aux_x : int) (aux_y : int) : (int * int) option =
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

(**
  [set_map_tile tiles map] sets the tiles of the map.
  @param tiles The new list of tiles.
  @param map The map to update.
  @return A new map with the updated tiles.
*)
let set_map_tile (tiles : tile list) (map : map) : map =
  {
    width = map.width;
    height = map.height;
    tiles = tiles;
    regions = map.regions;
    floor = map.floor;
    music = map.music
  }

(**
  [set_map_music music map] sets the music of the map.
  @param music The new music to set.
  @param map The map to update.
  @return A new map with the updated music.
*)
let set_map_music (music : string) (map : map) : map =
  {
    width = map.width;
    height = map.height;
    tiles = map.tiles;
    regions = map.regions;
    floor = map.floor;
    music = Some music
  }

(**
  [take n lst] takes the first n elements from the list lst.
  @param n The number of elements to take.
  @param lst The list to take elements from.
  @return A new list containing the first n elements of lst.
*)
let rec take (n : int) (lst : tile list) : tile list =
  match lst with
  | [] -> []
  | hd :: tl -> if n > 0 then hd :: take (n - 1) tl else []

(**
  [set_map_exploded x y map] sets the map to exploded state by removing tiles in a radius around (x, y).
  @param x The x coordinate of the explosion.
  @param y The y coordinate of the explosion.
  @param map The map to update.
  @return A new map with the updated tiles.
*)
let set_map_exploded (x : int) (y : int) (map: map) : map =
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
