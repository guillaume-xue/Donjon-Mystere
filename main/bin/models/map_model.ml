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