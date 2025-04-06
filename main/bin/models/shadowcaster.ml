open Utils.Types

(* Multipliers for octants *)
let multipliers = [|
  [| 1; 0; 0; -1; -1; 0; 0; 1 |];
  [| 0; 1; -1; 0; 0; -1; 1; 0 |];
  [| 0; 1; 1; 0; 0; -1; -1; 0 |];
  [| 1; 0; 0; 1; -1; 0; 0; -1 |];
|]

(**
  Casts light in a given octant.
  @param x The x coordinate of the light source.
  @param y The y coordinate of the light source.
  @param x_max The maximum x coordinate of the grid.
  @param y_max The maximum y coordinate of the grid.
  @param radius The radius of the light.
  @param set_visible A function to set visibility for a tile.
  @param is_blocked A function to check if a tile is blocked.
  @param octant The octant to cast light in (0-7).
  @param row The current row being processed.
  @param start_slope The starting slope for the current row.
  @param end_slope The ending slope for the current row.
  @param xx, xy, yx, yy Multipliers for the octant transformation.
*)
let rec cast_light x y x_max y_max radius set_visible is_blocked octant row start_slope end_slope xx xy yx yy =
  if start_slope < end_slope then ()
  else
    let rec process_row i start_slope =
      if i > radius then ()
      else
        let rec process_column dx start_slope blocked =
          if dx > 0 then start_slope
          else
            let dy = -i in
            let l_slope = (float_of_int dx -. 0.5) /. (float_of_int dy +. 0.5) in
            let r_slope = (float_of_int dx +. 0.5) /. (float_of_int dy -. 0.5) in
            if start_slope < r_slope then process_column (dx + 1) start_slope blocked
            else if end_slope > l_slope then start_slope
            else
              let sax = dx * xx + dy * xy in
              let say = dx * yx + dy * yy in
              if (sax < 0 && abs sax > x) || (say < 0 && abs say > y) then process_column (dx + 1) start_slope blocked
              else
                let ax = x + sax in
                let ay = y + say in
                if ax < 0 || ay < 0 || ax >= x_max || ay >= y_max then process_column (dx + 1) start_slope blocked
                else
                  let radius2 = radius * radius in
                  if dx * dx + dy * dy < radius2 then
                    set_visible ax ay (float_of_int i /. float_of_int radius);
                  if blocked then
                    if is_blocked ax ay then
                      process_column (dx + 1) r_slope true
                    else
                      let _ = cast_light x y x_max y_max radius set_visible is_blocked octant (i + 1) start_slope l_slope xx xy yx yy in
                      process_column (dx + 1) start_slope false
                  else if is_blocked ax ay then
                    let _ = cast_light x y x_max y_max radius set_visible is_blocked octant (i + 1) start_slope l_slope xx xy yx yy in
                    process_column (dx + 1) r_slope true
                  else
                    process_column (dx + 1) start_slope false
        in
        let new_start_slope = process_column (-i) start_slope false in
        process_row (i + 1) new_start_slope
    in
    process_row row start_slope

(** 
  Process all octants for a given tile.
  @param x The x coordinate of the light source.
  @param y The y coordinate of the light source.
  @param x_max The maximum x coordinate of the grid.
  @param y_max The maximum y coordinate of the grid.
  @param radius The radius of the light.
  @param visibility A function to set visibility for a tile.
  @param i The current octant being processed (0-7).
*)
let rec process_octants x y x_max y_max radius visibility i =
  if i > 7 then visibility
  else (
    cast_light x y x_max y_max radius i 1 1.0 0.0
      multipliers.(0).(i) multipliers.(1).(i)
      multipliers.(2).(i) multipliers.(3).(i);
    process_octants x y x_max y_max radius visibility (i + 1)
  )

(**
  Compute the field of view for a player.
  @param player The player object.
  @param radius The radius of the light.
  @param grid The grid of tiles.
  @param max_x The maximum x coordinate of the grid.
  @param max_y The maximum y coordinate of the grid.
  @return A matrix representing visibility for each tile.
*)
let compute_fov player radius grid max_x max_y =
  let x = int_of_float (player.pos_x) in
  let y = int_of_float (player.pos_y) in
  let visibility = Array.make_matrix max_y max_x (0.0) in
  let set_visible x y visibility_value =
    if x >= 0 && x < max_x && y >= 0 && y < max_y then
      visibility.(y).(x) <- visibility_value
  in
  let is_blocked x y = 
    let rec is_this_tile x y grid =
      match grid with
      | [] -> false
      | tile :: rest ->
        if tile.x = x && tile.y = y then
          if tile.texture_id = 0 then
            true
          else
            is_this_tile x y rest
        else
          is_this_tile x y rest
    in
    is_this_tile x y grid
  in
  set_visible x y 0.0;
  process_octants x y max_x max_y radius set_visible is_blocked visibility 0