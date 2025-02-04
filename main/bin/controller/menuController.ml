open Raylib

let check_button_click () =
  if is_mouse_button_pressed MouseButton.Left then
    let mouse_x = get_mouse_x () in
    let mouse_y = get_mouse_y () in
    if mouse_x >= 0 && mouse_x <= 800 then
      if mouse_y >= 0 && mouse_y <= 600 then
        print_endline "Play clicked"