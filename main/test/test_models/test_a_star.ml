open OUnit2
open Models.A_star
open Utils.Types

let test_a_star _ =
  Printf.printf "Testing a_star :\n";

  (* Création d'une grille de tuiles *)
  let tiles = [
    { x = 0; y = 0; texture_id = 1; biome_id = 0 };
    { x = 1; y = 0; texture_id = 1; biome_id = 0 };
    { x = 2; y = 0; texture_id = 1; biome_id = 0 };
    { x = 0; y = 1; texture_id = 1; biome_id = 0 };
    { x = 1; y = 1; texture_id = 0; biome_id = 0 }; (* Mur *)
    { x = 2; y = 1; texture_id = 1; biome_id = 0 };
    { x = 0; y = 2; texture_id = 1; biome_id = 0 };
    { x = 1; y = 2; texture_id = 1; biome_id = 0 };
    { x = 2; y = 2; texture_id = 1; biome_id = 0 };
  ] in

  (* Définir le point de départ et le but *)
  let start = (0, 0) in
  let goal = (2, 2) in

  (* Appeler la fonction a_star *)
  let path = a_star tiles start goal [] in

  (* Vérifier le chemin trouvé *)
  Printf.printf "Path found: %s\n" (String.concat " -> " (List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y) path));
  assert_equal [(1, 0); (2, 0); (2, 1)] path;
  Printf.printf "a_star test 1 passed\n";

  let path = a_star tiles start goal [(1, 0)] in
  Printf.printf "Path found: %s\n" (String.concat " -> " (List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y) path));
  assert_equal [(0, 1); (0, 2); (1, 2)] path;
  Printf.printf "a_star test 2 passed\n";

  let path = a_star tiles start goal [(1, 0); (2, 1)] in
  Printf.printf "Path found: %s\n" (String.concat " -> " (List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y) path));
  assert_equal [(0, 1); (0, 2); (1, 2)] path;
  Printf.printf "a_star test 3 passed\n\n"

let test_manhattan _ =
  Printf.printf "Testing manhattan_distance :\n";

  let distance = manhattan_distance ((0, 0), (3, 4)) in
  Printf.printf "Distance: %d\n" distance;
  assert_equal 7 distance;
  Printf.printf "manhattan_distance test 1 passed\n";

  let distance = manhattan_distance ((10, 20), (40, 60)) in
  Printf.printf "Distance: %d\n" distance;
  assert_equal 70 distance;
  Printf.printf "manhattan_distance test 2 passed\n";

  let distance = manhattan_distance ((-100, 200), (300, -400)) in
  Printf.printf "Distance: %d\n" distance;
  assert_equal 1000 distance;
  Printf.printf "manhattan_distance test 3 passed\n\n"


(* Test suite *)
let suite =
  "AStar Tests" >::: [
    "test_a_star" >:: test_a_star;
    "test_manhattan" >:: test_manhattan;
  ]