open Types
open Raylib
open Settings_map

(**
  Remplace l'élément à l'index `n` par `new_value` dans la liste `lst`
  @param lst La liste
  @param n L'index
  @param new_value La nouvelle valeur
  @return La liste avec l'élément à l'index `n` remplacé par `new_value`
*)
let rec replace_nth lst n new_value =
  if n < 0 || n >= List.length lst then
    failwith "Index out of bounds"
  else
    match lst with
    | [] -> failwith "Index out of bounds"
    | _ :: tl when n = 0 -> new_value :: tl
    | hd :: tl -> hd :: replace_nth tl (n - 1) new_value

let rec init_textures x max image textures =
  if x < max then
    begin
      let image_width = float_of_int(Image.width image) in
      let source_rec = Rectangle.create 0.0 (image_width *. float_of_int(x)) image_width image_width in
      let sub_image = image_from_image image source_rec in
      image_resize (addr sub_image) (int_of_float tile_texture_size) (int_of_float tile_texture_size); (* Resize the sub-image to 24x24 pixels *)
      let tex = load_texture_from_image sub_image in
      init_textures (x + 1) max image (tex :: textures)
    end
  else
    textures

let trap_ground_to_int trap_ground =
  match trap_ground with 
  | Stairs_Up -> 0
  | Stairs_Down -> 1
  | Bug_Switch -> 2
  | Chestnut_Switch -> 3
  | Drop_Hole -> 4
  | Explosion_Switch -> 5
  | Fan_Switch -> 6
  | Glue_Switch -> 7
  | Grimer_Switch -> 8
  | Imprison_Switch -> 9
  | Mud_Switch -> 10
  | Poison_Sting_Switch -> 11
  | Pokemon_Switch -> 12
  | Self_Destruct_Switch -> 13
  | Skill_Drop_Switch -> 14
  | Slowpoke_Switch -> 15
  | Spin_Swith -> 16
  | Summon_Switch -> 17
  | Warp_Trap -> 18

let int_to_trap_ground i =
  match i with 
  | 0 -> Stairs_Up
  | 1 -> Stairs_Down
  | 2 -> Bug_Switch
  | 3 -> Chestnut_Switch
  | 4 -> Drop_Hole
  | 5 -> Explosion_Switch
  | 6 -> Fan_Switch
  | 7 -> Glue_Switch
  | 8 -> Grimer_Switch
  | 9 -> Imprison_Switch
  | 10 -> Mud_Switch
  | 11 -> Poison_Sting_Switch
  | 12 -> Pokemon_Switch
  | 13 -> Self_Destruct_Switch
  | 14 -> Skill_Drop_Switch
  | 15 -> Slowpoke_Switch
  | 16 -> Spin_Swith
  | 17 -> Summon_Switch
  | 18 -> Warp_Trap
  | _ -> failwith "Invalid trap_ground integer"