
(**current_material_name_list head is last material we encountered*)
(**current material list is list of materials, where head is the material currently being constructed*)
let parse_mtl_line line current_material_name_list current_material_list =
  let open World.Material in
  if String.length line < 3 then
    (current_material_name_list, current_material_list)
  else if String.sub line 0 3 = "Kd " then
    (**Base color*)
    let base_color = Scanf.sscanf line "Kd %f %f %f" ( fun r g b ->
      ({r = 255.0 *. r; g = 255.0 *. g; b = 255.0 *. b;} : Math.Color.color)
    )
    in
    let new_material = {(List.hd current_material_list) with base_color = base_color} in
    (current_material_name_list, new_material :: (List.tl current_material_list))
  else if String.sub line 0 3 = "Ns " then
    (**Specular exponent - convert to shininess*)
    let spec_exp = Scanf.sscanf line "Ns %f" ( fun exp -> exp ) in
    let shininess = World.Material.convert_specular_exponent_to_shininess spec_exp in
    let new_material = {(List.hd current_material_list) with shininess = shininess} in
    (current_material_name_list, new_material :: (List.tl current_material_list))
  else if String.sub line 0 2 = "d " then
    (**Opacity - straight copy*)
    let opacity = Scanf.sscanf line ("d %f") (fun opacity -> opacity) in
    let new_material = {(List.hd current_material_list) with opacity = opacity} in
    (current_material_name_list, new_material :: (List.tl current_material_list))
  else if String.sub line 0 3 = "Ke " then
    (**Emissive - straight copy*)
    let emissive_color = Scanf.sscanf line "Ke %f %f %f" (fun r g b ->
      ({r = r; g = g; b = b;} : Math.Color.color)
    )
    in
    let new_material = {(List.hd current_material_list) with emissive = emissive_color} in
    (current_material_name_list, new_material :: (List.tl current_material_list))
  else if String.sub line 0 3 = "Ni " then
    (**index of refraction - straight copy*)
    let ior = Scanf.sscanf line ("Ni %f") (fun ior -> ior) in
    let new_material = {(List.hd current_material_list) with refractive_index = ior} in
    (current_material_name_list, new_material :: (List.tl current_material_list))
  else if String.length line > 6 && String.sub line 0 7 = "newmtl " then
    let new_mtl_name = Scanf.sscanf line "newmtl %s" (fun str -> str) in
    let new_material = {World.Material.default_material with name = new_mtl_name} in
    print_endline new_mtl_name;
    (new_mtl_name :: current_material_name_list, new_material :: current_material_list)
  else
    (**something else - ignore*)
    (current_material_name_list, current_material_list)

let parse_mtl_lines file_name = 
  In_channel.with_open_text file_name (
    fun ic ->
      let rec parse_lines name_list material_list =
        match In_channel.input_line ic with
        | Some line ->
          let new_name_list, new_material_list = 
            parse_mtl_line line name_list material_list
          in
          parse_lines new_name_list new_material_list
        | None -> (name_list, material_list)
      in
      parse_lines [] []
  )
  
    
(**given the material list parsed from the corresponding obj file, return a list of Materials
   which is the same order as the material list*)
let parse_mtl_file file_name obj_material_list = 
  let material_name_list, material_list = parse_mtl_lines file_name in
  (**Not the most efficient sort, but there won't be that many materials*)
  List.sort (fun mat1 mat2 -> 
    compare 
    (List.find_index (fun name -> name = mat1.World.Material.name) material_name_list)
    (List.find_index (fun name -> name = mat2.World.Material.name) material_name_list)
  ) material_list
