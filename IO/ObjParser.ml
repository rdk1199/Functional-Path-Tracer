type int_tuple = {
  i1 : int;
  i2 : int;
  i3 : int;
}

(**parse the line and append the vertex to the list*)
let parse_vertex_line line vertex_list =
  Scanf.sscanf line "v %f %f %f" ( fun x y z ->
    {Math.Vector.x = x; Math.Vector.y = y; Math.Vector.z = z;} :: vertex_list
  )

let parse_face_line line face_list face_material_indices current_material_index =
  Scanf.sscanf line "f %d %d %d" ( fun i1 i2 i3 ->
    (**face vertex indices are 1-indexed for .obj, so subtract 1*)
    let new_face_list = {i1 = i1-1; i2 = i2-1; i3 = i3-1} :: face_list in
    let material_index = 
      match current_material_index with
      | [] -> -1
      | head :: tail -> head
    in
    let new_face_material_indices = material_index :: face_material_indices in
    new_face_list, new_face_material_indices
  )

(**parse line for a material - if the material is new -> append to old list*)
let parse_material_line line material_list current_material_index = 
  Scanf.sscanf line "usemtl %s" (fun name ->
  match List.find_index (fun mat -> mat = name) material_list with
  | Some index ->
    (material_list, index::current_material_index)
  | None ->
    (**append materials to the material list so that indices stay consistent*)
    (**this has a cost but the hope is that the material list is relatively small*)
    let new_material_list = material_list @ [name] in
    (new_material_list, ((List.length new_material_list) - 1)::current_material_index)
)

let parse_line line vertex_list material_list face_list face_material_indices current_material_index =
  if String.length line < 2 then
    (vertex_list, material_list, face_list, face_material_indices, current_material_index)
  else if String.sub line 0 2 = "v " then
    let new_vertex_list = 
    parse_vertex_line line vertex_list in
    (new_vertex_list, material_list, face_list, face_material_indices, current_material_index)
  else if String.sub line 0 2 = "f " then
    let new_face_list, new_face_material_indices = 
    parse_face_line line face_list face_material_indices current_material_index in
    (vertex_list, material_list, new_face_list, new_face_material_indices, current_material_index)
  else if String.length line >= 6 && String.sub line 0 6 = "usemtl" then
    let new_material_list, new_current_material_index = 
    parse_material_line line material_list current_material_index in
    (vertex_list, new_material_list, face_list, face_material_indices, new_current_material_index)
  else (**ignore other lines*)
    (vertex_list, material_list, face_list, face_material_indices, current_material_index)


(**gather list of vertices, materials, and face tuples*)
let parse file_name =
  let reversed_vertex_list, material_list, face_index_list, face_material_indices = 
  In_channel.with_open_text file_name (
    fun ic ->
      (**vertex list: list of vertices parsed from the file so far*)
      (**material list: list of material names gathered so far*)
      (**face list: list of face int tuples gathered so far*)
      (**face_material_indices: material index for each face gathered so far*)
      (**current_material_index: head = material specified in last usemtl directive*)
      let rec parse_lines vertex_list material_list face_list face_material_indices current_material_index =
        match In_channel.input_line ic with
        | Some line ->
          let new_vertex_list, new_material_list, new_face_list, new_face_material_indices, new_current_material_index =
            parse_line line vertex_list material_list face_list face_material_indices current_material_index
          in
          parse_lines new_vertex_list new_material_list new_face_list new_face_material_indices new_current_material_index
        | None -> (vertex_list, material_list, face_list, face_material_indices)
      in
      let vertex_list, material_list, face_list, face_material_indices, current_material_index 
      = [], [], [], [], [] in
      parse_lines vertex_list material_list face_list face_material_indices current_material_index
  ) in
  (**because we've been prepending to the list, but we need the vertices in the same order as in the
  (  file, reverse the list *)
  let vertex_list = List.rev reversed_vertex_list in
  (vertex_list, material_list, face_index_list, face_material_indices)

let get_triangle_list vertex_array face_index_array =
  let rec get_triangle_list_rec i vertex_array face_index_array triangle_list =
    if i >= Iarray.length face_index_array then
      triangle_list
    else
      let face_tuple = Iarray.get face_index_array i in
      let p1, p2, p3 = 
          Iarray.get vertex_array face_tuple.i1, 
          Iarray.get vertex_array face_tuple.i2, 
          Iarray.get vertex_array face_tuple.i3 
      in
      let new_triangle = {Math.Shape.p1 = p1; Math.Shape.p2 = p2; Math.Shape.p3 = p3} in
      get_triangle_list_rec (i+1) vertex_array face_index_array (new_triangle :: triangle_list)
  in
  (**since we are prepending new triangles onto the list as it's built, we must reverse at the end
     to preserve the indices*)
  List.rev (get_triangle_list_rec 0 vertex_array face_index_array [])

(**parse a Wavefront .obj model file and turn it into a list of triangles and their associated materials*)
let parse_obj file_name =
  let vertex_list, material_list, face_index_list, face_material_indices = parse file_name in
  let vertex_array = Iarray.of_list vertex_list in
  let face_index_array = Iarray.of_list face_index_list in

  print_endline ("Number of vertices: " ^ (string_of_int (Iarray.length vertex_array)) ^ "\n");
  print_endline ("Number of faces: " ^ (string_of_int (Iarray.length face_index_array)) ^ "\n");

  let triangle_list = get_triangle_list vertex_array face_index_array in
  (triangle_list, face_material_indices, material_list)

  
  

