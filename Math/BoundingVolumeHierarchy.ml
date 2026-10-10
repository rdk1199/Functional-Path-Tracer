(**acceleration structure for quickly testing ray intersection with a lot of primitives*)
(**since we're pretty much only going to be using triangles, just support those for now*)
(**This is basically a tree, where the leaves are triangles and the interior nodes
   are AABBs which contain their children*)
(**Leaf nodes also contain the int material index of that triangle*)
(**The implementation here will be fairly simple, but should get the job done*)

type bvh =
  | Node of bvh list * Shape.aabb
  | Leaf of Shape.triangle * int

let bvh_from_triangle triangle material_index =
  Leaf (triangle, material_index)

let aabb_of_bvh bvh =
  match bvh with
  | Leaf (triangle, _) -> Shape.aabb_of_triangle triangle
  | Node (bvhs, aabb) -> aabb
  
let centroid_of_bvh bvh = Shape.centroid_of_aabb (aabb_of_bvh bvh)

(**todo: we have quite a few hit_info structures that could probably be combined*)
type bvh_hit_info = {
  hit : bool;
  t : float;
  point : Vector.vector3;
  normal : Vector.vector3;
  material_index : int;
  triangle : Shape.triangle;
}

let rec ray_intersects_bvh_rec ray bvh bvh_hit_info : bvh_hit_info = 
  match bvh with 
  | Leaf (triangle, material_index) ->
    let hit_info = Intersection.ray_intersects_triangle ray triangle in
    {
      hit = hit_info.hit;
      t = hit_info.t;
      point = hit_info.point;
      normal = hit_info.normal;
      material_index = material_index;
      triangle = triangle;
    }
    
  | Node (bvhs, aabb) ->
    let aabb_hit_info = Intersection.ray_intersects_aabb ray aabb in
    if aabb_hit_info.hit && (bvh_hit_info.hit = false || aabb_hit_info.t < bvh_hit_info.t) then
      (**ray hit the bounding box in a closer spot than current hit*)
      let bvhs_hit_info = ray_intersects_bvhs ray bvhs bvh_hit_info in
      if bvhs_hit_info.hit && (bvh_hit_info.hit = false || bvhs_hit_info.t < bvh_hit_info.t) then
        (**ray hit something closer in the bounding box*)
        bvhs_hit_info
      else
        (**ray did not hit anything closer*)
        bvh_hit_info 
    else
      (**ray did not hit bounding box in a closer spot; keep current hit*)
      bvh_hit_info

(**hit_info is the closest hit we've had so far, or null_hit*)
and ray_intersects_bvhs ray bvhs bvh_hit_info =
  match bvhs with
  | [] -> bvh_hit_info
  | bvh :: tail ->
    let new_hit_info = ray_intersects_bvh_rec ray bvh bvh_hit_info in
      if new_hit_info.hit && (bvh_hit_info.hit = false || new_hit_info.t < bvh_hit_info.t) then
        (**closer hit - new hit info*)
        ray_intersects_bvhs ray tail new_hit_info
      else
        (**no hit*)
        ray_intersects_bvhs ray tail bvh_hit_info

let ray_intersects_bvh ray bvh =
  let null_bvh_hit = {
    hit = false;
    t = 0.0;
    point = Vector.zero_vec;
    normal = Vector.zero_vec;
    material_index = -1;
    triangle = Shape.null_triangle;
  }
  in
  ray_intersects_bvh_rec ray bvh null_bvh_hit

let aabb_of_bvhs bvh1 bvh2 =
  Shape.combine_aabbs (aabb_of_bvh bvh1) (aabb_of_bvh bvh2)

let create_bvh_from_two bvh1 bvh2 = 
  Node ([bvh1; bvh2], aabb_of_bvhs bvh1 bvh2)

(**removes the first two elements of bvh_list and replaces them with the one combined bvh*)
let combine_first_two bvh_list = 
  match bvh_list with
  | bvh1 :: bvh2 :: tail ->
    let new_bvh = create_bvh_from_two bvh1 bvh2 in
    new_bvh::tail
  | _ -> assert false

(**continually combines bvhs in the list until there is one root bvh left*)
let rec combine_bvhs bvh_list =
  if List.length bvh_list <= 1 then
    List.hd bvh_list
  else
    (** sort bvhs by centroid in x, y, z planes*)
    let x_sorted_bvh_list = List.sort (fun bvh1 bvh2 -> compare (centroid_of_bvh bvh1).x (centroid_of_bvh bvh2).x ) bvh_list in
    let y_sorted_bvh_list = List.sort (fun bvh1 bvh2 -> compare (centroid_of_bvh bvh1).y (centroid_of_bvh bvh2).y ) bvh_list in
    let z_sorted_bvh_list = List.sort (fun bvh1 bvh2 -> compare (centroid_of_bvh bvh1).z (centroid_of_bvh bvh2).z ) bvh_list in

    (**very lame attempt at surface area heuristic - just take lowest two of each, 
       merge the ones which form the lowest surface area AABB*)
    let x_aabb_sa = Shape.aabb_surface_area (aabb_of_bvhs (List.nth x_sorted_bvh_list 0) (List.nth x_sorted_bvh_list 1)) in
    let y_aabb_sa = Shape.aabb_surface_area (aabb_of_bvhs (List.nth y_sorted_bvh_list 0) (List.nth y_sorted_bvh_list 1)) in
    let z_aabb_sa = Shape.aabb_surface_area (aabb_of_bvhs (List.nth z_sorted_bvh_list 0) (List.nth z_sorted_bvh_list 1)) in

    if x_aabb_sa <= y_aabb_sa && x_aabb_sa <= z_aabb_sa then
      combine_bvhs (combine_first_two x_sorted_bvh_list)
    else if y_aabb_sa <= z_aabb_sa then
      combine_bvhs (combine_first_two y_sorted_bvh_list)
    else
      combine_bvhs (combine_first_two z_sorted_bvh_list)

(**build bvh from list of triangles*)
let build_bvh triangle_list material_indices = 
  print_endline "starting BVH generation";
  let triangle_and_material_list = List.combine triangle_list material_indices in
  let bvh = combine_bvhs (List.map (fun (tri, material) -> (bvh_from_triangle tri material)) triangle_and_material_list) in
  print_endline "finished BVH generation";
  bvh
