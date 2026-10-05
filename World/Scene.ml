type scene = {
  spheres : Math.Shape.sphere list;
  (** list of indices -> sphere_materials[i] = index of i'th sphere's material in list*)
  sphere_materials : int list;

  triangles : Math.Shape.triangle list;
  triangle_materials : int list;

  bvh : Math.BoundingVolumeHierarchy.bvh;

  lights : Light.point_light list;
  materials : Material.material list;

  background_color : Math.Color.color;
  ambient_color : Math.Color.color;
}

(**record for information about where a ray hits a scene*)
type scene_hit_record = {
  hit : bool;
  t : float;
  point : Math.Vector.vector3;
  normal : Math.Vector.vector3;
  material_index : int;
}

(**default scene hit record (no hit)*)
let default_scene_hit = {
  hit = false; 
  t = 0.0; 
  point = {x = 0.0; y = 0.0; z=0.0}; 
  normal = {x = 0.0; y =0.0; z=0.0}; 
  material_index = 0
} 

(**hit_record is the hit record for the CLOSEST hit we've had so far*)
(** i is the current index of where we are in the list, for matching spheres to their materials*)
let rec ray_intersects_scene_spheres_rec ray scene sphere_list hit_record i =
  match sphere_list with
    (**end of list - we're done*)
  | [] -> hit_record
  | sphere :: tail ->
    let sphere_hit_record = Math.Intersection.ray_intersects_sphere ray sphere in
    if sphere_hit_record.hit && (sphere_hit_record.t < hit_record.t || hit_record.hit = false) then
      (**hit a closer object*)
      let new_scene_hit_record = 
      {
        hit = true; 
        t = sphere_hit_record.t; 
        point = sphere_hit_record.point;
        normal = sphere_hit_record.normal;
        material_index = List.nth scene.sphere_materials i;
      } in
      ray_intersects_scene_spheres_rec ray scene tail new_scene_hit_record (i+1)
    else
      (**no hit or object is farther from object we already hit*)
      ray_intersects_scene_spheres_rec ray scene tail hit_record (i+1)

(**hit_record is the hit record for the CLOSEST hit we've had so far*)
(** i is the current index of where we are in the list, for matching triangles to their materials*)
let rec ray_intersects_scene_triangles_rec ray scene triangle_list hit_record i =
  match triangle_list with
    (**end of list - we're done*)
  | [] -> hit_record
  | triangle :: tail ->
    let triangle_hit_record = Math.Intersection.ray_intersects_triangle ray triangle in
    if triangle_hit_record.hit && (triangle_hit_record.t < hit_record.t || hit_record.hit = false) then
      (**hit a closer object*)
      let new_scene_hit_record = 
      {
        hit = true; 
        t = triangle_hit_record.t; 
        point = triangle_hit_record.point;
        normal = triangle_hit_record.normal;
        material_index = List.nth scene.triangle_materials i;
      } in
      ray_intersects_scene_triangles_rec ray scene tail new_scene_hit_record (i+1)
    else
      (**no hit or object is farther from object we already hit*)
      ray_intersects_scene_triangles_rec ray scene tail hit_record (i+1)

let rec ray_intersects_scene_bvh ray scene =
  let bvh_hit_record = Math.BoundingVolumeHierarchy.ray_intersects_bvh ray scene.bvh in
  if bvh_hit_record.hit then
    {
      hit = true;
      t = bvh_hit_record.t;
      point = bvh_hit_record.point;
      normal = bvh_hit_record.normal;
      (**TODO: get material!*)
      material_index = bvh_hit_record.material_index;
    }
  else
    default_scene_hit

let ray_intersects_scene ray scene = 
  let open Math.Vector in

  let sphere_hit = ray_intersects_scene_spheres_rec ray scene scene.spheres default_scene_hit 0 in
  let bvh_hit = ray_intersects_scene_bvh ray scene in

  if not sphere_hit.hit then
    bvh_hit
  else if not bvh_hit.hit then 
    sphere_hit
  else
    if bvh_hit.t < sphere_hit.t then
      bvh_hit
    else
      sphere_hit


(**(**keep around for testing with/without acceleration structures*)
let ray_intersects_scene ray scene = 
  let open Math.Vector in
  let default_scene_hit = {hit = false; 
                           t = 0.0; 
                           point = {x = 0.0; y = 0.0; z=0.0}; 
                           normal = {x = 0.0; y =0.0; z=0.0}; 
                           material_index = 0} in
  let sphere_hit = ray_intersects_scene_spheres_rec ray scene scene.spheres default_scene_hit 0 in
  let triangle_hit = ray_intersects_scene_triangles_rec ray scene scene.triangles default_scene_hit 0 in

  if not sphere_hit.hit then
    triangle_hit
  else if not triangle_hit.hit then 
    sphere_hit
  else
    if triangle_hit.t < sphere_hit.t then
      triangle_hit
    else
      sphere_hit
*)