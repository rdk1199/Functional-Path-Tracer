type scene = {
  spheres : Math.Shape.sphere list;
  (** list of indices -> sphere_materials[i] = index of i'th sphere's material in list*)
  sphere_materials : int list;
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

(**hit_record is the hit record for the CLOSEST hit we've had so far*)
let rec ray_intersects_scene_rec ray scene i hit_record =
  if i >= List.length scene.spheres then
    (**end of list - we're done*)
    hit_record
  else
    let sphere_hit_record = Math.Intersection.ray_intersects_sphere ray (List.nth scene.spheres i) in
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
      ray_intersects_scene_rec ray scene (i+1) new_scene_hit_record
    else
      (**no hit or object is farther from object we already hit*)
      ray_intersects_scene_rec ray scene (i+1) hit_record


let ray_intersects_scene ray scene = 
  let open Math.Vector in
  let default_scene_hit = {hit = false; 
                           t = 0.0; 
                           point = {x = 0.0; y = 0.0; z=0.0}; 
                           normal = {x = 0.0; y =0.0; z=0.0}; 
                           material_index = 0} in
  ray_intersects_scene_rec ray scene 0 default_scene_hit
