(**convert from solid angle measure to area measure given the origin (point from where we're sampling),
 chosen point on the light, and light normal*)
let compute_geometric_term_for_light_sample scene origin_to_light_ray light_normal =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let to_origin = ~-|(origin_to_light_ray.direction) in
  let signed_light_normal = if dot origin_to_light_ray.direction light_normal > 0. then ~-| light_normal else light_normal in
  let cos_angle_between = cos_angle_between to_origin signed_light_normal in
  let sq_distance = square_magnitude to_origin in
  let scene_hit = Scene.ray_intersects_scene origin_to_light_ray scene in
  if scene_hit.hit && square_magnitude (scene_hit.point -| origin_to_light_ray.origin) < (sq_distance -. Math.Util.epsilon) then
    (**no hit or hit occluded*)
    Math.Color.black
  else
    if sq_distance < Math.Util.epsilon then
      {Math.Color.r = Float.max_float; Math.Color.g = Float.max_float; Math.Color.b = Float.max_float;}
    else
      let material = Iarray.get scene.materials scene_hit.material_index in
      (cos_angle_between /. sq_distance) |*.| material.emissive


(**uniformly sample a point on a light in the scene - returns the point as well as the sample weight
 (num_lights * area of selected light)*)
 (**origin = point from where we are sampling*)
(**for our scenes, emissive triangles are the only lights*)
let uniform_sample_light scene origin gen =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let num_lights = Iarray.length scene.Scene.emissive_triangles in
  let (sample_index, gen2) = Math.Random.rand_int_in_range 0 (num_lights-1) gen in
  let emissive_tri_index = Iarray.get scene.Scene.emissive_triangles sample_index in
  let emissive_tri = Iarray.get scene.Scene.triangles emissive_tri_index in
  let (point, gen3) = (Math.Sampler.uniform_sample_triangle emissive_tri gen2) in
  let origin_to_light_ray = {origin = origin; direction = point -| origin; refractive_index = 1.0; inside = false;} in
  let geo_term = compute_geometric_term_for_light_sample scene origin_to_light_ray emissive_tri.normal in
  (point, (float_of_int num_lights) *. emissive_tri.area |*.| geo_term, gen3)
