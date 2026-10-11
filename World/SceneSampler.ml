(**convert from solid angle measure to area measure given the origin (point from where we're sampling),
 chosen point on the light, and light normal*)
let compute_geometric_term_for_light_sample scene origin light_point light_normal =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let origin_to_light = light_point -| origin in
  let light_to_origin = ~-|origin_to_light in
  let signed_light_normal = if dot origin_to_light light_normal > 0. then ~-| light_normal else light_normal in
  let cos_angle_between = Float.max 0.0 (cos_angle_between light_to_origin signed_light_normal) in
  let sq_distance = square_magnitude origin_to_light in
  let origin_to_light_ray = {
    origin = origin;
    direction = origin_to_light;
    refractive_index = 0.0;
    inside = false;
  }
  in
  let scene_hit = Scene.ray_intersects_scene origin_to_light_ray scene in
  if scene_hit.hit && square_magnitude (scene_hit.point -| origin) < (sq_distance -. Math.Util.epsilon) then
    (**no hit or hit occluded*)
    0.0, Math.Color.black
  else
     let material = Iarray.get scene.materials scene_hit.material_index in
    if sq_distance < Math.Util.epsilon then
      Float.max_float, material.emissive
    else
      ((cos_angle_between /. sq_distance), material.emissive)

(**for a given triangle light, return probability of hitting a particular point on that light i.e. 1/(num_lights * area)*)
(**triangle_index should be the index of an emissive triangle*)
let get_light_prob scene triangle_index =
  let num_lights = Iarray.length scene.Scene.emissive_triangles in
  let emissive_tri = Iarray.get scene.Scene.triangles triangle_index in
  1.0 /. ((float_of_int num_lights) *. emissive_tri.area)


(**uniformly sample a point on a light in the scene - returns the point as well as the light probability and 
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
  let geo_term, emissive = compute_geometric_term_for_light_sample scene origin point emissive_tri.normal in
  (point, (get_light_prob scene emissive_tri_index), geo_term, emissive, gen3)


(**given a ray, compute the probability that the uniform light sampler would pick the point it hits*)
(**i.e. will be 0 for no-hit or hit of non-emissive tri, get_light_prob tri for emissive tri*)
let get_light_prob_for_ray ray scene =
  let scene_hit = Scene.ray_intersects_scene ray scene in
  let num_lights = float_of_int (Iarray.length scene.Scene.emissive_triangles) in
  if not scene_hit.hit then
    0.0
  else
    let material = Iarray.get scene.materials scene_hit.material_index in
    if not (Math.Color.color_sq_magnitude material.emissive > Math.Util.epsilon) then
      0.0
    else
      let open Math.Vector in
      let triangle = scene_hit.triangle in
      let area_prob = 1.0 /. (num_lights *. triangle.area) in
      let sq_distance = Math.Vector.square_magnitude (scene_hit.point -| ray.origin) in
      let from_light_dir = Math.Vector.normalized (ray.origin -| scene_hit.point) in
      let cos_light_to_origin = Float.max 0.0 (Math.Vector.dot from_light_dir scene_hit.normal) in
      (area_prob *. sq_distance) /. cos_light_to_origin