
let compute_termination_weight stop_prob = 
  1.0 /. (1.0 -. stop_prob)

(**Simple diffuse Lambertian accumulator*)
(**Stop prob = depth limiter*)
let simple_lambertian_accumulate ray scene (stop_prob:float) gen =
  let rec simple_lambertian_accumulate_rec ray scene gen color_multiplier accum_color = 
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let scene_hit_info = World.Scene.ray_intersects_scene ray scene in
  if scene_hit_info.hit = false then
    (accum_color |+| (color_multiplier |*| scene.background_color), gen)
  else
    let material = List.nth scene.materials scene_hit_info.material_index in
    let base_color = material.base_color in
    let emissive = material.emissive in
    let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
    let new_accum_color = accum_color |+| (color_multiplier |*| emissive) in
    if stop then
      (new_accum_color, gen2)
    else
      let (reflect_sample, gen3) = Math.RandomVector.random_unit_vector_in_hemisphere gen2 scene_hit_info.normal in
      let terminate_weight = compute_termination_weight stop_prob in
      (**both the normal and sampled ray are normalized already, so can dot to get the cos theta*)
      let cos_theta = Float.max (Math.Vector.dot scene_hit_info.normal reflect_sample) 0.0 in
      let weight = 2.0 *. terminate_weight *. cos_theta in
      let reflect_ray = {
        origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal);
        direction = reflect_sample;
        refractive_index = ray.refractive_index;
        inside = ray.inside;
      }
      in
      let new_color_multiplier = weight |*.| base_color |*| color_multiplier in
      simple_lambertian_accumulate_rec reflect_ray scene gen3 new_color_multiplier new_accum_color
  in
  simple_lambertian_accumulate_rec ray scene gen Math.Color.ones Math.Color.black

(**Lambertian accumulator with cosine sampling*)
(**Stop prob = depth limiter*)
let cos_lambertian_accumulate ray scene (stop_prob:float) gen =
  let rec cos_lambertian_accumulate_rec ray scene gen color_multiplier accum_color = 
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let scene_hit_info = World.Scene.ray_intersects_scene ray scene in
  if scene_hit_info.hit = false then
    (accum_color |+| (color_multiplier |*| scene.background_color), gen)
  else
    let material = List.nth scene.materials scene_hit_info.material_index in
    let base_color = material.base_color in
    let emissive = material.emissive in
    let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
    let new_accum_color = accum_color |+| (color_multiplier |*| emissive) in
    if stop then
      (new_accum_color, gen2)
    else
      let (reflect_sample, gen3) = Math.RandomVector.cos_sample_hemisphere gen2 scene_hit_info.normal in
      let terminate_weight = compute_termination_weight stop_prob in
      (**both the normal and sampled ray are normalized already, so can dot to get the cos theta*)
      let weight = terminate_weight in
      let reflect_ray = {
        origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal);
        direction = reflect_sample;
        refractive_index = ray.refractive_index;
        inside = ray.inside;
      }
      in
      let new_color_multiplier = weight |*.| base_color |*| color_multiplier in
      cos_lambertian_accumulate_rec reflect_ray scene gen3 new_color_multiplier new_accum_color
  in
  cos_lambertian_accumulate_rec ray scene gen Math.Color.ones Math.Color.black