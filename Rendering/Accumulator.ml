
(**Simple diffuse Lambertian accumulator*)
(**Stop prob = depth limiter*)
let rec simple_lambertian_accumulate ray scene (stop_prob:float) gen =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let scene_hit_info = World.Scene.ray_intersects_scene ray scene in
  if scene_hit_info.hit = false then
    (scene.background_color, gen)
  else
    let material = List.nth scene.materials scene_hit_info.material_index in
    let base_color = material.base_color in
    let emissive = material.emissive in
    let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
    if stop then
      (emissive, gen2)
    else
      let (reflect_sample, gen3) = Math.RandomVector.random_unit_vector_in_hemisphere gen2 scene_hit_info.normal in
      let terminate_weight = (1.0 /. (1.0 -. stop_prob)) in
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
      let (reflect_color, final_gen) = simple_lambertian_accumulate reflect_ray scene stop_prob gen3 in
      let final_color = weight |*.| base_color |*| reflect_color in
      (**TODO: it's trickier but tail recursion could be used here*)
      (emissive |+| final_color, final_gen)