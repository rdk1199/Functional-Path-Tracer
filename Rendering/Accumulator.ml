
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
    let material = Iarray.get scene.materials scene_hit_info.material_index in
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
    let material = Iarray.get scene.materials scene_hit_info.material_index in
    let base_color = material.base_color in
    let emissive = material.emissive in
    let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
    let new_accum_color = accum_color |+| (color_multiplier |*| emissive) in
    if stop then
      (new_accum_color, gen2)
    else
      let (reflect_sample, gen3) = Math.RandomVector.cos_sample_hemisphere gen2 scene_hit_info.normal in
      let terminate_weight = compute_termination_weight stop_prob in
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

(**Lambertian accumulator with multiple importance sampling of lights + BSDF*)
let mis_diffuse_accumulate ray scene (stop_prob:float) gen =
(**also carry a flag to determine whether this is an initial ray or reflected ray*)
let rec mis_diffuse_accumulate_rec ray scene (stop_prob:float) gen =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let scene_hit_info = World.Scene.ray_intersects_scene ray scene in
  if scene_hit_info.hit = false then
    (scene.background_color, gen)
  else
    let material = Iarray.get scene.materials scene_hit_info.material_index in
    let base_color = material.base_color in
    let emissive = material.emissive in
    
    let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
    if stop then
      (emissive, gen2)
    else
      let terminate_weight = compute_termination_weight stop_prob in
      (**light sample*)
      let new_ray_origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal) in

      (**sample a point on a light*)
      let (light_point, light_prob, geo_term, gen3) = World.SceneSampler.uniform_sample_light scene new_ray_origin gen2 in
      let to_light_dir = normalized  (light_point -| scene_hit_info.point) in
      let direct_cos_theta = Float.max (Math.Vector.dot scene_hit_info.normal to_light_dir) 0.0 in
      let cos_sample_prob = Math.RandomVector.cos_sample_prob to_light_dir scene_hit_info.normal in
      let mis_light_weight = light_prob /. (light_prob +. cos_sample_prob) in
      let lambertian_brdf = (1.0 /. Float.pi) |*.| base_color in

      let direct_contribution = (1.0/.light_prob) *. terminate_weight  *. direct_cos_theta *. mis_light_weight |*.| geo_term |*| lambertian_brdf in 

      let (reflect_dir, gen4) = Math.RandomVector.cos_sample_hemisphere gen3 scene_hit_info.normal in   

      (**both the normal and sampled ray are normalized already, so can dot to get the cos theta*)
      let reflect_ray = {
        origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal);
        direction = reflect_dir;
        refractive_index = ray.refractive_index;
        inside = ray.inside;
      }
      in
      (**compute MIS balance heuristic probability weights*)
      let reflect_cos_sample_prob = Math.RandomVector.cos_sample_prob reflect_dir scene_hit_info.normal in
      let light_sample_prob = World.SceneSampler.get_light_prob_for_ray reflect_ray scene in
      let mis_cos_sample_weight = reflect_cos_sample_prob /. (reflect_cos_sample_prob +. light_sample_prob) in

      let (reflect_color, final_gen) = mis_diffuse_accumulate_rec reflect_ray scene stop_prob gen4 in
      let indirect_cos_theta = Float.max 0.0 (Math.Vector.dot reflect_dir scene_hit_info.normal) in

      let indirect_contribution = (1.0 /. reflect_cos_sample_prob) *. terminate_weight *. indirect_cos_theta *. mis_cos_sample_weight |*.| lambertian_brdf |*| reflect_color in
      (emissive |+| direct_contribution |+| indirect_contribution, final_gen)
in  mis_diffuse_accumulate_rec ray scene stop_prob gen

(*
(**Lambertian accumulator with multiple importance sampling of lights + BSDF*)
let mis_ggx_accumulate ray scene (stop_prob:float) gen =
(**also carry a flag to determine whether this is an initial ray or reflected ray*)
let rec mis_ggx_accumulate_rec ray scene (stop_prob:float) gen initial =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let scene_hit_info = World.Scene.ray_intersects_scene ray scene in
  if scene_hit_info.hit = false then
    (scene.background_color, gen)
  else
    let material = Iarray.get scene.materials scene_hit_info.material_index in
    let base_color = material.base_color in
    let emissive = material.emissive in
    if color_sq_magnitude emissive > Math.Util.epsilon && not initial then
      (Math.Color.black, gen)
    else
      let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
      if stop then
        (emissive, gen2)
      else
        let terminate_weight = compute_termination_weight stop_prob in
        (**light sample*)
        let new_ray_origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal) in

        (**sample a point on a light*)
        let (point, light_weight, gen3) = World.SceneSampler.uniform_sample_light scene new_ray_origin gen2 in
        let cos_theta = Float.max (Math.Vector.dot scene_hit_info.normal (normalized (point -| scene_hit_info.point))) 0.0 in

        let to_light = point -| scene_hit_info.point in
        let to_view = ~-|(ray.direction) in

        let ggx_brdf_weight_1 = BRDF.cook_torrance_brdf to_view to_light scene_hit_info.normal material in

        (**multiply by 0.5 for MIS*)
        let sample_weight =  0.5 *. cos_theta *. terminate_weight |*.| ggx_brdf_weight_1 |*| light_weight in
        let sampled_light_color = sample_weight |*| base_color in    


        let (reflect_sample, gen4) = Math.RandomVector.cos_sample_hemisphere gen3 scene_hit_info.normal in   
        (**both the normal and sampled ray are normalized already, so can dot to get the cos theta*)
        (**multiply by 0.5 for MIS*)
        let weight = 0.5 *. terminate_weight in
        let reflect_ray = {
          origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal);
          direction = reflect_sample;
          refractive_index = ray.refractive_index;
          inside = ray.inside;
        }
        in
        let ggx_brdf_weight_2 = BRDF.cook_torrance_brdf to_view reflect_sample scene_hit_info.normal material in
        let (reflect_color, final_gen) = mis_ggx_accumulate_rec reflect_ray scene stop_prob gen4 false in
        let final_color = weight |*.| ggx_brdf_weight_2 |*| base_color |*| reflect_color in
        (**TODO: it's trickier but tail recursion could be used here*)
        (emissive |+| sampled_light_color |+| final_color, final_gen)
in  mis_ggx_accumulate_rec ray scene stop_prob gen true*)