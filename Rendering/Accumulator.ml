
let compute_termination_weight stop_prob = 
  1.0 /. (1.0 -. stop_prob)

(**compute possibility of a transmission ray instead of reflection ray, based on opacity*)
let transmit_prob opacity = 
  (1.0 -. opacity)

(**Cook-Torrance GGX accumulator with multiple importance sampling of lights + BSDF and perfect refractions*)
(**TODO: This is pretty messy*)
let ggx_accumulate ray scene (stop_prob:float) gen =
(**also carry a flag to determine whether this is an initial ray or reflected ray*)
let rec ggx_accumulate_rec ray scene (stop_prob:float) gen accum_color multiplier =
  let open Math.Vector in
  let open Math.Ray in
  let open Math.Color in
  let scene_hit_info = World.Scene.ray_intersects_scene ray scene in
  if scene_hit_info.hit = false then
    (accum_color |+| (multiplier |*| scene.background_color), gen)
  else
    let material = Iarray.get scene.materials scene_hit_info.material_index in
    let emissive = material.emissive in
    let view_dir = ~-|(normalized ray.direction) in
    let alpha_squared = material.roughness ** 2.0 in
    let opacity = material.opacity in
    
    let (stop, gen2) = Math.Random.rand_flip stop_prob gen in
    if stop then
      (accum_color |+| (multiplier |*| emissive), gen2)
    else
      let terminate_weight = compute_termination_weight stop_prob in
      (**light sample*)
      let new_ray_origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal) in

      (**sample a point on a light*)
      let (light_point, light_area_prob, geo_term, light_emissive, gen3) = World.SceneSampler.uniform_sample_light scene new_ray_origin gen2 in
      let to_light_dir = normalized  (light_point -| scene_hit_info.point) in
      let direct_cos_theta = Float.max (Math.Vector.dot scene_hit_info.normal to_light_dir) 0.0 in

      (**multiply by probability we reflect as opposed to refract*)
      let cook_torrance_light_sample_prob = (1.0 -. (transmit_prob opacity)) *. BRDF.cook_torrance_sample_prob to_light_dir view_dir alpha_squared scene_hit_info.normal in

      let mis_light_weight = 
        if (light_area_prob +. (cook_torrance_light_sample_prob *. geo_term)) > Math.Util.epsilon then
          light_area_prob /. (light_area_prob +. (cook_torrance_light_sample_prob *. geo_term))
        else
          0.0
      in

      let light_cook_torrance_brdf = BRDF.cook_torrance_brdf view_dir to_light_dir scene_hit_info.normal material in
      let direct_contribution = (1.0/.light_area_prob) *. terminate_weight  *. direct_cos_theta *. mis_light_weight *. geo_term |*.| light_emissive |*| light_cook_torrance_brdf in 

      let accum_color_with_direct = accum_color |+| (multiplier |*| (emissive |+| direct_contribution)) in

      let transmit_prob = transmit_prob opacity in
      let reflect_prob = 1.0 -. transmit_prob in

      let choose_reflect, gen4 = Math.Random.rand_flip reflect_prob gen3 in

      if choose_reflect then
        (** reflect *)
        let (reflect_dir, final_gen) = BRDF.cook_torrance_sample gen4 view_dir scene_hit_info.normal alpha_squared in

        (**both the normal and sampled ray are normalized already, so can dot to get the cos theta*)
        let reflect_ray = {
          origin = scene_hit_info.point +| (Math.Util.epsilon *.| scene_hit_info.normal);
          direction = reflect_dir;
          refractive_index = ray.refractive_index;
          inside = ray.inside;
        }
        in
        (**compute MIS balance heuristic probability weights*)
        let reflect_cook_torrance_sample_prob = BRDF.cook_torrance_sample_prob reflect_dir view_dir alpha_squared scene_hit_info.normal in
        let light_sample_prob = World.SceneSampler.get_light_prob_for_ray reflect_ray scene in

        let mis_cos_sample_weight = 
          if reflect_cook_torrance_sample_prob +. light_sample_prob > Math.Util.epsilon then
            reflect_cook_torrance_sample_prob /. (reflect_cook_torrance_sample_prob +. light_sample_prob)
          else
            0.0
        in

        let indirect_cos_theta = Math.Vector.positive_dot reflect_dir scene_hit_info.normal in
        let reflect_cook_torrance_brdf = BRDF.cook_torrance_brdf view_dir reflect_dir scene_hit_info.normal material in
        let adjusted_reflect_ct_sample_prob = 
          if reflect_cook_torrance_sample_prob < Math.Util.epsilon then
            Math.Util.epsilon
          else
            reflect_cook_torrance_sample_prob
        in

        let new_multiplier = (1.0/.reflect_prob) *. (1.0 /. adjusted_reflect_ct_sample_prob) *. terminate_weight *. indirect_cos_theta *. mis_cos_sample_weight |*.| reflect_cook_torrance_brdf |*| multiplier in

        ggx_accumulate_rec reflect_ray scene stop_prob final_gen accum_color_with_direct new_multiplier
      else
        (**refract*)
        let refract_ray = Math.Geometry.refract_ray ray scene_hit_info.point scene_hit_info.normal (if ray.inside then  1.0 else material.refractive_index) in
        let total_internal_reflect = (Math.Vector.dot refract_ray.direction scene_hit_info.normal > 0.0) in

        (**since we're perfectly refracting, the mis weight is just 1.0*)
  
        let indirect_cos_theta = if total_internal_reflect then Math.Vector.dot refract_ray.direction scene_hit_info.normal else Math.Vector.dot refract_ray.direction ~-|(scene_hit_info.normal) in
        let refract_btdf = BTDF.refractive_btdf material refract_ray.refractive_index ray.refractive_index scene_hit_info.normal refract_ray.direction view_dir in

        let new_multiplier = (1.0 /. transmit_prob) *. terminate_weight *. indirect_cos_theta |*.| refract_btdf |*| multiplier in
        ggx_accumulate_rec refract_ray scene stop_prob gen4 accum_color_with_direct new_multiplier
  
in  ggx_accumulate_rec ray scene stop_prob gen Math.Color.black Math.Color.ones
