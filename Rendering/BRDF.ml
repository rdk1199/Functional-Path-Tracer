
(**assumes all unit vectors*)
let normalized_half_vector v1 v2 =
  let open Math.Vector in
  let magnitude_of_sum = magnitude (v1 +| v2) in
  (1.0 /. magnitude_of_sum) *.| (v1 +| v2)

let ggx_normal_distribution half_vector normal alpha_squared =
  let open Math.Vector in
  let n_dot_half = dot normal half_vector in
  let denominator = Float.pi *. ((n_dot_half ** 2.0)*.(alpha_squared -. 1.0) +. 1.0) ** 2.0 in
  if denominator < Math.Util.epsilon then
    0.0
  else
    alpha_squared /. denominator


let ggx_geometric_term view_direction outgoing normal alpha_squared = 
  let open Math.Vector in
  let numerator = 2.0 *. (positive_dot normal view_direction) *. (positive_dot normal outgoing) in
    let denom_term v1 v2 = 
      (dot normal v1) *. (Float.sqrt (alpha_squared +. (1.0 -. alpha_squared) *. ((dot normal v2) ** 2.0)))
    in
  let denom = (denom_term view_direction outgoing) +. (denom_term outgoing view_direction) in
  if denom < Math.Util.epsilon then
    0.0
  else
    Float.min (numerator/.denom) 1.0


(**schlick approximation - assume vectors are unit*)
let ggx_fresnel_term fresnel_term direction half_vector =
  let open Math.Vector in
  let open Math.Color in
  let l_dot_h = dot direction half_vector in
  fresnel_term |+| (((1.0 -. l_dot_h) ** 5.0) |*.| (Math.Color.ones |-| fresnel_term)) 

let ggx_spec unit_view unit_light half_vector normal shininess fresnel  =
  let open Math.Vector in
  let open Math.Color in
  (**TODO: storing roughness instead of shininess directly saves a subtraction*)
  let alpha_squared = (1.0 -. shininess) ** 2.0 in
  let fresnel_term = ggx_fresnel_term fresnel unit_light half_vector in
  let geo_term = ggx_geometric_term unit_view unit_light normal alpha_squared in
  let normal_term = ggx_normal_distribution half_vector normal alpha_squared in
  let n_dot_view = positive_dot unit_view normal in
  let n_dot_light = positive_dot unit_light normal in
  let denom = 4.0 *. n_dot_view *. n_dot_light in
  if denom < Math.Util.epsilon then
    Math.Color.black
  else
    let numerator = geo_term *. normal_term |*.| fresnel_term in
    (1.0 /. denom) |*.| numerator

let cook_torrance_brdf view_direction to_light normal material =
  let open Math.Vector in
  let open Math.Color in
  let open World.Material in
  let unit_view = normalized view_direction in
  let unit_light = normalized to_light in 
  let half_vector = normalized_half_vector unit_view unit_light in
  let ks = ggx_fresnel_term material.fresnel unit_view half_vector in
 
  let ggx_spec = ggx_spec unit_view unit_light half_vector normal material.shininess material.fresnel in
  let specular_term = ggx_spec  in

  let kd = (Math.Color.ones |-| ks) in
  let diffuse_term = (1.0/.Float.pi) |*.| material.base_color |*| kd in

  if color_is_nan diffuse_term then
    print_endline "diffuse_term is NaN!";
  
  if color_is_nan specular_term then
    print_endline "specular_term is NaN!";

  diffuse_term |+| specular_term 

let rec ggx_sample_hemisphere gen normal view_dir alpha_squared =

  let open Math.Vector in
  let (r1, gen2) = Math.Random.rand_0_1_float gen in
  let (r2, gen3) = Math.Random.rand_0_1_float gen2 in
  let phi = 2.0 *. Float.pi *. r2 in
  let radicand_denom = r1 *. (alpha_squared -. 1.0) +. 1.0 in

  if radicand_denom < Math.Util.epsilon then
    (**reject problematic samples*)
    ggx_sample_hemisphere gen3 normal view_dir alpha_squared
  else
    let radicand = (1.0 -. r1) /. radicand_denom in
    let cos_theta = Float.sqrt radicand in
    let theta = Float.acos cos_theta in
    let sin_theta = Float.sin theta in
    let initial_half_vec = {
      x = (Float.cos phi) *. sin_theta;
      y = (Float.sin phi) *. sin_theta;
      z = cos_theta;
    }

  in
  let half_vec = Math.Transform.tangent_to_world_space initial_half_vec normal in
  let final_vec = Math.Geometry.reflect (~-|view_dir) half_vec in

if is_nan_vector final_vec then
  print_endline "NaN GGX sample generated!";
(final_vec, gen3)

let cook_torrance_sample gen unit_view normal alpha_squared =
  let specular_sample, gen2 = Math.Random.rand_flip (1.0 -. alpha_squared) gen in
  if specular_sample = true then
    ggx_sample_hemisphere gen2 normal unit_view alpha_squared
  else
    Math.RandomVector.cos_sample_hemisphere gen2 normal

(**given vector, determine the probability that the ggx sampler would have picked it*)
let ggx_sample_prob vector view_direction alpha_squared normal =
  let open Math.Vector in
  let unit_view = normalized view_direction in
  let unit_vector = normalized vector in
  let half_vector = normalized_half_vector unit_view unit_vector in

  let cos_theta_m = positive_dot normal half_vector in

  let numerator = alpha_squared *. cos_theta_m in
  let denom = 4.0 *. (positive_dot unit_vector half_vector) *. Float.pi *. ((alpha_squared -. 1.0) *. (cos_theta_m ** 2.0) +. 1.0) ** 2.0 in

  if denom < Math.Util.epsilon then 
    0.0
  else
  numerator/.denom


let cook_torrance_sample_prob vector view_dir alpha_squared normal =
  (1.0 -. alpha_squared) *. (ggx_sample_prob vector view_dir alpha_squared normal) +. alpha_squared *. (Math.RandomVector.cos_sample_prob vector normal)
