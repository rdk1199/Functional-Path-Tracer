
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
    Float.max_float
  else
    alpha_squared /. denominator


(**assume unit vectors*)
let ggx_geometric_term view_direction to_light normal alpha_squared = 
  let open Math.Vector in
  (**v is unit vector*)
  let ggx_smith_geometric_factor v normal alpha_squared =
    let n_dot_v = dot normal v in
    let radicand = alpha_squared +. (1.0 -. alpha_squared) *. n_dot_v *. n_dot_v in
    let numerator = 2.0 *. n_dot_v in
    let denominator = n_dot_v +. Float.sqrt radicand in
    numerator/.denominator
  in
  (ggx_smith_geometric_factor to_light normal alpha_squared) *. (ggx_smith_geometric_factor view_direction normal alpha_squared)

(**schlick approximation - assume vectors are unit*)
let ggx_fresnel_term fresnel_term direction half_vector =
  let open Math.Vector in
  let open Math.Color in
  let l_dot_h = dot direction half_vector in
  let l_dot_h_5 = l_dot_h ** 5.0 in
  fresnel_term |+| ((1.0 -. l_dot_h_5) |*.| (Math.Color.ones |-| fresnel_term)) 

let ggx_spec unit_view unit_light half_vector normal shininess fresnel  =
  let open Math.Vector in
  let open Math.Color in
  (**TODO: storing roughness instead of shininess directly saves a subtraction*)
  let alpha_squared = (1.0 -. shininess) ** 2.0 in
  let fresnel_term = ggx_fresnel_term fresnel unit_light half_vector in
  let geo_term = ggx_geometric_term unit_view unit_light normal alpha_squared in
  let normal_term = ggx_normal_distribution half_vector normal alpha_squared in
  let n_dot_view = dot unit_view normal in
  let n_dot_light = dot unit_light normal in
  let denom = 4.0 *. n_dot_view *. n_dot_light in
  if denom < Math.Util.epsilon then
    Math.Color.max_color
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
  let kd = (Math.Color.ones |-| ks) in
  let diffuse_term = (1.0/.Float.pi) |*.| material.base_color |*| kd in
  let ggx_spec = ggx_spec unit_view unit_light half_vector normal material.shininess material.fresnel in
  let specular_term = ggx_spec |*| ks in
  diffuse_term |+| specular_term 

