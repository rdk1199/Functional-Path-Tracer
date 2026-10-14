
  (**assume unit vectors*)
let refractive_btdf material new_ior old_ior normal transmit_dir view_dir =
  let open Math.Vector in
  let open Math.Color in
  let open World.Material in
  (**assume the transmit direction is the perfect refractive direction*)
  (**normal is the incident normal (i.e. faces away from the transmit_dir)*)
  (**for our renderer, the transmissiveness is always (1-opacity) * base_color*)
  let radiance_scaling_factor = (new_ior ** 2.0) /. (old_ior ** 2.0) in
  let transmissivity = (1.0 -. material.opacity) |*.| Math.Color.ones in
  let denom = positive_dot (~-|normal) transmit_dir in
  let half_vector = Math.Geometry.normalized_half_vector (old_ior *.| view_dir) (new_ior *.| transmit_dir) in
  let _fresnel_term = Math.Geometry.fresnel_term material.fresnel view_dir half_vector in
  if denom < Math.Util.epsilon then
    Math.Color.black
  else
    (1.0/.denom) *. radiance_scaling_factor |*.| transmissivity

