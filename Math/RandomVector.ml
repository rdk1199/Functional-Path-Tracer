let random_unit_vector gen =
  let open Vector in
  let open Random in
  let (theta, gen2) = rand_float_in_range 0.0 (2.0 *. Float.pi) gen in
  let (z, gen3) = rand_float_in_range (-.1.0) 1.0 gen2 in
  let sqrt_one_minus_z_sq = Float.sqrt (1.0 -. z *. z) in
  let cos_theta = Float.cos theta in
  let sin_theta = Float.sin theta in
  ({x = sqrt_one_minus_z_sq *. cos_theta; y = sqrt_one_minus_z_sq *. sin_theta; z = z}, gen3)

(**rejection sampling of vector in hemisphere centered on v - v should not be zero (but need not be normalized)*)
(**50% chance of success*)
let rec random_unit_vector_in_hemisphere gen v =
  let (try_vec, gen2) = random_unit_vector gen in
  if Vector.dot v try_vec > 0.0 then
    (try_vec, gen2)
  else
    random_unit_vector_in_hemisphere gen2 v