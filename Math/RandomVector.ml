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

(**randomly sample in hemisphere around normal, weighted by cosine*)
(**due to precision ranges, we may accidentally sample the edge of the 
hemisphere - resample in that case*)
let rec cos_sample_hemisphere gen normal =
  let open Vector in
  let open Random in
  let (phi, gen2) = rand_float_in_range 0.0 (2.0 *. Float.pi) gen in
  let (cos_theta, gen3) = (random_bind Float.sqrt)(rand_float_in_range 0.0 1.0 gen2) in
  let sin_theta = Float.sqrt (1.0 -. cos_theta *. cos_theta) in
  let initial_vec =
  {
    x = (Float.cos phi) *. sin_theta;
    y = (Float.sin phi) *. sin_theta;
    z = cos_theta;
  }
  in

  (**how much to rotate - assume normal is unit vector*)
  let rot = Float.acos (dot normal Vector.up) in
  let cross = cross_product Vector.up normal in
  let axis =
    if square_magnitude cross < Util.epsilon then
      {x=1.0; y=0.0; z=0.0}
    else
      normalized cross
  in
  (**Rodrigues rotation formula*)
  let try_vec = 
    ((Float.cos rot) *.| initial_vec) +| 
    ((Float.sin rot) *.| (cross_product axis initial_vec)) +|
    (((1.0 -. (Float.cos rot)) *. (dot axis initial_vec)) *.| axis)
  in
  if dot try_vec normal > Util.epsilon then
    (normalized try_vec, gen3)
  else
    let () = print_endline "looping" in
    cos_sample_hemisphere gen3 normal


(**probability that we would have chosen this direction with the cos sampler, for MIS weighting*)
(**assume unit direction vector*)
let cos_sample_prob direction normal =
  Float.max 0.0 (Vector.dot direction normal)