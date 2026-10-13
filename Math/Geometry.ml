
(** reflect vector v at surface with UNIT normal n*)
let reflect v n = 
  let open Vector in
  v -| 2. *. (dot v n) *.| n

(** refract vector v at surface with UNIT normal n*)
(** old_n and new_n are old and new refractive indices respectively*)
let refract v n old_ior new_ior =
  let open Vector in
  let refract_perp = (old_ior /. new_ior) *.| (v -| ((dot v n) *.| n) ) in
  let refract_perp_sq_mag = square_magnitude refract_perp in
  let discriminant = 1.0 -. refract_perp_sq_mag in
  if discriminant >= 0.0 then
    let refract_parallel = -.(sqrt (1.0 -. refract_perp_sq_mag)) *.| n in
    refract_perp +| refract_parallel
  else
    (**total internal reflection*)
    reflect v n

let refract_ray ray normal new_ior = 
  let open Vector in
  let open Ray in
  let refract_direction = refract ray.direction normal ray.refractive_index new_ior in
  let total_internal_reflection = ((dot refract_direction normal) > 0.0) in
  let new_origin = ray.origin +|
    if total_internal_reflection then Util.epsilon *.| normal else (-.Util.epsilon)*.|normal 
  in
  {
    origin = new_origin;
    direction = normalized refract_direction;
    refractive_index = if total_internal_reflection then ray.refractive_index else new_ior;
    inside = if total_internal_reflection then ray.inside else (not ray.inside);
  }

let normalized_half_vector v1 v2 =
  let open Vector in
  let magnitude_of_sum = magnitude (v1 +| v2) in
  (1.0 /. magnitude_of_sum) *.| (v1 +| v2)

(**schlick approximation - assume vectors are unit*)
let fresnel_term fresnel_term direction half_vector =
  let open Vector in
  let open Color in
  let l_dot_h = positive_dot direction half_vector in
  fresnel_term |+| (((1.0 -. l_dot_h) ** 5.0) |*.| (Color.ones |-| fresnel_term)) 

