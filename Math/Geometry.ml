
(** reflect vector v at surface with UNIT normal n*)
let reflect v n = 
  let open Vector in
  v -| 2. *. (dot v n) *.| n

(** refract vector v at surface with UNIT normal n*)
(** old_n and new_n are old and new refractive indices respectively*)
let refract v n old_ior new_ior =
  let open Vector in
  let normalized_v = normalized v in

  (*
  print_endline ("direction:  " ^ string_of_vector3 normalized_v);
  print_endline ("normal:  " ^ string_of_vector3 n);
*)
  let eta = old_ior /. new_ior in

  let discriminant = 1.0 -. (eta ** 2.0) *. (1.0 -. ((dot n normalized_v)**2.0)) in

  if discriminant >= 0.0 then
    let eta_n_dot_i  = eta *. (dot normalized_v n) in
    let normal_component = (eta_n_dot_i +. (Float.sqrt discriminant)) *.| n in
    eta *.| normalized_v -| normal_component
  else
    (**total internal reflection*)
    let _ = print_endline "total internal reflection!" in 
    reflect normalized_v n    

let refract_ray old_ray hit_point normal new_ior = 
  let open Vector in
  let open Ray in
  let refract_direction = refract old_ray.direction normal old_ray.refractive_index new_ior in
  let total_internal_reflection = ((dot refract_direction normal) > 0.0) in
  let new_origin = hit_point +|
    if total_internal_reflection then Util.epsilon *.| normal else (-.Util.epsilon)*.|normal 
  in
  {
    origin = new_origin;
    direction = normalized refract_direction;
    refractive_index = if total_internal_reflection then old_ray.refractive_index else new_ior;
    inside = if total_internal_reflection then old_ray.inside else (not old_ray.inside);
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

