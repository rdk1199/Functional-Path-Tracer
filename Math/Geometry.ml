
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