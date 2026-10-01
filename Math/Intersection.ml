
let epsilon = 0.00001

type hit_info = {
  hit : bool;
  t : float;
  normal : Vector.vector3;
}

let ray_intersects_sphere ray sphere =
  let open Vector in
  let dir_sq_mag = square_magnitude ray.Ray.direction in
  let zero_vec = {x = 0.0; y = 0.0; z = 0.0} in
  if dir_sq_mag < epsilon then
    {hit = false; t = 0.; normal = zero_vec}
  else
    let r_squared = sphere.Shape.radius *. sphere.Shape.radius in
    let offset = ray.origin -| sphere.Shape.center in
    let offset_square_mag = square_magnitude offset in
    let dir_dot_offset = dot offset ray.direction in
    let discriminant = dir_dot_offset *. dir_dot_offset -. dir_sq_mag *. (offset_square_mag -. r_squared) in
    if discriminant < 0. then
      {hit = false; t = 0.; normal = zero_vec}
    else
      let t1 = ((-.dir_dot_offset) -. sqrt (discriminant)) /. dir_sq_mag in
      let t2 = ((-.dir_dot_offset) +. sqrt (discriminant)) /. dir_sq_mag in
      if t1 >= 0. then
        let hit_point = ray.Ray.origin +| (t1 *.| ray.Ray.direction) in
        {hit = true; t = t1; normal = (normalized (hit_point -| sphere.Shape.center))}
      else if t2 >= 0. then
        let hit_point = ray.Ray.origin +| (t1 *.| ray.Ray.direction) in
        {hit = true; t = t2; normal = (normalized (hit_point -| sphere.Shape.center))}
      else
        {hit = false; t = 0.; normal = zero_vec}


