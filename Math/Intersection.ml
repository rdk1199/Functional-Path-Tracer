type hit_info = {
  hit : bool;
  t : float;
  point : Vector.vector3;
  normal : Vector.vector3;
}

let null_hit = {
  hit = false;
  t = 0.0;
  point = Vector.zero_vec;
  normal = Vector.zero_vec;
}

let ray_intersects_sphere ray sphere =
  let open Vector in
  let dir_sq_mag = square_magnitude ray.Ray.direction in
  if dir_sq_mag < Util.epsilon then
    null_hit
  else
    let r_squared = sphere.Shape.radius *. sphere.Shape.radius in
    let offset = ray.origin -| sphere.Shape.center in
    let offset_square_mag = square_magnitude offset in
    let dir_dot_offset = dot offset ray.direction in
    let discriminant = dir_dot_offset *. dir_dot_offset -. dir_sq_mag *. (offset_square_mag -. r_squared) in
    if discriminant < 0. then
      null_hit
    else
      let t1 = ((-.dir_dot_offset) -. sqrt (discriminant)) /. dir_sq_mag in
      let t2 = ((-.dir_dot_offset) +. sqrt (discriminant)) /. dir_sq_mag in
      if t1 >= 0. then
        let hit_point = ray.Ray.origin +| (t1 *.| ray.Ray.direction) in
        let outer_normal = normalized (hit_point -| sphere.Shape.center) in
        let hit_normal = if dot outer_normal ray.Ray.direction < 0. then outer_normal else -.1.0 *.| outer_normal in
        {hit = true; t = t1; point = hit_point; normal = hit_normal}
      else if t2 >= 0. then
        let hit_point = ray.Ray.origin +| (t2 *.| ray.Ray.direction) in
        let outer_normal = normalized (hit_point -| sphere.Shape.center) in
        let hit_normal = if dot outer_normal ray.Ray.direction < 0. then outer_normal else -.1.0 *.| outer_normal in
        {hit = true; t = t2; point = hit_point; normal = hit_normal}
      else
        null_hit

      
(**Moller-Trumbore algorithm for ray-triangle intersection*)
let ray_intersects_triangle ray triangle =
  let open Vector in
  let open Ray in
  let open Shape in
  let edge1 = triangle.p3 -| triangle.p1 in
  let edge2 = triangle.p2 -| triangle.p1 in
  let normal = cross_product edge1 edge2 in
  let signed_normal = if dot normal ray.direction > 0. then ~-|normal else normal in
  let ray_cross_edge2 = cross_product ray.direction edge2 in
  let det = dot edge1 ray_cross_edge2 in
  if (Float.abs det < Util.epsilon) then
    null_hit
  else
    let inv_det = 1.0 /. det in
    let s = ray.origin -| triangle.p1 in
    let u = inv_det *. (dot s ray_cross_edge2) in
    if (u < -.Util.epsilon || (u -. 1.0) > Util.epsilon) then
      null_hit
    else
      let s_cross_e1 = cross_product s edge1 in
      let v = inv_det *. dot ray.direction s_cross_e1 in
      if (v < -.Util.epsilon || u +. v -. 1.0 > Util.epsilon) then
        null_hit
      else
        let t = inv_det *. dot edge2 s_cross_e1 in
        if t > Util.epsilon then {
           hit = true;
           t = t; 
           point = ray.origin +| t *.| ray.direction;
           normal = signed_normal
        }
        else
          null_hit

(**TODO: check if divide by zero case actually matters*)
let ray_intersects_aabb ray aabb = 
  let open Ray in
  let open Vector in
  let open Shape in
  let inv_dir_x = 1.0 /. ray.direction.x in
  let inv_dir_y = 1.0 /. ray.direction.y in
  let inv_dir_z = 1.0 /. ray.direction.z in
  let t1 = inv_dir_x *. (aabb.min.x -. ray.origin.x) in
  let t2 = inv_dir_x *. (aabb.max.x -. ray.origin.x) in
  let t3 = inv_dir_y *. (aabb.min.y -. ray.origin.y) in
  let t4 = inv_dir_y *. (aabb.max.y -. ray.origin.y) in
  let t5 = inv_dir_z *. (aabb.min.z -. ray.origin.z) in
  let t6 = inv_dir_z *. (aabb.max.z -. ray.origin.z) in
  let t_min = Float.max (Float.min t5 t6) (Float.max (Float.min t1 t2) (Float.min t3 t4)) in
  let t_max = Float.min (Float.max t5 t6) (Float.min (Float.max t1 t2) (Float.max t3 t4)) in

  if t_max < 0.0 then
    null_hit
  else if t_min > t_max then
    null_hit
  else
    {
      hit = true;
      t = t_min;
      point = ray.origin +| t_min *.| ray.direction;
      (**TODO: we usually only use AABBs as bounding boxes for other geometry,
      meaning that the normal should not be necessary*)
      normal = zero_vec;
    }
