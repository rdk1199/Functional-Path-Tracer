
(**maps vector_up to normal*)
let tangent_to_world_space vector normal =
  let open Vector in
  let axis = 
    if Float.abs (dot normal vector_up) < 1.0 -. Util.epsilon then
      vector_up
    else
      vector_right
  in

  let tangent = normalized (axis -| (dot normal axis) *.| normal) in
  let bitangent = normalized (cross_product tangent normal) in
  (vector.x *.| tangent) +| (vector.y *.| bitangent) +| (vector.z *.| normal)
