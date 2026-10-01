
let compute_pixel camera scene x y =
  let (black : Math.Color.color) = {Math.Color.r = 0.0; Math.Color.g = 0.0; Math.Color.b = 0.0} in
  let hit_record = World.Scene.ray_intersects_scene (Camera.get_camera_ray x y camera) scene in
    if hit_record.hit then
      ((List.nth scene.materials hit_record.material_index).base_color)
    else
      black;