
let rec accumulate_lights_rec light_index surface_point (total_color : Math.Color.color) hit_record scene : Math.Color.color =
  if light_index >= List.length scene.World.Scene.lights then
    total_color
  else
  let open Math.Color in
  let open Math.Vector in
  let open Math.Ray in
  let open World.Light in
  let open World.Scene in
  let light = List.nth scene.lights light_index in
  let ray_to_light = {origin = surface_point; 
                      direction = light.position -| surface_point} in
  let distance_to_light = Math.Vector.magnitude ray_to_light.direction in
  let cos_angle_between = Math.Vector.cos_angle_between ray_to_light.direction hit_record.normal in
  if cos_angle_between < 0. then
    accumulate_lights_rec (light_index + 1) surface_point total_color hit_record scene
  else
    let to_light_hit_record = World.Scene.ray_intersects_scene ray_to_light scene in
    let blocked = to_light_hit_record.hit && (magnitude (to_light_hit_record.t *.| ray_to_light.direction)) < distance_to_light in
    if blocked then
      accumulate_lights_rec (light_index + 1) surface_point total_color hit_record scene
    else
      let adjusted_intensity = light.intensity /. (distance_to_light *. distance_to_light) in
      let color_delta = (cos_angle_between *. adjusted_intensity) |*.| 
                       (light.color |*| (List.nth scene.materials hit_record.material_index).base_color) in 
      let new_total = total_color |+| color_delta in
      accumulate_lights_rec (light_index + 1) surface_point new_total hit_record scene


let accumulate_lights hit_record scene =
  let open Math.Color in
  let open Math.Vector in
  let open World.Scene in
  (**push out the surface point a bit to avoid self intersection when checking for lights*)
  let surface_point = hit_record.point +| (Math.Util.epsilon *.| hit_record.normal) in
  let total_color = scene.World.Scene.ambient_color |*| (List.nth scene.materials hit_record.material_index).base_color in
  accumulate_lights_rec 0 surface_point total_color hit_record scene

let compute_pixel camera scene x y =
  let hit_record = World.Scene.ray_intersects_scene (Camera.get_camera_ray x y camera) scene in
    if hit_record.hit then
      accumulate_lights hit_record scene
    else
      scene.World.Scene.background_color;