
let rec accumulate_lights_rec surface_point (total_color : Math.Color.color) hit_record scene light_list: Math.Color.color =
  match light_list with
  | [] -> total_color
  | light::tail ->
  let open Math.Color in
  let open Math.Vector in
  let open Math.Ray in
  let open World.Light in
  let open World.Scene in
  let ray_to_light = {origin = surface_point; 
                      direction = light.position -| surface_point;
                      refractive_index = 1.0;
                      inside = false} in
  let distance_to_light = Math.Vector.magnitude ray_to_light.direction in
  let cos_angle_between = Math.Vector.cos_angle_between ray_to_light.direction hit_record.normal in
  if cos_angle_between < 0. then
    accumulate_lights_rec surface_point total_color hit_record scene tail
  else
    let to_light_hit_record = World.Scene.ray_intersects_scene ray_to_light scene in
    let blocked = to_light_hit_record.hit && (magnitude (to_light_hit_record.t *.| ray_to_light.direction)) < distance_to_light in
    if blocked then
      accumulate_lights_rec surface_point total_color hit_record scene tail
    else
      let adjusted_intensity = light.intensity /. (distance_to_light *. distance_to_light) in
      let color_delta = (cos_angle_between *. adjusted_intensity) |*.| 
                       (light.color |*| (List.nth scene.materials hit_record.material_index).base_color) in 
      let new_total = total_color |+| color_delta in
      accumulate_lights_rec surface_point new_total hit_record scene tail


let accumulate_lights hit_record scene =
  let open Math.Color in
  let open Math.Vector in
  let open World.Scene in
  (**push out the surface point a bit to avoid self intersection when checking for lights*)
  let surface_point = hit_record.point +| (Math.Util.epsilon *.| hit_record.normal) in
  let total_color = scene.World.Scene.ambient_color |*| (List.nth scene.materials hit_record.material_index).base_color in
  accumulate_lights_rec surface_point total_color hit_record scene scene.lights

let rec compute_ray_color ray scene color depth = 
  (*let () = print_endline (Math.Ray.string_of_ray ray) in*)
  if depth < 0 then
    color
  else
    let hit_record = World.Scene.ray_intersects_scene ray scene in
      if hit_record.hit then
        let material = List.nth scene.materials hit_record.material_index in
        let surface_color = Math.Color.(color |+| (accumulate_lights hit_record scene)) in

        let reflect_color = 
          if material.shininess > 0.0 then
            (**offset hit point by normal*)
            let reflected = Math.Geometry.reflect ray.direction hit_record.normal in
            let reflect_ray = {Math.Ray.origin = Math.Vector.(hit_record.point +| Math.Util.epsilon *.| hit_record.normal);
                               Math.Ray.direction = reflected;
                               Math.Ray.refractive_index = ray.Math.Ray.refractive_index;
                               Math.Ray.inside = ray.Math.Ray.inside;
                              }
            in
            let open Math.Color in
            (*let () = print_endline ("reflecting, normal: " ^ (Math.Vector.string_of_vector3 hit_record.normal)) in*)
            (material.shininess |*.| (compute_ray_color reflect_ray scene Math.Color.black (depth-1)))
          else
            Math.Color.black
        in

        let refract_color =
          if material.transparency > Math.Util.epsilon then
            let _ = print_endline ("transparency: " ^ (string_of_float material.transparency) ^ "\n") in
            let new_refractive_index = if ray.Math.Ray.inside then 1.0 else material.refractive_index in
            let refracted = Math.Geometry.refract ray.direction hit_record.normal ray.refractive_index new_refractive_index in
            let total_internal_reflection = (Math.Vector.dot refracted hit_record.normal > 0.0) in
            let refract_ray = {
              Math.Ray.origin = if total_internal_reflection then
                                  Math.Vector.(hit_record.point +| Math.Util.epsilon *.| hit_record.normal)
                                else
                                  Math.Vector.(hit_record.point -| Math.Util.epsilon *.| hit_record.normal);
              Math.Ray.direction = refracted;
              Math.Ray.refractive_index = if ray.Math.Ray.inside then
                                            if total_internal_reflection then 
                                              ray.refractive_index 
                                            else 
                                              1.0
                                          else
                                            material.refractive_index;
              Math.Ray.inside = if ray.Math.Ray.inside then
                                  if total_internal_reflection then
                                    true
                                  else
                                    false
                                else
                                  true;
            }
            in
            let () = print_endline "refracting\n" in
            Math.Color.(material.transparency |*.| (compute_ray_color refract_ray scene Math.Color.black (depth-1)));
          else
            Math.Color.black
        in
        Math.Color.(surface_color |+| reflect_color |+| refract_color)
      else
        color

let compute_pixel camera scene x y depth = 
  compute_ray_color (Camera.get_camera_ray x y camera) scene Math.Color.black depth
  