open IO.Image
open IO.PPMWriter
open Math.Color
open RenderingLib.Camera
open Math.Vector
open Math.Ray
open Math.Shape
open Math.Intersection
open World.Scene
open World.Light
open World.Material

let _ =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info) (* Set global log level filter *)

let red : color = {r = 255.0; g = 0.0; b = 0.0}
let black : color = {r = 0.0; g = 0.0; b= 0.0}
let purple : color = {r = 255.0; g = 0.0; b = 255.0}

let width = 1600
let height = 900

let test_image = IO.Image.create_image width height red

let camera_pos = {x = 0.0; y = 0.0; z = 0.0;}
let camera_forward = {x = 0.0; y = 1.0; z = 0.0;}
let camera_up = {x = 0.0; y = 0.0; z = 1.0;}
let focal_length = 1.0
let fov = 90.0

let camera = create_camera camera_pos camera_forward camera_up width height focal_length fov

let () = print_endline (string_of_float (camera.screen_width))

let () = print_endline (string_of_vector3 ((get_camera_ray 0 0 camera).direction))

(**define spheres*)
let sphere_center = {x = 0.0; y = 4.0; z = 0.0;}
let sphere_radius = 1.0

let sphere_1 = {center = sphere_center; radius = sphere_radius}

let sphere_2_center = {x = 1.0; y = 5.5; z = 0.5;}
let sphere_2_radius = 0.6

let sphere_2 = {center = sphere_2_center; radius = sphere_2_radius}

let sphere_3_center = {x = -1.0; y = 7.0; z = -0.5;}
let sphere_3_radius = 1.25

let sphere_3 = {center = sphere_3_center; radius = sphere_3_radius;}
let spheres = [sphere_1; sphere_2; sphere_3]

(**define materials*)
let red_material = {base_color = {r = 255.0; g = 0.0; b = 0.0;}; shininess = 0.0;}
let green_material = {base_color = {r = 0.0; g = 0.0; b = 255.0;}; shininess = 0.0;}
let blue_material = {base_color = {r = 0.0; g = 255.0; b = 0.0;}; shininess = 0.0;}
let materials = [red_material; green_material; blue_material]

let sphere_materials = [1; 0; 2]

let scene = {
  spheres = spheres;
  sphere_materials = sphere_materials;
  lights = [];
  materials = materials;
}

let () = print_endline "defined camera and scene"

let rec draw_scene_rec camera scene x y image =
  if y >= camera.res_y then
    (**done!*)
    ()
  else if x >= camera.res_x then
    (**finished this row - onto the next*)
    draw_scene_rec camera scene 0 (y+1) image
  else
    let hit_record = ray_intersects_scene (get_camera_ray x y camera) scene in
    if hit_record.hit then
      set_pixel image ((List.nth materials hit_record.material_index).base_color) x y
    else
      set_pixel image black x y;
    draw_scene_rec camera scene (x+1) y image

let draw_scene camera scene image =
  draw_scene_rec camera scene 0 0 image


let _  = draw_scene camera scene test_image

let file_name = "test.ppm"
let _ = image_to_ppm test_image file_name