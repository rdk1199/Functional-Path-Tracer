open IO.Image
open IO.PPMWriter
open RenderingLib.Color
open RenderingLib.Camera
open Math.Vector
open Math.Shape
open Math.Intersection

let _ =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info) (* Set global log level filter *)

let red : color = {r = 255.0; g = 0.0; b = 0.0}
let black : color = {r = 0.0; g = 0.0; b= 0.0}
let purple : color = {r = 255.0; g = 255.0; b = 255.0}

let width = 1600
let height = 900

let test_image = IO.Image.create_image width height red

let camera_pos = {x = 0.0; y = 0.0; z = 0.0;}
let camera_forward = {x = 0.0; y = 1.0; z = 0.0;}
let camera_up = {x = 0.0; y = 0.0; z = 1.0;}
let focal_length = 0.5
let fov = 90.0

let camera = create_camera camera_pos camera_forward camera_up width height focal_length fov

let sphere_center = {x = 0.0; y = 4.0; z = 0.0;}
let sphere_radius = 1.0

let sphere = {center = sphere_center; radius = sphere_radius}

let () = Logs.info (fun m -> m "defined camera and sphere")

let rec draw_sphere_rec camera sphere x y image =
  if y >= camera.res_y then
    (**done!*)
    ()
  else if x >= camera.res_x then
    (**finished this row - onto the next*)
    draw_sphere_rec camera sphere 0 (y+1) image
  else
    let hit_record = ray_intersects_sphere (get_camera_ray x y camera) sphere in
    if hit_record.hit then
      set_pixel image purple x y
    else
      set_pixel image black x y;
    draw_sphere_rec camera sphere (x+1) y image

let draw_sphere camera sphere image =
  draw_sphere_rec camera sphere 0 0 image


let _  = draw_sphere camera sphere test_image

let file_name = "test.ppm"
let _ = image_to_ppm test_image file_name