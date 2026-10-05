open IO.Image
open IO.PPMWriter
open IO.ObjParser
open Math.Color
open Math.Random
open RenderingLib.Camera
open Math.Vector
open Math.BoundingVolumeHierarchy
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

let width = 900
let height = 900
let test_image = IO.Image.create_image width height red

let camera_pos = {x = 0.0; y = -3.0; z = 1.0;}
let camera_forward = {x = 0.0; y = 1.0; z = 0.0;}
let camera_up = {x = 0.0; y = 0.0; z = 1.0;}
let focal_length = 1.0
let fov = 90.0

let camera = create_camera camera_pos camera_forward camera_up width height focal_length fov

let light_1 = {
  color = {r = 1.0; g = 1.0; b=1.0};
  intensity = 10.0;
  position = {x = 0.0; y = -1.0; z = 2.0;};
}

let (triangle_list, triangle_materials, material_name_list) = parse_obj "ModelFiles/CornellBox.obj"
let material_list = IO.MtlParser.parse_mtl_file "ModelFiles/CornellBox.mtl" material_name_list
let () = print_endline "Parsed obj and mtl!"


let scene = {
  spheres = [];
  sphere_materials = [];
  triangles = triangle_list;
  triangle_materials = triangle_materials;
  bvh = build_bvh triangle_list triangle_materials;
  lights = [light_1];
  materials = material_list;
  background_color = {r = 0.0; g = 0.0; b = 0.0};
  ambient_color = {r = 0.1; g = 0.1; b = 0.1}
}

let () = print_endline "defined camera and scene"

let rec draw_scene_rec camera scene x y image r_gen =
  if y >= camera.res_y then
    (**done!*)
    ()
  else if x >= camera.res_x then
    (**finished this row - onto the next*)
    draw_scene_rec camera scene 0 (y+1) image r_gen
  else begin
    print_string ("\r" ^ (string_of_int x) ^ ", " ^ (string_of_int y));
    let accum_color, new_gen = RenderingLib.Render.accumulate_pixel camera scene x y 5 RenderingLib.Accumulator.simple_lambertian_accumulate 400 r_gen in
    set_pixel image accum_color x y;
    draw_scene_rec camera scene (x+1) y image new_gen
  end


let draw_scene camera scene image =
  draw_scene_rec camera scene 0 0 image

let start_float = Unix.gettimeofday ()


let gen = Math.Random.create_pcg_32_gen 0 1

let _  = draw_scene camera scene test_image gen

let duration = Unix.gettimeofday() -. start_float

let () = print_endline ("render time: " ^ (string_of_float duration) ^ " s")

let file_name = "test.ppm"
let _ = image_to_ppm test_image file_name

