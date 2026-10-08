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

let width = 600
let height = 600
let test_image = IO.Image.create_image width height red

let camera_pos = {x = 0.0; y = -2.0; z = 1.0;}
let camera_forward = {x = 0.0; y = 1.0; z = 0.0;}
let camera_up = {x = 0.0; y = 0.0; z = 1.0;}
let focal_length = 1.0
let fov = 90.0

let camera = create_camera camera_pos camera_forward camera_up width height focal_length fov

let (triangle_list, triangle_materials, material_name_list) = parse_obj "ModelFiles/CornellBox.obj"
let material_list = IO.MtlParser.parse_mtl_file "ModelFiles/CornellBox.mtl" material_name_list
let () = print_endline "Parsed obj and mtl!"


let scene = create_scene triangle_list triangle_materials (build_bvh triangle_list triangle_materials) material_list Math.Color.black
let () = print_endline "defined camera and scene"

let rec draw_scene_rec camera scene x y image r_gen accumulator num_samples avg_depth =
  if y >= camera.res_y then
    (**done!*)
    ()
  else if x >= camera.res_x then
    (**finished this row - onto the next*)
    draw_scene_rec camera scene 0 (y+1) image r_gen accumulator num_samples avg_depth
  else begin
    let accum_color, new_gen = RenderingLib.Render.accumulate_pixel camera scene x y avg_depth accumulator num_samples r_gen in
    set_pixel image accum_color x y;
    draw_scene_rec camera scene (x+1) y image new_gen accumulator num_samples avg_depth
  end


let draw_scene camera scene image accumulator num_samples avg_depth gen =
  draw_scene_rec camera scene 0 0 image gen accumulator num_samples avg_depth


let multithreaded_draw_scene camera scene accumulator samples_per_thread avg_depth =
  let seed_gen = Math.Random.create_pcg_32_gen 0 1 in
  let num_threads =  Domain.recommended_domain_count () - 1  in
  let seed_list = Math.Random.generate_random_stream seed_gen num_threads in
  print_endline (string_of_int num_threads);
  let image_list = List.init num_threads (fun i -> (create_image width height Math.Color.black)) in
  let make_draw_thread camera scene index accumulator num_samples avg_depth =
    let thread_gen = Math.Random.create_pcg_32_gen (List.nth seed_list index) 1 in
    Domain.spawn (fun () -> draw_scene camera scene (List.nth image_list index) accumulator samples_per_thread avg_depth thread_gen)
  in
  let thread_list = List.init num_threads (fun i -> 
    print_endline (string_of_int (List.nth seed_list i));
    (make_draw_thread camera scene i accumulator samples_per_thread avg_depth)
    ) in
  let _ = List.iter Domain.join thread_list in
  print_endline (string_of_int (List.length image_list));
  average_combine_images image_list
  


let start_float = Unix.gettimeofday ()

let final_image = multithreaded_draw_scene camera scene RenderingLib.Accumulator.cos_lambertian_accumulate 50 5

let duration = Unix.gettimeofday() -. start_float

let () = print_endline ("render time: " ^ (string_of_float duration) ^ " s")

let file_name = "st_cos_test.ppm"
let _ = image_to_ppm final_image file_name

