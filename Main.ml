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

let camera_pos = {x = 0.0; y = -5.0; z = 0.0;}
let camera_forward = {x = 0.0; y = 1.0; z = 0.0;}
let camera_up = {x = 0.0; y = 0.0; z = 1.0;}
let focal_length = 1.0
let fov = 90.0

let camera = create_camera camera_pos camera_forward camera_up width height focal_length fov

(**define spheres*)
let sphere_center = {x = 0.0; y = 4.0; z = 0.0;}
let sphere_radius = 1.0

let sphere_1 = {center = sphere_center; radius = sphere_radius}

let sphere_2_center = {x = 2.2; y = 4.0; z = 0.0;}
let sphere_2_radius = 1.0

let sphere_2 = {center = sphere_2_center; radius = sphere_2_radius}

let sphere_3_center = {x = -2.2; y = 4.0; z = 0.0;}
let sphere_3_radius = 1.0

let sphere_3 = {center = sphere_3_center; radius = sphere_3_radius;}

let ground = {center = {x = 0.0; y = 1.0; z = -2000.0}; radius = 1999.0}


let glass_ball = {center = {x = 0.0; y = 0.5; z = 0.0}; radius = 1.0;}

let spheres = [sphere_1; sphere_2; sphere_3; ground]

(**define materials*)
let red_material = {base_color = {r = 125.0; g = 0.0; b = 0.0;}; shininess = 0.5; refractive_index = 1.4; transparency = 0.0;}
let green_material = {base_color = {r = 0.0; g = 125.0; b = 0.0;}; shininess = 0.5; refractive_index = 1.4; transparency = 0.0;}
let blue_material = {base_color = {r = 0.0; g = 0.0; b = 125.0;}; shininess = 0.5; refractive_index = 1.4; transparency = 0.0;}
let grass_material = {base_color = {r = 25.0; g = 25.0; b = 25.0;}; shininess = 0.5; refractive_index = 1.4; transparency = 0.0;}
let glass_material = {base_color = {r = 0.0; g = 0.0; b = 0.0}; shininess = 0.0; refractive_index = 1.2; transparency = 1.0;}
let mirror_material = {base_color = Math.Color.white; shininess = 1.0; refractive_index = 1.2; transparency = 0.0;}
let materials = [red_material; green_material; blue_material; grass_material; glass_material; mirror_material]

let sphere_materials = [1; 0; 2; 3]

let light_1 = {
  color = {r = 1.0; g = 1.0; b=1.0};
  intensity = 2500000.0;
  position = {x = -500.0; y = -100.0; z = 1000.0;};
}

let mirror_corner_1 = {x = -4.0; y = -1.0; z = -1.0}
let mirror_corner_2 = {x = -4.0; y = -1.0; z = 2.0}
let mirror_corner_3 = {x = 4.0; y = -1.0; z = -1.0}
let mirror_corner_4 = {x = 4.0; y = -1.0; z = 2.0}



let mirror_tri_1 = {
  p1 = mirror_corner_1;
  p2 = mirror_corner_2;
  p3 = mirror_corner_3;
}

let mirror_tri_2 = {
  p1 = mirror_corner_2;
  p2 = mirror_corner_3;
  p3 = mirror_corner_4;
}

let (triangle_list, triangle_materials, material_list) = parse_obj "ModelFiles/Monkey.obj"
let () = print_endline "Parsed obj!"

let scene = {
  spheres = spheres;
  sphere_materials = sphere_materials;
  triangles = triangle_list;
  triangle_materials = triangle_materials;
  bvh = build_bvh triangle_list triangle_materials;
  lights = [light_1];
  materials = materials;
  background_color = {r = 135.0; g = 206.0; b = 235.0};
  ambient_color = {r = 0.1; g = 0.1; b = 0.1}
}

let () = print_endline "defined camera and scene"

let rec draw_scene_rec camera scene x y image =
  if y >= camera.res_y then
    (**done!*)
    ()
  else if x >= camera.res_x then
    (**finished this row - onto the next*)
    draw_scene_rec camera scene 0 (y+1) image
  else begin
    print_string ("\r" ^ (string_of_int x) ^ ", " ^ (string_of_int y));
    set_pixel image (RenderingLib.Render.compute_pixel camera scene x y 3) x y;
    draw_scene_rec camera scene (x+1) y image
  end


let draw_scene camera scene image =
  draw_scene_rec camera scene 0 0 image

let start_float = Unix.gettimeofday ()

(*let _  = draw_scene camera scene test_image*)

let pcg_32_gen = create_pcg_32_gen 568 1

let () = print_endline (Math.Util.vertical_string_of_float_list (get_all_bit_distributions (generate_random_stream pcg_32_gen 10000000) 31))

let duration = Unix.gettimeofday() -. start_float

let () = print_endline ("render time: " ^ (string_of_float duration) ^ " s")

let file_name = "test.ppm"
let _ = image_to_ppm test_image file_name

