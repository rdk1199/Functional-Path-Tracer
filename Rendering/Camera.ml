(**assumption is that forward and up are unit vectors*)
type camera = {
  position : Math.Vector.vector3;
  forward : Math.Vector.vector3;
  up : Math.Vector.vector3;
  right : Math.Vector.vector3;

  focal_length : float;
  x_fov : float;

  (**image resolution in pixels*)
  res_x : int;
  res_y : int;

  (**physical width and height of the screen plane*)
  screen_width : float;
  screen_height : float;
}

(**camera constructor that fills in all the necessary values*)
let create_camera position forward up res_x res_y focal_length x_fov =
  let normalized_forward = Math.Vector.normalized forward in
  let normalized_up = Math.Vector.normalized up in
  let aspect_ratio = (float_of_int res_x) /. (float_of_int res_y) in
  let screen_width = (2. *. focal_length) /. (tan (Math.Util.deg_to_rad x_fov /. 2.)) in 
  let screen_height = screen_width /. aspect_ratio in   
  {
   position = position;
   forward = normalized_forward;
   up = normalized_up;
   right = Math.Vector.cross_product normalized_forward normalized_up;
   focal_length = focal_length;
   x_fov = x_fov;
   res_x = res_x;
   res_y = res_y;
   screen_width = screen_width;
   screen_height = screen_height;
  }

(**get position of center of pixel x y on the screen*)
let get_screen_point x y camera = 
  let open Math.Vector in
  let screen_center = camera.position +| (camera.focal_length *.| camera.forward) in
  let px_width = camera.screen_width /. (float_of_int camera.res_x) in
  let px_height = camera.screen_height /. (float_of_int camera.res_y) in
  let to_left_edge = (-.camera.screen_width /. 2.) *.| camera.right in
  let to_top_edge = (camera.screen_height /. 2.) *.| camera.up in
  let x_as_float = float_of_int x in
  let y_as_float = float_of_int y in
  let x_shift_from_left = ((x_as_float +. 0.5) *. px_width) *.| camera.right in
  let x_shift = to_left_edge +| x_shift_from_left in
  let y_shift_from_top = (-.(y_as_float +. 0.5) *. px_height) *.| camera.up in
  let y_shift = to_top_edge +| y_shift_from_top in
  screen_center +| x_shift +| y_shift

(**get ray from focal point to pixel x,y on screen*)
let get_camera_ray x y camera =
  let open Math.Vector in
  let open Math.Ray in {
    origin = camera.position;
    direction = (get_screen_point x y camera) -| camera.position;
    refractive_index = 1.0;
    inside = false;
  }
