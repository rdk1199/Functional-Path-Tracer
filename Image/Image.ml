open Color

(**flattened 2D matrix of colors*)
type image = {
  width : int;
  height : int;
  data : color list;
}

(**convert pixel (x,y) to its index in the flattened image array*)
let get_flattened_pixel_index width x y =
  y * width + x

let get_pixel image x y =
  List.nth image.data (get_flattened_pixel_index image.width x y)

(**generate a list of all the same color pixels *)
(**builds the image row by row, from top to bottom*)
(**assume width and height fields are already set*)
let rec generate_uniform_image_list width height x y color list =
  if y >= height then
    (**we are done*)
    let _ = Logs.info(fun m -> m "finished image generation") in
    list 
  else if x >= width then
    (**end of current row - move to next*)
    generate_uniform_image_list width height 0 (y + 1) color list
  else 
    let _ = Logs.info(fun m -> m "setting pixel %d, %d" x y) in
    let color_list = [color] in
    let new_list = list @ color_list in
    generate_uniform_image_list width height (x + 1) y color new_list

(**generate an image of uniform color*)
let generate_uniform_image width height color =
  {width = width;
   height = height;
   data = generate_uniform_image_list width height 0 0 color []}


