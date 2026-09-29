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

(**generate a list of all the same color pixels *)
(**builds the image row by row, from top to bottom*)
(**assume width and height fields are already set*)
let rec generate_uniform_image_list width height x y color list =
  if y >= height then
    (**we are done*)
    list 
  else if x >= width then
    (**end of current row - move to next*)
    generate_uniform_image_list width height 0 (y + 1) color list
  else 
    let color_list = [color] in
    let new_list = list @ color_list in
    generate_uniform_image_list width height (x + 1) y color new_list


