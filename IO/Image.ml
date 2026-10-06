open Math.Color

(**flattened 2D matrix of colors*)
type image = {
  width : int;
  height : int;
  data : color array;
}

(**convert pixel (x,y) to its index in the flattened image array*)
let get_flattened_pixel_index width x y =
  y * width + x

let get_pixel image x y =
  image.data.(get_flattened_pixel_index image.width x y)

(**non-functional! mutating state! set pixel (x,y) to value color*)
(** TODO: this probably isn't even necessary as we could use a list *)
let set_pixel image color x y =
  image.data.(get_flattened_pixel_index image.width x y) <- (color)

let create_image width height color =
  {width = width; height = height; data = Array.make (width * height) color}
  let () = Logs.info (fun m-> m "Finished creating image\n")

(**non-functional!*)
let average_combine_images image_list = 
  let () = assert ((List.length image_list) > 0) in
  let multiplier = 1.0 /. (float_of_int (List.length image_list)) in 
  print_endline (string_of_float multiplier);
  let combined_image = create_image (List.hd image_list).width (List.hd image_list).height Math.Color.black in
  let rec average_combine_images_rec x y image_list = 
    let rec combine_pixel x y image_list accum_color =
      match image_list with
      | [] -> accum_color
      | head :: tail -> combine_pixel x y tail (accum_color |+| (multiplier |*.| (get_pixel head x y)))
    in
  if y >= combined_image.height then
    combined_image
  else if x >= combined_image.width then
    average_combine_images_rec 0 (y+1) image_list
  else
    let () = set_pixel combined_image (combine_pixel x y image_list Math.Color.black) x y in
    average_combine_images_rec (x+1) y image_list
  in
average_combine_images_rec 0 0 image_list
  
