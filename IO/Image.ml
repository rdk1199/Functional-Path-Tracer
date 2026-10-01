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

(**non-functional! set pixel (x,y) to value color*)
let set_pixel image color x y =
  image.data.(get_flattened_pixel_index image.width x y) <- color

let create_image width height color =
  {width = width; height = height; data = Array.make (width * height) color}
  let () = Logs.info (fun m-> m "Finished creating image\n")