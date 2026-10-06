let append_to_file out_channel content = 
  output_string out_channel content

(**create the ppm file format header string*)
let ppm_header (image : Image.image) =
  "P3\n" ^ 
  string_of_int(image.width) ^ " " ^ string_of_int(image.height) ^ "\n" ^
  "255\n"

let rec image_to_ppm_rec x y (image : Image.image) out_channel =
  let open Math.Color in
  let open Image in
  if y >= image.height then
    (**done*)
    ()
  else if x >= image.width then
    (**done with current row - onto the next*)
    image_to_ppm_rec 0 (y+1) image out_channel
  else begin
    append_to_file out_channel ((string_of_int_color (round_color (255.0 |*.| (get_pixel image x y)))) ^ " ");
    image_to_ppm_rec (x+1) y image out_channel
  end

let image_to_ppm image filename =
  Out_channel.with_open_text filename (fun oc -> 
  Logs.info (fun m -> m "created image file!");
  append_to_file oc (ppm_header image);
  image_to_ppm_rec 0 0 image oc;
  flush oc;
  close_out oc)
  