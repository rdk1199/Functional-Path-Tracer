let write_file filename content = 
  Out_channel.with_open_text filename (fun oc -> Out_channel.output_string oc content)

(**create the ppm file format header string*)
let ppm_header (image : ImageLib.Image.image) =
  "P3\n" ^ 
  string_of_int(image.width) ^ " " ^ string_of_int(image.height) ^ "\n" ^
  "255\n"

let rec string_of_image_rec image_string x y (image : ImageLib.Image.image) =
  let open ImageLib.Color in
  let open ImageLib.Image in
  if y >= image.height then
    (**done*)
    image_string
  else if x >= image.height then
    (**done with current row - onto the next*)
    string_of_image_rec (image_string ^ "\n") 0 (y+1) image
  else
    string_of_image_rec (image_string ^ (string_of_int_color (round_color (get_pixel image x y))) ^ " ") (x + 1) y image

let string_of_image image =
  string_of_image_rec "" 0 0 image

let image_to_ppm image filename =
  write_file filename ((ppm_header image) ^ (string_of_image image))