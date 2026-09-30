open IO.PPMWriter
open RenderingLib.Color

let _ =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info) (* Set global log level filter *)

let purple : color = {r = 255.0; g = 0.0; b=255.0}
let test_image = IO.Image.create_image 1600 1600 purple

let file_name = "test.ppm"
let _ = image_to_ppm test_image file_name