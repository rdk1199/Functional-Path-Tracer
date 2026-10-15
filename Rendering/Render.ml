
(**Repeatedly sample a pixel with a given accumulator function, accumulating the result in color*)
let accumulate_pixel camera scene x y expected_depth accumulator num_samples gen = 
  let open Math.Color in
  let stop_prob = 1.0 /. (float_of_int expected_depth) in
  let rec accum_pixel_rec accum_color sample_index rec_gen=
    if sample_index >= num_samples then
      (accum_color, rec_gen)
    else
      let camera_ray = Camera.get_camera_ray x y camera in
      let sample_color, gen2 = accumulator camera_ray scene stop_prob rec_gen in
      let sample_weight = 1.0 /. (float_of_int num_samples) in
      let new_accum_color = accum_color |+| (sample_weight |*.| sample_color) in
      (accum_pixel_rec new_accum_color (sample_index + 1) gen2)
  in
  accum_pixel_rec Math.Color.black 0 gen





  