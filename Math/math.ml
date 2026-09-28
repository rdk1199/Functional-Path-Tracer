open Vector

let v1 = {x = 1.0; y = -1.0; z = 2.0}
let v2 = {x = 1.0; y = 1.0; z = -10.0}
let _ = print_endline (string_of_vector3 (v1 +| v2))
let _ = print_endline (string_of_vector3 (v1 -| v2))
let _ = print_endline (string_of_float (dot v1 v2))
let _ = print_endline (string_of_float (square_magnitude v1))
let _ = print_endline (string_of_float (magnitude v1))