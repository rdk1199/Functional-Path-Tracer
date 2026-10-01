type color = {
  r : float;
  g : float;
  b : float;
}

type int_color = {
  r : int;
  g : int;
  b : int;
}

(** turn color to int_color for writing to file*)
let round_color (color : color) = {r = int_of_float(Float.round color.r); 
                         g = int_of_float(Float.round color.g);
                         b = int_of_float(Float.round color.b);
}

(** convert color to string - also used for writing .ppm images*)
let string_of_int_color color = string_of_int(color.r) ^ " " ^ 
                                string_of_int(color.g) ^ " " ^ 
                                string_of_int(color.b)

(** convert a vector3 to color *)
let color_of_vector3 v : color = {r = v.Vector.x; g = v.Vector.y; b = v.Vector.z}