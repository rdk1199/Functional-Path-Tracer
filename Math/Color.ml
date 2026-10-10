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

let (black : color) = {r = 0.0; g =0.0; b = 0.0}

let (white : color) = {r = 255.0; g = 255.0; b=255.0}

let (ones : color ) = {r = 1.0; g = 1.0; b=1.0}

let (max_color : color) = {r = Float.max_float; g = Float.max_float; b = Float.max_float}

(**add colors*)
let (|+|) (c1:color) (c2:color) : color = {r = c1.r +. c2.r; g = c1.g +. c2.g; b = c1.b +. c2.b}

(**subtract colors*)
let (|-|) (c1:color) (c2:color) : color = {r = c1.r -. c2.r; g = c1.g -. c2.g; b = c1.b -. c2.b}

(**component-wise multiply colors*)
let (|*|) (c1:color) (c2:color) : color = {r = c1.r *. c2.r; g = c1.g *. c2.g; b = c1.b *. c2.b}

(**multiply color by scalar*)
let (|*.|) (k:float) (c:color) : color = {r = k *. c.r; g = k *. c.g; b = k *. c.b}

let color_sq_magnitude (c:color) = c.r *. c.r +. c.g *. c.g +. c.b *. c.b

let clamp_color (color : color) : color = {
  r = Util.clamp color.r 0.0 255.0;
  g = Util.clamp color.g 0.0 255.0;
  b = Util.clamp color.b 0.0 255.0;
}

let string_of_color (color : color) =
  string_of_float(color.r) ^ " " ^ string_of_float(color.g) ^ " " ^ string_of_float(color.b) 

(** turn color to int_color for writing to file*)
let round_color (color : color) = 
  let clamped_color = clamp_color color in
  {
    r = int_of_float(Float.round clamped_color.r); 
    g = int_of_float(Float.round clamped_color.g);
    b = int_of_float(Float.round clamped_color.b);
  } 

(** convert color to string - also used for writing .ppm images*)
let string_of_int_color color = string_of_int(color.r) ^ " " ^ 
                                string_of_int(color.g) ^ " " ^ 
                                string_of_int(color.b)

(** convert a vector3 to color *)
let color_of_vector3 v : color = {r = v.Vector.x; g = v.Vector.y; b = v.Vector.z}