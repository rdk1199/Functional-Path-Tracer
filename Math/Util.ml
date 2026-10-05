
let epsilon = 0.00001

(**clamp a float value to between low and high*)
let clamp value low high = Float.max low (Float.min high value) 

let deg_to_rad value = value *. (Float.pi /. 180.0)
let rad_to_deg value = value *. (180.0 /. Float.pi)

let min_of_three_floats a b c = Float.min (Float.min a b) c
let max_of_three_floats a b c = Float.max (Float.max a b) c

let rec to_binary_string value = 
  if value = 0 then "0"
  else if value = 1 then "1"
  else
    (to_binary_string (value lsr 1)) ^ (string_of_int (value land 1))

let rec vertical_string_of_int_list int_list = 
  match int_list with
  | [] -> ""
  | head :: tail -> string_of_int head ^ "\n" ^ (vertical_string_of_int_list tail)

let rec vertical_string_of_float_list float_list = 
  match float_list with
  | [] -> ""
  | head :: tail -> string_of_float head ^ "\n" ^ (vertical_string_of_float_list tail)