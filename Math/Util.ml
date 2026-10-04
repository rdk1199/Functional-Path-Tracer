
let epsilon = 0.00001

(**clamp a float value to between low and high*)
let clamp value low high = Float.max low (Float.min high value) 

let deg_to_rad value = value *. (Float.pi /. 180.0)
let rad_to_deg value = value *. (180.0 /. Float.pi)

let min_of_three_floats a b c = Float.min (Float.min a b) c
let max_of_three_floats a b c = Float.max (Float.max a b) c