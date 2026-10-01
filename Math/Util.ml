
let epsilon = 0.00001

(**clamp a float value to between low and high*)
let clamp value low high = Float.max low (Float.min high value) 

let deg_to_rad value = value *. (Float.pi /. 180.0)
let rad_to_deg value = value *. (180.0 /. Float.pi)