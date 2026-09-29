
(**clamp a float value to between low and high*)
let clamp value low high = Float.max low (Float.min high value) 