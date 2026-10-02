open Vector

type ray = {
  origin : vector3;
  direction : vector3;
  (**index of refraction of object we are currently in*)
  refractive_index : float;
  (**are we inside an object?*)
  inside : bool;
}

let string_of_ray ray = 
  "origin: " ^ (string_of_vector3 ray.origin) ^ "\n" ^
  "direction: " ^ (string_of_vector3 ray.direction) ^ "\n" ^
  "ior: " ^ (string_of_float ray.refractive_index) ^ "\n" ^
  "inside: " ^ (string_of_bool ray.inside) ^ "\n"