type material = {
  name : string;
  base_color : Math.Color.color;
  shininess : float;
  refractive_index : float;
  opacity : float;
  emissive: Math.Color.color;
}

let default_material = {
  name = "";
  base_color = Math.Color.black;
  shininess = 0.0;
  refractive_index = 0.0;
  opacity = 0.0;
  emissive = Math.Color.black;
}

let convert_specular_exponent_to_shininess spec_exp =
  (sqrt spec_exp) /. (sqrt 1000.0)
