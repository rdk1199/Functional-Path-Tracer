type material = {
  name : string;
  base_color : Math.Color.color;
  roughness : float;
  refractive_index : float;
  metallic : float;
  opacity : float;
  emissive: Math.Color.color;
  fresnel: Math.Color.color;
}

let default_material = {
  name = "";
  base_color = Math.Color.black;
  roughness = 1.0;
  refractive_index = 0.0;
  opacity = 0.0;
  metallic = 0.0;
  emissive = Math.Color.black;
  fresnel = Math.Color.(0.04 |*.| Math.Color.ones); (**temporary value*)
}

let compute_fresnel metallic base_color = 
  let open Math.Color in
  let fresnel = (1.0 -. metallic) |*.| (0.04 |*.| Math.Color.ones) |+| (metallic |*.| base_color) in
  print_endline (Math.Color.string_of_color fresnel);
  fresnel
