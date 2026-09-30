type vector3 = {
    x : float;
    y : float;
    z : float;
}

(** Convert Vector3 to string*)
let string_of_vector3 v = string_of_float(v.x) ^ ", " ^ string_of_float(v.y) ^ ", " ^ string_of_float(v.z)
 
(** Add two vectors*)
let (+|) v1 v2 = {x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z}

(** Subtract vectors*)
let (-|) v1 v2 = {x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z}

(** multiply by scalar*)
let ( *.| ) c v = {x = c *. v.x; y = c *. v.y; z = c *. v.z}

let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let square_magnitude v = dot v v

let magnitude v = sqrt (square_magnitude v)

let cross_product v1 v2 = {x = v1.y *. v2.z -. v1.z *. v2.y;
                           y = v1.z *. v2.x -. v1.x *. v2.z;
                           z = v1.x *. v2.y -. v1.y *. v2.x}