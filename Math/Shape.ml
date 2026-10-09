
type triangle = {
  p1 : Vector.vector3;
  p2 : Vector.vector3;
  p3 : Vector.vector3;

  (**first edge (p2 - p1)*)
  e1 : Vector.vector3;
  (**second edge (p3 - p1)*)
  e2 : Vector.vector3;

  normal : Vector.vector3;

  area : float;
}

let create_triangle p1 p2 p3 = 
  let e1 = Vector.(p2 -| p1) in
  let e2 = Vector.(p3 -| p1) in
  let cross = Vector.cross_product e1 e2 in
  {
    p1 = p1;
    p2 = p2;
    p3 = p3;
    e1 = e1;
    e2 = e2;
    normal = Vector.normalized (cross);
    area = 0.5 *. (Vector.magnitude cross);
 }
  

(** Axis aligned bounding box*)
type aabb = {
  min : Vector.vector3;
  max : Vector.vector3;
}

type sphere = {
  center : Vector.vector3;
  radius : float;
}

let string_of_triangle triangle =
  Vector.string_of_vector3 triangle.p1 ^ "\n" ^ 
  Vector.string_of_vector3 triangle.p2 ^ "\n" ^ 
  Vector.string_of_vector3 triangle.p3

let centroid_of_aabb aabb = Vector.(0.5 *.| (aabb.max +| aabb.min))

let aabb_surface_area aabb = 
  let open Vector in
  let diff = aabb.max -| aabb.min in
  2.0 *. (diff.x *. diff.y +. diff.x *. diff.z +. diff.y *. diff.z)

let aabb_of_triangle triangle =
  let open Vector in
  let p1, p2, p3 = triangle.p1, triangle.p2, triangle.p3 in
  let min_x = Util.min_of_three_floats p1.x p2.x p3.x in
  let min_y = Util.min_of_three_floats p1.y p2.y p3.y in
  let min_z = Util.min_of_three_floats p1.z p2.z p3.z in
  let max_x = Util.max_of_three_floats p1.x p2.x p3.x in
  let max_y = Util.max_of_three_floats p1.y p2.y p3.y in
  let max_z = Util.max_of_three_floats p1.z p2.z p3.z in
  {
    min = {x = min_x; y = min_y; z = min_z};
    max = {x = max_x; y = max_y; z = max_z};
  }

let combine_aabbs aabb_1 aabb_2 = {
    min = Vector.vector3_min aabb_1.min aabb_2.min;
    max = Vector.vector3_max aabb_1.max aabb_2.max; 
}

(**return an aabb encompassing both the aabb and triangle provided*)
let combine_aabb_triangle aabb triangle =
  combine_aabbs aabb (aabb_of_triangle triangle)
    