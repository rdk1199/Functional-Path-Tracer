
type triangle = {
  p1 : Vector.vector3;
  p2 : Vector.vector3;
  p3 : Vector.vector3;
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
