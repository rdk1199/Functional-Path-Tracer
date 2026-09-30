
type triangle = {
  p1 : Vector.vector3;
  p2 : Vector.vector3;
  p3 : Vector.vector3;
}

(** Axis aligned bounding box*)
type aabb = {
  x_min : float;
  x_max : float;
  y_min : float;
  y_max : float;
  z_min : float;
  z_max : float;
}

type sphere = {
  center : Vector.vector3;
  radius : float;
}