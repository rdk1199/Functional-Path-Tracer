open Math.Vector

type camera = {
  position : vector3;
  forward : vector3;
  up : vector3;

  focal_length : float;

  (**image resolution in pixels*)
  px_width : int;
  px_height : int;
}