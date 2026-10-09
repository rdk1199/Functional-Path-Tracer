
let uniform_sample_triangle triangle gen =
  let open Vector in
  let open Shape in
  let (r1, gen2) = Random.rand_0_1_float gen in
  let (r2, gen3) = Random.rand_0_1_float gen2 in
  let c1 = Float.sqrt r1 in
  let c2 = r2 in
  let s, t = 
    if c1 +. c2 > 1.0 then
      (1.0 -. c1, 1.0 -. c2 )
    else
      (c1, c2)
  in
  let e1 = triangle.e1 in
  let e2 = triangle.e2 in
  (triangle.p1 +| (s *.| e1) +| (t *.| e2), gen3)
