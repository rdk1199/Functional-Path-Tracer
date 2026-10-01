
(** reflect vector v at surface with UNIT normal n*)

let reflect v n = 
  let open Vector in
  v -| 2. *. (dot v n) *.| n