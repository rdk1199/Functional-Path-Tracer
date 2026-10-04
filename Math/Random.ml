(**Functional implementation of PCG32 pseudorandom number generation algorithm*)

(**type to hold the fixed state for a pcg_32 generator, which is just the shift*)
(**hide all the annoying int64/int32 logic in here so users can just use int*)
type pcg_32_gen_state = {
  seed : int64;
  shift : int;
}

let lcg_rand seed shift =
  let multiplier = 0x5851F42D4C957F2DL in
  (Int64.add  (Int64.mul multiplier seed) (Int64.of_int shift))

(**returns random 32 bit int (positive)*)
let pcg_32_rand gen  = 
  (**adapted from here: https://www.pcg-random.org/download.html*)
  let new_seed = lcg_rand gen.seed (gen.shift lor 1) in
  let xor_shifted = Int64.to_int32(Int64.shift_right_logical (Int64.logxor (Int64.shift_right_logical gen.seed 18) gen.seed) 27) in
  let rot = Int64.to_int (Int64.shift_right_logical gen.seed 59) in (**5 bits*)
  let out = Int32.to_int (
    Int32.logor 
    (Int32.shift_right_logical xor_shifted rot) 
    (Int32.shift_left xor_shifted ((-rot) land 31))
  )
  in
  let new_pcg_32_gen_state = {
    seed = new_seed;
    shift = gen.shift;
  }
  in
  (out, new_pcg_32_gen_state)

(**do initial seeding procedure and return a pcg_32_gen_state*)
let create_pcg_32_gen seed shift = 
  let seed64 = Int64.of_int seed in
  let odd_shift = shift lor 1 in
  let init_gen =   { seed = 0L; shift = odd_shift} in
  let (out, gen2) = pcg_32_rand init_gen in
  let seed2 = Int64.add gen2.seed seed64 in
  let gen3 = {seed = seed2; shift = odd_shift} in
  let (out2, gen4) = pcg_32_rand gen3 in
  gen4

let rand_max = ((1 lsl 32) - 1)

let rand_0_1_float gen = 
  let (rand_int, new_gen) = pcg_32_rand gen in
  let out = (float_of_int rand_int) /. (float_of_int rand_max) in
  (new_gen, out)

let rand_float_in_range low high gen =
  let (new_gen, rand_float) = rand_0_1_float gen in
  let out = low +. (high -. low) *. rand_float in
  (new_gen, out)

(**return list of randomly generated ints*)
let rec generate_random_stream gen length = 
  if length = 0 then
    []
  else
    let (out, new_gen) = pcg_32_rand gen in
    out :: (generate_random_stream new_gen (length - 1))

(**DEBUG: return proportion of numbers in int_list for which n'th bit is 0*)
(**n increases as we go from least -> most significant bit*)
let get_distribution_of_nth_bit int_list n =
  let rec get_distribution_of_nth_bit_rec int_list n zero_count one_count =
    match int_list with
    | [] -> (zero_count, one_count)
    | num :: tail ->
      if (1 lsl n) land num <> 0 then
        get_distribution_of_nth_bit_rec tail n zero_count (one_count + 1)
      else
        get_distribution_of_nth_bit_rec tail n (zero_count + 1) one_count
  in
  let (zero_count, one_count) = get_distribution_of_nth_bit_rec int_list n 0 0 in
  let total = float_of_int (zero_count + one_count) in
  (float_of_int zero_count) /. total

(**DEBUG: get all bit distributions up to max - list is ordered lsb -> msb*)
(**TODO: this implementation is dumb - easier to go msb -> lsb*)
let get_all_bit_distributions int_list max =
  let rec get_all_bit_distributions_rec int_list n dist_list max =
    if n > max then
      dist_list
    else
      let nth_bit_dist = get_distribution_of_nth_bit int_list n in
      let new_list = nth_bit_dist :: dist_list in
      get_all_bit_distributions_rec int_list (n+1) new_list max
  in
  get_all_bit_distributions_rec int_list 0 [] max



