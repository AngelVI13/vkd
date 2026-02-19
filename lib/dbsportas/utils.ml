open Core

let calculate_percent (part : int) (whole : int) =
  assert (whole > 0);
  Float.(to_int (round_nearest 100.0 * (of_int part / of_int whole)))
